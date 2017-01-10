#!/usr/bin/env perl

#
# Markdown -- A text-to-HTML conversion tool for web writers
#
# Copyright (C) 2004 John Gruber
# Copyright (C) 2015,2016,2017 Kyle J. McKay
# All rights reserved.
# License is Modified BSD (aka 3-clause BSD) License\n";
# See LICENSE file (or <https://opensource.org/licenses/BSD-3-Clause>)
#

package Markdown;

require 5.006_000;
use strict;
use warnings;

use vars qw($COPYRIGHT $VERSION @ISA @EXPORT_OK);

BEGIN {*COPYRIGHT =
\"Copyright (C) 2004 John Gruber
Copyright (C) 2015,2016,2017 Kyle J. McKay
All rights reserved.
";
*VERSION = \"1.0.4+" # Sun 05 Jun 2016+
}

require Exporter;
use Digest::MD5 qw(md5 md5_hex);
use File::Basename qw(basename);
use Scalar::Util qw(refaddr looks_like_number);
use Pod::Usage;
@ISA = qw(Exporter);
@EXPORT_OK = qw(Markdown);
$INC{__PACKAGE__.'.pm'} = $INC{basename(__FILE__)} unless exists $INC{__PACKAGE__.'.pm'};

close(DATA) if fileno(DATA);
exit(&_main(@ARGV)||0) unless caller;

## Disabled; causes problems under Perl 5.6.1:
# use utf8;
# binmode( STDOUT, ":utf8" );  # c.f.: http://acis.openlib.org/dev/perl-unicode-struggle.html


#
# Global default settings:
#
my ($g_style_prefix, $g_empty_element_suffix, $g_indent_width, $g_tab_width);
BEGIN {
    $g_style_prefix = "_markdown-";	# Prefix for markdown css class styles
    $g_empty_element_suffix = " />";	# Change to ">" for HTML output
    $g_indent_width = 4;		# Number of spaces considered new level
    $g_tab_width = 4;			# Legacy even though it's wrong
}


#
# Globals:
#

# Style sheet template
my $g_style_sheet;

# Permanent block id table
my %g_perm_block_ids;

# Global hashes, used by various utility routines
my %g_urls;
my %g_titles;
my %g_anchors;
my %g_block_ids;
my %g_html_blocks;
my %g_code_blocks;
my %opt;

# Return a "block id" to use to identify the block that does not contain
# any characters that could be misinterpreted by the rest of the code
# Originally this used md5_hex but that's unnecessarily slow
# Instead just use the refaddr of the scalar ref of the entry for that
# key in either the global or, if the optional second argument is true,
# permanent table.  To avoid the result being confused with anything
# else, it's prefixed with a control character and suffixed with another
# both of which are not allowed by the XML standard or Unicode.
sub block_id {
    $_[1] ?
    "\2".refaddr(\$g_perm_block_ids{$_[0]})."\3" :
    "\5".refaddr(\$g_block_ids{$_[0]})."\6";
}

# Regex to match balanced [brackets]. See Friedl's
# "Mastering Regular Expressions", 2nd Ed., pp. 328-331.
my $g_nested_brackets;
BEGIN {
    $g_nested_brackets = qr{
    (?>					# Atomic matching
	[^\[\]]+			# Anything other than brackets
     |
	\[
	    (??{ $g_nested_brackets })	# Recursive set of nested brackets
	\]
    )*
    }ox
}


# Table of hash values for escaped characters:
my %g_escape_table;
BEGIN {
    foreach my $char (split //, "\\\`*_~{}[]()>#+-.!") {
	$g_escape_table{$char} = block_id($char,1);
    }
}

# Used to track when we're inside an ordered or unordered list
# (see _ProcessListItems() for details):
my $g_list_level;
BEGIN {
    $g_list_level = 0;
}


#### Blosxom plug-in interface ##########################################
my $_haveBX;
BEGIN {
    no warnings 'once';
    $_haveBX = defined($blosxom::version);
}

# Set $g_blosxom_use_meta to 1 to use Blosxom's meta plug-in to determine
# which posts Markdown should process, using a "meta-markup: markdown"
# header. If it's set to 0 (the default), Markdown will process all
# entries.
my $g_blosxom_use_meta;
BEGIN {
    $g_blosxom_use_meta = 0;
}

sub start { 1; }
sub story {
    my($pkg, $path, $filename, $story_ref, $title_ref, $body_ref) = @_;

    if ((! $g_blosxom_use_meta) or
	(defined($meta::markup) and ($meta::markup =~ /^\s*markdown\s*$/i))
	 ) {
	    $$body_ref = Markdown($$body_ref);
    }
    1;
}


#### Movable Type plug-in interface #####################################
my $_haveMT = eval {require MT; 1;}; # Test to see if we're running in MT
my $_haveMT3 = $_haveMT && eval {require MT::Plugin; 1;}; # and MT >= MT 3.0.

unless ($_haveMT) {
    require MT;
    import  MT;
    require MT::Template::Context;
    import  MT::Template::Context;

    unless ($_haveMT3) {
	require MT::Plugin;
	import  MT::Plugin;
	my $plugin = new MT::Plugin({
	    name => "Markdown",
	    description => "A plain-text-to-HTML formatting plugin. (Version: $VERSION)",
	    doc_link => 'http://daringfireball.net/projects/markdown/'
	});
	MT->add_plugin( $plugin );
    }

    MT::Template::Context->add_container_tag(MarkdownOptions => sub {
	my $ctx  = shift;
	my $args = shift;
	my $builder = $ctx->stash('builder');
	my $tokens = $ctx->stash('tokens');

	if (defined ($args->{'output'}) ) {
	    $ctx->stash('markdown_output', lc $args->{'output'});
	}

	defined (my $str = $builder->build($ctx, $tokens) )
	    or return $ctx->error($builder->errstr);
	$str; # return value
    });

    MT->add_text_filter('markdown' => {
	label     => 'Markdown',
	docs      => 'http://daringfireball.net/projects/markdown/',
	on_format => sub {
	    my $text = shift;
	    my $ctx  = shift;
	    my $raw  = 0;
	    if (defined $ctx) {
	    my $output = $ctx->stash('markdown_output');
		if (defined $output && $output =~ m/^html/i) {
		    $g_empty_element_suffix = ">";
		    $ctx->stash('markdown_output', '');
		}
		elsif (defined $output && $output eq 'raw') {
		    $raw = 1;
		    $ctx->stash('markdown_output', '');
		}
		else {
		    $raw = 0;
		    $g_empty_element_suffix = " />";
		}
	    }
	    $text = $raw ? $text : Markdown($text);
	    $text;
	},
    });

    # If SmartyPants is loaded, add a combo Markdown/SmartyPants text filter:
    my $smartypants;

    {
	no warnings "once";
	$smartypants = $MT::Template::Context::Global_filters{'smarty_pants'};
    }

    if ($smartypants) {
	MT->add_text_filter('markdown_with_smartypants' => {
	    label     => 'Markdown With SmartyPants',
	    docs      => 'http://daringfireball.net/projects/markdown/',
	    on_format => sub {
		my $text = shift;
		my $ctx  = shift;
		if (defined $ctx) {
		    my $output = $ctx->stash('markdown_output');
		    if (defined $output && $output eq 'html') {
			$g_empty_element_suffix = ">";
		    }
		    else {
			$g_empty_element_suffix = " />";
		    }
		}
		$text = Markdown($text);
		$text = $smartypants->($text, '1');
	    },
	});
    }
}

#### BBEdit/command-line text filter interface ##########################
sub _main {
    local *ARGV = \@_;


    #### Check for command-line switches: #################
    my %options = ();
    my %cli_opts;
    use Getopt::Long;
    Getopt::Long::Configure(qw(bundling require_order pass_through));
    GetOptions(\%cli_opts,
	'help','h',
	'version|V',
	'shortversion|short-version|s',
	'html4tags',
	'htmlroot|r=s',
	'imageroot|i=s',
	'tabwidth|tab-width=s',
	'stylesheet|style-sheet',
    );
    if ($cli_opts{'help'}) {
	pod2usage(-verbose => 2, -exitval => 0);
    }
    if ($cli_opts{'h'}) {
	pod2usage(-verbose => 0, -exitval => 0);
    }
    if ($cli_opts{'version'}) { # Version info
	print "\nThis is Markdown, version $VERSION.\n", $COPYRIGHT;
	print "License is Modified BSD (aka 3-clause BSD) License\n";
	print "<https://opensource.org/licenses/BSD-3-Clause>\n";
	exit 0;
    }
    if ($cli_opts{'shortversion'}) { # Just the version number string.
	print $VERSION;
	exit 0;
    }
    if ($cli_opts{'html4tags'}) {	 # Use HTML tag style instead of XHTML
	$options{empty_element_suffix} = ">";
    }
    if ($cli_opts{'tabwidth'}) {
	my $tw = $cli_opts{'tabwidth'};
	die "invalid tab width (must be integer)\n" unless looks_like_number $tw;
	die "invalid tab width (must be >= 2 and <= 32)\n" unless $tw >= 2 && $tw <= 32;
	$options{tab_width} = int(0+$tw);
    }
    if ($cli_opts{'htmlroot'}) {	 # Use URL prefix
	$options{url_prefix} = $cli_opts{'htmlroot'};
    }
    if ($cli_opts{'imageroot'}) {	 # Use image URL prefix
	$options{img_prefix} = $cli_opts{'imageroot'};
    }
    if ($cli_opts{'stylesheet'}) {  # Display the style sheet
	$options{show_styles} = 1;
    }
    $options{tab_width} = 8 unless defined($options{tab_width});

    #### Show the style sheet if requested
    if ($options{show_styles}) {
	my $stylesheet = $g_style_sheet;
	$stylesheet =~ s/%\(base\)/$g_style_prefix/g;
	print $stylesheet;
    }

    #### Process incoming text: ###########################
    for (;;) {
	local $_;
	{
	    local $/; # Slurp the whole file
	    $_ = <>;
	}
	defined($_) or last;
	print Markdown($_, \%options);
    }


    exit 0;
}


sub Markdown {
#
# Primary function. The order in which other subs are called here is
# essential. Link and image substitutions need to happen before
# _EscapeSpecialChars(), so that any *'s or _'s in the <a>
# and <img> tags get encoded.
#
    my $text = shift;
    defined $text or $text='';

    # Any remaining arguments after the first are options; either a single
    # hashref or a list of name, value paurs.
    %opt = (
	# set initial defaults
	style_prefix		=> $g_style_prefix,
	empty_element_suffix	=> $g_empty_element_suffix,
	tab_width		=> $g_tab_width,
	indent_width		=> $g_indent_width,
	url_prefix		=> "", # Prefixed to non-absolute URLs
	img_prefix		=> "", # Prefixed to non-absolute image URLs
    );
    my %args = ();
    if (ref($_[0]) eq "HASH") {
	%args = %{$_[0]};
    } else {
	%args = @_;
    }
    while (my ($k,$v) = each %args) {
	$opt{$k} = $v;
    }

    # Clear the globals. If we don't clear these, you get conflicts
    # from other articles when generating a page which contains more than
    # one article (e.g. an index page that shows the N most recent
    # articles):
    %g_urls = ();
    %g_titles = ();
    %g_anchors = ();
    %g_block_ids = ();
    %g_html_blocks = ();
    %g_code_blocks = ();
    $g_list_level = 0;

    # Standardize line endings:
    $text =~ s{\r\n}{\n}g;  # DOS to Unix
    $text =~ s{\r}{\n}g;    # Mac to Unix

    # Make sure $text ends with a couple of newlines:
    $text .= "\n\n";

    # Handle backticks-delimited code blocks
    $text = _HashBTCodeBlocks($text);

    # Convert all tabs to spaces.
    $text = _Detab($text);

    # Strip any lines consisting only of spaces.
    # This makes subsequent regexen easier to write, because we can
    # match consecutive blank lines with /\n+/ instead of something
    # contorted like / *\n+/ .
    $text =~ s/^ +$//mg;

    # Turn block-level HTML blocks into hash entries
    $text = _HashHTMLBlocks($text);

    # Strip link definitions, store in hashes.
    $text = _StripLinkDefinitions($text);

    $text = _RunBlockGamut("\n".$text, 1);

    # Unhashify code blocks
    $text =~ s/(\005\d+\006)/$g_code_blocks{$1}/g;

    $text = _UnescapeSpecialChars($text);

    $text .= "\n" unless $text eq "";
    return $text;
}


sub _HashBTCodeBlocks {
#
#   Process Markdown backticks (```) delimited code blocks
#
    my $text = shift;

    $text =~ s{
	    (?:\n|\A)
		``(`+)[ \t]*(?:([\w.+-]+)[ \t]*)?\n
	     ( # $3 = the code block -- one or more lines, starting with ```
	      (?:
		.*\n+
	      )+?
	     )
	    (?:(?:``\1[ \t]*(?:\n|\Z))|\Z) # and ending with ``` or end of document
	}{
	    # $2 contains syntax highlighting to use if defined
	    my $codeblock = $3;
	    $codeblock =~ s/[ \t]+$//mg; # trim trailing spaces on lines
	    $codeblock = _Detab($codeblock, 8); # physical tab stops are always 8
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace
	    $codeblock = _EncodeCode($codeblock); # or run highlighter here
	    $codeblock = "<div class=\"$opt{style_prefix}code-bt\"><dl></dl><pre><code>"
		. $codeblock . "\n</code></pre></div>";

	    my $key = block_id($codeblock);
	    $g_html_blocks{$key} = $codeblock;
	    "\n\n" . $key . "\n\n";
	}egmx;

    return $text;
}


sub _StripLinkDefinitions {
#
# Strips link definitions from text, stores the URLs and titles in
# hash references.
#
    my $text = shift;
    my $less_than_indent = $opt{indent_width} - 1;

    # Link defs are in the form: ^[id]: url "optional title"
    while ($text =~ s{
			^[ ]{0,$less_than_indent}\[(.+)\]: # id = $1
			  [ ]*
			  \n?		    # maybe *one* newline
			  [ ]*
			<?(\S+?)>?	    # url = $2
			  [ ]*
			  \n?		    # maybe one newline
			  [ ]*
			(?:
			    (?<=\s)	    # lookbehind for whitespace
			    ["(]
			    (.+?)	    # title = $3
			    [")]
			    [ ]*
			)?  # title is optional
			(?:\n+|\Z)
		    }
		    {}mx) {
	$g_urls{lc $1} = _EncodeAmpsAndAngles( $2 ); # Link IDs are case-insensitive
	if ($3) {
	    $g_titles{lc $1} = $3;
	    $g_titles{lc $1} =~ s/\042/&quot;/g;
	}
    }

    return $text;
}

my ($block_tags_a, $block_tags_b);
BEGIN {
    $block_tags_a = qr/p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del/o;
    $block_tags_b = qr/p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math/o;
}

sub _HashHTMLBlocks {
    my $text = shift;
    my $less_than_indent = $opt{indent_width} - 1;

    # Hashify HTML blocks:
    # We only want to do this for block-level HTML tags, such as headers,
    # lists, and tables. That's because we still want to wrap <p>s around
    # "paragraphs" that are wrapped in non-block-level tags, such as anchors,
    # phrase emphasis, and spans. The list of tags we're looking for is
    # hard-coded:

    # First, look for nested blocks, e.g.:
    #   <div>
    #       <div>
    #       tags for inner block must be indented.
    #       </div>
    #   </div>
    #
    # The outermost tags must start at the left margin for this to match, and
    # the inner nested divs must be indented.
    # We need to do this before the next, more liberal match, because the next
    # match will start at the first `<div>` and stop at the first `</div>`.
    $text =~ s{
		(			# save in $1
		    ^			# start of line (with /m)
		    <($block_tags_a)	# start tag = $2
		    \b			# word break
		    (.*\n)*?		# any number of lines, minimally matching
		    </\2>		# the matching end tag
		    [ ]*		# trailing spaces
		    (?=\n+|\Z) # followed by a newline or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }egmx;


    #
    # Now match more liberally, simply from `\n<tag>` to `</tag>\n`
    #
    $text =~ s{
		(			# save in $1
		    ^			# start of line (with /m)
		    <($block_tags_b)	# start tag = $2
		    \b			# word break
		    (.*\n)*?		# any number of lines, minimally matching
		    .*</\2>		# the matching end tag
		    [ ]*		# trailing spaces
		    (?=\n+|\Z) # followed by a newline or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }egmx;
    # Special case just for <hr />. It was easier to make a special case than
    # to make the other regex more complicated.
    $text =~ s{
		(?:
		    (?<=\n\n)	    # Starting after a blank line
		    |		    # or
		    \A\n?	    # the beginning of the doc
		)
		(			# save in $1
		    [ ]{0,$less_than_indent}
		    <(hr)		# start tag = $2
		    \b			# word break
		    ([^<>])*?		#
		    /?>			# the matching end tag
		    [ ]*
		    (?=\n{2,}|\Z)	# followed by a blank line or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }egx;

    # Special case for standalone HTML comments:
    $text =~ s{
		(?:
		    (?<=\n\n)	    # Starting after a blank line
		    |		    # or
		    \A\n?	    # the beginning of the doc
		)
		(		    # save in $1
		    [ ]{0,$less_than_indent}
		    (?s:
			<!
			(--.*?--\s*)+
			>
		    )
		    [ ]*
		    (?=\n{2,}|\Z)   # followed by a blank line or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }egx;


    return $text;
}


sub _RunBlockGamut {
#
# These are all the transformations that form block-level
# tags like paragraphs, headers, and list items.
#
    my ($text, $anchors) = @_;

    $text = _DoHeaders($text, $anchors);

    # Do Horizontal Rules:
    $text =~ s{^ {0,3}\*(?: {0,2}\*){2,}[ ]*$}{\n<hr$opt{empty_element_suffix}\n}gm;
    $text =~ s{^ {0,3}\_(?: {0,2}\_){2,}[ ]*$}{\n<hr$opt{empty_element_suffix}\n}gm;
    $text =~ s{^ {0,3}\-(?: {0,2}\-){2,}[ ]*$}{\n<hr$opt{empty_element_suffix}\n}gm;

    $text = _DoLists($text);

    $text = _DoCodeBlocks($text);

    $text = _DoBlockQuotes($text);

    # We already ran _HashHTMLBlocks() before, in Markdown(), but that
    # was to escape raw HTML in the original Markdown source. This time,
    # we're escaping the markup we've just created, so that we don't wrap
    # <p> tags around block-level tags.
    $text = _HashHTMLBlocks($text);

    $text = _FormParagraphs($text);

    return $text;
}


sub _RunSpanGamut {
#
# These are all the transformations that occur *within* block-level
# tags like paragraphs, headers, and list items.
#
    my $text = shift;

    $text = _DoCodeSpans($text);

    $text = _EscapeSpecialChars($text);

    # Process anchor and image tags. Images must come first,
    # because ![foo][f] looks like an anchor.
    $text = _DoImages($text);
    $text = _DoAnchors($text);

    # Make links out of things like `<http://example.com/>`
    # Must come after _DoAnchors(), because you can use < and >
    # delimiters in inline links like [this](<url>).
    $text = _DoAutoLinks($text);

    $text = _EncodeAmpsAndAngles($text);

    $text = _DoItalicsAndBoldAndStrike($text);

    # Do hard breaks:
    $text =~ s/ {2,}\n/<br$opt{empty_element_suffix}\n/g;

    return $text;
}


sub _EscapeSpecialChars {
    my $text = shift;
    my $tokens ||= _TokenizeHTML($text);

    $text = ''; # rebuild $text from the tokens
#   my $in_pre = 0;  # Keep track of when we're inside <pre> or <code> tags.
#   my $tags_to_skip = qr!<(/?)(?:pre|code|kbd|script|math)[\s>]!;

    foreach my $cur_token (@$tokens) {
	if ($cur_token->[0] eq "tag") {
	    # Within tags, encode *, _ and ~ so they don't conflict
	    # with their use in Markdown for italics and strong.
	    # We're replacing each such character with its
	    # corresponding block id value; this is likely
	    # overkill, but it should prevent us from colliding
	    # with the escape values by accident.
	    $cur_token->[1] =~ s!([*_~])!$g_escape_table{$1}!g;
	    $text .= $cur_token->[1];
	} else {
	    my $t = $cur_token->[1];
	    $t = _EncodeBackslashEscapes($t);
	    $text .= $t;
	}
    }
    return $text;
}


sub _DoAnchors {
#
# Turn Markdown link shortcuts into XHTML <a> tags.
#
    my $text = shift;

    #
    # First, handle reference-style links: [link text] [id]
    #
    $text =~ s{
	(		    # wrap whole match in $1
	  \[
	    ($g_nested_brackets) # link text = $2
	  \]

	  [ ]?		    # one optional space
	  (?:\n[ ]*)?	    # one optional newline followed by spaces

	  \[
	    (.*?)	    # id = $3
	  \]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my $link_id	= lc $3;

	if ($link_id eq "") {
	    $link_id = lc $link_text;	  # for shortcut links like [this][].
	}

	if (defined($g_urls{$link_id}) || defined($g_anchors{$link_id})) {
	    my $url = $g_urls{$link_id};
	    $url = defined($url) ? _PrefixURL($url) : $g_anchors{$link_id};
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<a href=\"$url\"";
	    if ( defined $g_titles{$link_id} ) {
		my $title = $g_titles{$link_id};
		$title =~ s!([*_~])!$g_escape_table{$1}!g;
		$result .=  " title=\"$title\"";
	    }
	    $result .= ">$link_text</a>";
	}
	else {
	    $result = $whole_match;
	}
	$result;
    }xsge;

    #
    # Next, inline-style links: [link text](url "optional title")
    #
    $text =~ s{
	(		# wrap whole match in $1
	  \[
	    ($g_nested_brackets) # link text = $2
	  \]
	  \(		# literal paren
	    [ ]*
	    <?(.*?)>?	# href = $3
	    [ ]*
	    (		# $4
	      (['\042]) # quote char = $5
	      (.*?)	# Title = $6
	      \5	# matching quote
	    )?		# title is optional
	  \)
	)
    }{
	my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my $url		= $3;
	my $title	= $6;

	$url = _PrefixURL($url);
	# We've got to encode these to avoid conflicting
	# with italics, bold and strike through.
	$url =~ s!([*_~])!$g_escape_table{$1}!g;
	$result = "<a href=\"$url\"";

	if (defined $title) {
	    $title =~ s/\042/&quot;/g;
	    $title =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result .= " title=\"$title\"";
	}

	$result .= ">$link_text</a>";

	$result;
    }xsge;

    return $text;
}


sub _DoImages {
#
# Turn Markdown image shortcuts into <img> tags.
#
    my $text = shift;

    #
    # First, handle reference-style labeled images: ![alt text][id]
    #
    $text =~ s{
	(		# wrap whole match in $1
	  !\[
	    (.*?)	# alt text = $2
	  \]

	  [ ]?		# one optional space
	  (?:\n[ ]*)?	# one optional newline followed by spaces

	  \[
	    (.*?)	# id = $3
	  \]

	)
    }{
	my $result;
	my $whole_match = $1;
	my $alt_text	= $2;
	my $link_id	= lc $3;

	if ($link_id eq "") {
	    $link_id = lc $alt_text; # for shortcut links like ![this][].
	}

	$alt_text =~ s/"/&quot;/g;
	if (defined $g_urls{$link_id}) {
	    my $url = _PrefixURL($g_urls{$link_id});
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<img src=\"$url\" alt=\"$alt_text\"";
	    if (defined $g_titles{$link_id}) {
		my $title = $g_titles{$link_id};
		$title =~ s!([*_~])!$g_escape_table{$1}!g;
		$result .=  " title=\"$title\"";
	    }
	    $result .= $opt{empty_element_suffix};
	}
	else {
	    # If there's no such link ID, leave intact:
	    $result = $whole_match;
	}

	$result;
    }xsge;

    #
    # Next, handle inline images:  ![alt text](url "optional title")
    # Don't forget: encode * and _

    $text =~ s{
	(		# wrap whole match in $1
	  !\[
	    (.*?)	# alt text = $2
	  \]
	  \(		# literal paren
	    [ ]*
	    <?(\S+?)>?	# src url = $3
	    [ ]*
	    (		# $4
	      (['\042]) # quote char = $5
	      (.*?)	# title = $6
	      \5	# matching quote
	      [ ]*
	    )?		# title is optional
	  \)
	)
    }{
	my $result;
	my $whole_match = $1;
	my $alt_text	= $2;
	my $url		= $3;
	my $title	= '';
	if (defined($6)) {
	    $title	= $6;
	}

	$url = _PrefixURL($url);
	$alt_text =~ s/"/&quot;/g;
	$title	  =~ s/"/&quot;/g;
	# We've got to encode these to avoid conflicting
	# with italics, bold and strike through.
	$url =~ s!([*_~])!$g_escape_table{$1}!g;
	$result = "<img src=\"$url\" alt=\"$alt_text\"";
	if (defined $title) {
	    $title =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result .= " title=\"$title\"";
	}
	$result .= $opt{empty_element_suffix};

	$result;
    }xsge;

    return $text;
}


sub _MakeAnchorId {
    use bytes;
    my $link = lc(shift);
    $link =~ s/^\s+//;
    $link =~ s/\s+$//;
    $link =~ s/\s+/ /g;
    $link =~ tr/-a-z0-9_/_/cs;
    return '' unless $link ne '';
    $link = md5_hex($link) if length($link) > 64;
    "_".$link."_";
}


sub _GetNewAnchorId {
    my $link = lc(shift);
    return '' if defined($g_anchors{$link});
    my $id = _MakeAnchorId($link);
    return '' unless $id;
    $g_anchors{$link} = '#'.$id;
    $id;
}


sub _DoHeaders {
    my ($text, $anchors) = @_;

    # Setext-style headers:
    #     Header 1
    #     ========
    #
    #     Header 2
    #     --------
    #
    #     Header 3
    #     ~~~~~~~~
    #
    $text =~ s{ ^(?:=+[ ]*\n)?(.+)[ ]*\n=+[ ]*\n+ }{
	my $h = $1;
	my $id = _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h1$id>" . _RunSpanGamut($h) . "</h1>\n\n";
    }egmx;

    $text =~ s{ ^(?:-+[ ]*\n)?(.+)[ ]*\n-+[ ]*\n+ }{
	my $h = $1;
	my $id = _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h2$id>" . _RunSpanGamut($h) . "</h2>\n\n";
    }egmx;

    $text =~ s{ ^(?:~+[ ]*\n)?(.+)[ ]*\n~+[ ]*\n+ }{
	my $h = $1;
	my $id = _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h3$id>" . _RunSpanGamut($h) . "</h3>\n\n";
    }egmx;


    # atx-style headers:
    #   # Header 1
    #   ## Header 2
    #   ## Header 2 with closing hashes ##
    #   ...
    #   ###### Header 6
    #
    $text =~ s{
	    ^(\#{1,6})	# $1 = string of #'s
	    [ ]*
	    (.+?)	# $2 = Header text
	    [ ]*
	    \#*		# optional closing #'s (not counted)
	    \n+
	}{
	    my $h = $2;
	    my $h_level = length($1);
	    my $id = $h_level <= 3 ? _GetNewAnchorId($h) : '';
	    $id = " id=\"$id\"" if $id ne "";
	    "<h$h_level$id>" . _RunSpanGamut($h) . "</h$h_level>\n\n";
	}egmx;

    return $text;
}


my ($marker_ul, $marker_ol, $marker_any);
BEGIN {
    # Re-usable patterns to match list item bullets and number markers:
    $marker_ul  = qr/[*+-]/o;
    $marker_ol  = qr/\d+[.]/o;
    $marker_any = qr/(?:$marker_ul|$marker_ol)/o;
}

sub _DoLists {
#
# Form HTML ordered (numbered) and unordered (bulleted) lists.
#
    my $text = shift;
    my $indent = $opt{indent_width};
    my $less_than_indent = $indent - 1;
    my $less_than_double_indent = 2 * $indent - 1;

    # Re-usable pattern to match any entirel ul or ol list:
    my $whole_list = qr{
	(			    # $1 = whole list
	  (			    # $2
	    (?<=\n)
	    [ ]{0,$less_than_indent}
	    (${marker_any})	    # $3 = first list item marker
	    [ ]+
	  )
	  (?s:.+?)
	  (			    # $4
	      \z
	    |
	      \n{2,}
	      (?=\S)
	      (?!		    # Negative lookahead for another list item marker
		${marker_any}[ ]
	      )
	  )
	)
    }mx;

    # We use a different prefix before nested lists than top-level lists.
    # See extended comment in _ProcessListItems().
    #
    # Note: (jg) There's a bit of duplication here. My original implementation
    # created a scalar regex pattern as the conditional result of the test on
    # $g_list_level, and then only ran the $text =~ s{...}{...}egmx
    # substitution once, using the scalar as the pattern. This worked,
    # everywhere except when running under MT on my hosting account at Pair
    # Networks. There, this caused all rebuilds to be killed by the reaper (or
    # perhaps they crashed, but that seems incredibly unlikely given that the
    # same script on the same server ran fine *except* under MT. I've spent
    # more time trying to figure out why this is happening than I'd like to
    # admit. My only guess, backed up by the fact that this workaround works,
    # is that Perl optimizes the substition when it can figure out that the
    # pattern will never change, and when this optimization isn't on, we run
    # afoul of the reaper. Thus, the slightly redundant code to that uses two
    # static s/// patterns rather than one conditional pattern.
    #
    # Note: (kjm) With the addition of the two-of-the-same-kind-in-a-row-
    # starts-a-list-at-the-top-level rule the two patterns really are somewhat
    # different now.

    if ($g_list_level) {
	$text =~ s{
		^
		$whole_list
	    }{
		my $list = $1;
		my $list_type = ($3 =~ m/$marker_ul/) ? "ul" : "ol";
		# Turn double returns into triple returns, so that we can make a
		# paragraph for the last item in a list, if necessary:
		$list =~ s/\n\n/\n\n\n/g;
		my $result = _ProcessListItems($list_type, $list, $marker_any);
		$result = "<$list_type>\n" . $result . "</$list_type>\n";
		$result;
	    }egmx;
    }
    else {
	$text =~ s{
		(?: (?<=\n\n) |
		    \A\n? |
		    (?:(?<=\n) # two of the same kind of marker lines
		       (?=[ ]{0,$less_than_indent}$marker_ul[ ].*\n
		          [ ]{0,$less_than_indent}$marker_ul[ ])) |
		    (?:(?<=\n) # in a row will start a list
		       (?=[ ]{0,$less_than_indent}$marker_ol[ ].*\n
		          [ ]{0,$less_than_indent}$marker_ol[ ])) |
		    (?:(?<=\n) # or any marker and a sublist marker
		       (?=[ ]{0,$less_than_indent}$marker_any[ ].*\n
		          [ ]{$indent,$less_than_double_indent}$marker_any[ ]))
		)
		$whole_list
	    }{
		my $list = $1;
		my $list_type = ($3 =~ m/$marker_ul/) ? "ul" : "ol";
		# Turn double returns into triple returns, so that we can make a
		# paragraph for the last item in a list, if necessary:
		$list =~ s/\n\n/\n\n\n/g;
		my $result = _ProcessListItems($list_type, $list, $marker_any);
		$result = "<$list_type>\n" . $result . "</$list_type>\n";
		$result;
	    }egmx;
    }


    return $text;
}


sub _ProcessListItems {
#
#   Process the contents of a single ordered or unordered list, splitting it
#   into individual list items.
#

    my $list_type = shift;
    my $list_str = shift;
    my $marker_any = shift;


    # The $g_list_level global keeps track of when we're inside a list.
    # Each time we enter a list, we increment it; when we leave a list,
    # we decrement. If it's zero, we're not in a list anymore.
    #
    # We do this because when we're not inside a list, we want to treat
    # something like this:
    #
    #	I recommend upgrading to version
    #	8. Oops, now this line is treated
    #	as a sub-list.
    #
    # As a single paragraph, despite the fact that the second line starts
    # with a digit-period-space sequence.
    #
    # Whereas when we're inside a list (or sub-list), that line will be
    # treated as the start of a sub-list. What a kludge, huh? This is
    # an aspect of Markdown's syntax that's hard to parse perfectly
    # without resorting to mind-reading. Perhaps the solution is to
    # change the syntax rules such that sub-lists must start with a
    # starting cardinal number; e.g. "1." or "a.".

    $g_list_level++;

    # trim trailing blank lines:
    $list_str =~ s/\n{2,}\z/\n/;

    $list_str =~ s{
	(\n)?				# leading line = $1
	(^[ ]*)				# leading whitespace = $2
	($marker_any) [ ] ([ ]*)	# list marker = $3 leading item space = $4
	((?s:.+?)			# list item text = $5
	 (?:\n{1,2}))
	(?= \n* (?: \z | \2 $marker_any [ ]))
    }{
	my $item = $5;
	my $leading_line = $1;
	my $leading_space = $2;
	my $leading_item_space = $4;
	my $liclass = '';
	my $checkbox = '';

	if ($list_type eq "ul" && !$leading_item_space && $item =~ /^\[([ xX])\] +(.*)$/s) {
	    my $checkmark = lc $1;
	    $item = $2;
	    my ($checkbox_class, $checkbox_val);
	    if ($checkmark eq "x") {
		($checkbox_class, $checkbox_val) = ("checkbox-on", "x");
	    } else {
		($checkbox_class, $checkbox_val) = ("checkbox-off", "&#160;");
	    }
	    $liclass = " class=\"$opt{style_prefix}$checkbox_class\"";
	    $checkbox = "<span><span></span></span><span></span><span>[<tt>$checkbox_val</tt>]&#160;</span>";
	}

	if ($leading_line or ($item =~ m/\n{2,}/)) {
	    $item = _RunBlockGamut(_Outdent($item));
	}
	else {
	    # Recursion for sub-lists:
	    $item = _DoLists(_Outdent($item));
	    chomp $item;
	    $item = _RunSpanGamut($item);
	}

	"<li$liclass>" . $checkbox . $item . "</li>\n";
    }egmx;

    $g_list_level--;
    return $list_str;
}



sub _DoCodeBlocks {
#
#   Process Markdown `<pre><code>` blocks.
#

    my $text = shift;

    $text =~ s{
	    (?:\n\n|\A\n?)
	    (		# $1 = the code block -- one or more lines, starting with indent_width spaces
	      (?:
		(?:[ ]{$opt{indent_width}})  # Lines must start with indent_width of spaces
		.*\n+
	      )+
	    )
	    ((?=^[ ]{0,$opt{indent_width}}\S)|\Z) # Lookahead for non-space at line-start, or end of doc
	}{
	    my $codeblock = $1;

	    $codeblock =~ s/\n\n\n/\n\n/g; # undo "paragraph for last list item" change
	    $codeblock = _EncodeCode(_Outdent($codeblock));
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace

	    my $result = "<div class=\"$opt{style_prefix}code\"><dl></dl><pre><code>"
		. $codeblock . "\n</code></pre></div>";
	    my $key = block_id($result);
	    $g_code_blocks{$key} = $result;
	    "\n\n" . $key . "\n\n";
	}egmx;

    return $text;
}


sub _DoCodeSpans {
#
# * Backtick quotes are used for <code></code> spans.
#
# * You can use multiple backticks as the delimiters if you want to
#   include literal backticks in the code span. So, this input:
#
#     Just type ``foo `bar` baz`` at the prompt.
#
#   Will translate to:
#
#     <p>Just type <code>foo `bar` baz</code> at the prompt.</p>
#
#   There's no arbitrary limit to the number of backticks you
#   can use as delimters. If you need three consecutive backticks
#   in your code, use four for delimiters, etc.
#
# * You can use spaces to get literal backticks at the edges:
#
#     ... type `` `bar` `` ...
#
#   Turns to:
#
#     ... type <code>`bar`</code> ...
#

    my $text = shift;

    $text =~ s@
	    (`+)	# $1 = Opening run of `
	    (.+?)	# $2 = The code block
	    (?<!`)
	    \1		# Matching closer
	    (?!`)
	@
	    my $c = "$2";
	    $c =~ s/^[ ]+//g; # leading whitespace
	    $c =~ s/[ ]+$//g; # trailing whitespace
	    $c = _EncodeCode($c);
	    "<code>$c</code>";
	@egsx;

    return $text;
}


sub _EncodeCode {
#
# Encode/escape certain characters inside Markdown code runs.
# The point is that in code, these characters are literals,
# and lose their special Markdown meanings.
#
    local $_ = shift;

    # Encode all ampersands; HTML entities are not
    # entities within a Markdown code span.
    s/&/&amp;/g;

    # Encode $'s, but only if we're running under Blosxom.
    # (Blosxom interpolates Perl variables in article bodies.)
    s/\$/&#036;/g if $_haveBX;

    # Do the angle bracket song and dance:
    s! <  !&lt;!gx;
    s! >  !&gt;!gx;

    # Now, escape characters that are magic in Markdown:
    s!([*_~{}\[\]\\])!$g_escape_table{$1}!g;

    return $_;
}


sub _DoItalicsAndBoldAndStrike {
    my $text = shift;

    # <strong> must go first:
    $text =~ s{ \*\* (?=\S) (.+?[*_]*) (?<=\S) \*\* }
	{<strong>$1</strong>}gsx;
    $text =~ s{ (?<!\w) __ (?=\S) (.+?[*_]*) (?<=\S) __ (?!\w) }
	{<strong>$1</strong>}gsx;

    $text =~ s{ ~~ (?=\S) (.+?[*_]*) (?<=\S) ~~ }
	{<strike>$1</strike>}gsx;

    $text =~ s{ \* (?=\S) (.+?) (?<=\S) \* }
	{<em>$1</em>}gsx;
    $text =~ s{ (?<!\w) _ (?=\S) (.+?) (?<=\S) _ (?!\w) }
	{<em>$1</em>}gsx;

    return $text;
}


sub _DoBlockQuotes {
    my $text = shift;

    $text =~ s{
	  (			# Wrap whole match in $1
	    (
	      ^[ ]*>[ ]?	# '>' at the start of a line
		.+\n		# rest of the first line
	      (.+\n)*		# subsequent consecutive lines
	      \n*		# blanks
	    )+
	  )
	}{
	    my $bq = $1;
	    $bq =~ s/^[ ]*>[ ]?//gm; # trim one level of quoting
	    $bq =~ s/^[ ]+$//mg;	 # trim whitespace-only lines
	    $bq = _RunBlockGamut($bq);	 # recurse

	    $bq =~ s/^/  /mg;
	    "<blockquote>\n$bq\n</blockquote>\n\n";
	}egmx;


    return $text;
}


sub _FormParagraphs {
#
# Params:
#   $text - string to process with html <p> tags
#
    my $text = shift;

    # Strip leading and trailing lines:
    $text =~ s/\A\n+//;
    $text =~ s/\n+\z//;

    my @grafs = split(/\n{2,}/, $text);

    #
    # Wrap <p> tags.
    #
    foreach (@grafs) {
	unless (defined($g_html_blocks{$_}) || defined($g_code_blocks{$_})) {
	    $_ = _RunSpanGamut($_);
	    s/^([ ]*)/<p>/;
	    $_ .= "</p>";
	}
    }

    #
    # Unhashify HTML blocks
    #
    foreach (@grafs) {
	if (defined( $g_html_blocks{$_} )) {
	    $_ = $g_html_blocks{$_};
	}
    }

    return join "\n\n", @grafs;
}


sub _EncodeAmpsAndAngles {
# Smart processing for ampersands and angle brackets that need to be encoded.

    my $text = shift;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    # Encode naked <'s
    $text =~ s{<(?![a-z/?\$!])}{&lt;}gi;

    return $text;
}


sub _EncodeBackslashEscapes {
#
# Parameter: String.
# Returns:   String after processing the following backslash escape sequences.
#
    local $_ = shift;

    s!\\\\!$g_escape_table{'\\'}!go; # Must process escaped backslashes first.
    s{\\([`*_~{}\[\]()>#+\-.!`])}{$g_escape_table{$1}}g;

    return $_;
}


sub _DoAutoLinks {
    local $_ = shift;

    s{<((https?|ftps?):[^'\042>\s]+)>}{<a href="$1">&lt;$1&gt;</a>}gi;

    # Email addresses: <address@domain.foo>
    s{
	<
	(?:mailto:)?
	(
	    [-.\w]+
	    \@
	    [-a-z0-9]+(\.[-a-z0-9]+)*\.[a-z]+
	)
	>
    }{
	_EncodeEmailAddress(_UnescapeSpecialChars($1), "&#x3c;", "&#62;");
    }egix;

    # (kjm) I don't do "x" patterns
    s{(?<![\042'<>])(?<!&[Ll][Tt];)(?<!&#60;)(?<!&#x3[Cc];)\b((?:https?|ftps?)://(?:[-a-zA-Z0-9./?\&\%=_~!*;:\@+\$,\x23](?:(?<![.,:;])|(?=[^\s])))+)}
     {<a href="$1">$1</a>}sog;
    s{(?<![][])(?<!\] )\[RFC( ?)([0-9]{1,5})\](?![][])(?! \[)}
     {[<a href="http://tools.ietf.org/html/rfc$2">RFC$1$2</a>]}sog;

    return $_;
}


sub _EncodeEmailAddress {
#
# Input: an email address, e.g. "foo@example.com"
#
# Output: the email address as a mailto link, with each character
#         of the address encoded as either a decimal or hex entity, in
#         the hopes of foiling most address harvesting spam bots. E.g.:
#
#   <a href="&#x6D;&#97;&#105;&#108;&#x74;&#111;:&#102;&#111;&#111;&#64;&#101;
#   x&#x61;&#109;&#x70;&#108;&#x65;&#x2E;&#99;&#111;&#109;">&#102;&#111;&#111;
#   &#64;&#101;x&#x61;&#109;&#x70;&#108;&#x65;&#x2E;&#99;&#111;&#109;</a>
#
# Based on a filter by Matthew Wickline, posted to the BBEdit-Talk
# mailing list: <http://tinyurl.com/yu7ue>
#

    my ($addr, $prefix, $suffix) = @_;
    $prefix = "" unless defined($prefix);
    $suffix = "" unless defined($suffix);

    srand(unpack('N',md5($addr)));
    my @encode = (
	sub { '&#' .		     ord(shift)	  . ';' },
	sub { '&#x' . sprintf( "%X", ord(shift) ) . ';' },
	sub {				 shift		},
    );

    $addr = "mailto:" . $addr;

    $addr =~ s{(.)}{
	my $char = $1;
	if ( $char eq '@' ) {
	    # this *must* be encoded. I insist.
	    $char = $encode[int rand 1]->($char);
	} elsif ( $char ne ':' ) {
	    # leave ':' alone (to spot mailto: later)
	    my $r = rand;
	    # roughly 10% raw, 45% hex, 45% dec
	    $char = (
		$r > .9	  ?  $encode[2]->($char)  :
		$r < .45  ?  $encode[1]->($char)  :
			     $encode[0]->($char)
	    );
	}
	$char;
    }gex;

    # strip the mailto: from the visible part
    (my $bareaddr = $addr) =~ s/^.+?://;
    $addr = qq{<a href="$addr">$prefix$bareaddr$suffix</a>};

    return $addr;
}


sub _UnescapeSpecialChars {
#
# Swap back in all the special characters we've hidden.
#
    my $text = shift;

    while( my($char, $hash) = each(%g_escape_table) ) {
	$text =~ s/$hash/$char/g;
    }
    return $text;
}


sub _TokenizeHTML {
#
# Parameter: String containing HTML markup.
# Returns:   Reference to an array of the tokens comprising the input
#            string. Each token is either a tag (possibly with nested,
#            tags contained therein, such as <a href="<MTFoo>">, or a
#            run of text between tags. Each element of the array is a
#            two-element array; the first is either 'tag' or 'text';
#            the second is the actual value.
#
#
# Derived from the _tokenize() subroutine from Brad Choate's MTRegex plugin.
#   <http://www.bradchoate.com/past/mtregex.php>
#

    my $str = shift;
    my $pos = 0;
    my $len = length $str;
    my @tokens;

    my $depth = 6;
    my $nested_tags = join('|', ('(?:<[a-z/!$](?:[^<>]') x $depth) . (')*>)' x $depth);
    my $match = qr/(?s: <! ( -- .*? -- \s* )+ > ) | # comment
		   (?s: <\? .*? \?> ) |		    # processing instruction
		   $nested_tags/iox;		    # nested tags

    while ($str =~ m/($match)/g) {
	my $whole_tag = $1;
	my $sec_start = pos $str;
	my $tag_start = $sec_start - length $whole_tag;
	if ($pos < $tag_start) {
	    push @tokens, ['text', substr($str, $pos, $tag_start - $pos)];
	}
	push @tokens, ['tag', $whole_tag];
	$pos = pos $str;
    }
    push @tokens, ['text', substr($str, $pos, $len - $pos)] if $pos < $len;
    \@tokens;
}


sub _Outdent {
#
# Remove one level of line-leading indent_width of spaces
#
    my $text = shift;

    $text =~ s/^ {1,$opt{indent_width}}//gm;
    return $text;
}


sub _Detab {
#
# Expand tabs to spaces using $opt{tab_width} if no second argument
#
    my $text = shift;
    my $ts = shift || $opt{tab_width};
    # From the Perl camel book "Fluent Perl" section (slightly modified)
    $text =~ s/(.*?)(\t+)/$1 . ' ' x (length($2) * $ts - length($1) % $ts)/ge;
    return $text;
}


sub _PrefixURL {
#
# Add URL prefix if needed
#
    my $url = shift;

    return $url unless $opt{url_prefix} ne '' || $opt{img_prefix} ne '';
    return $url if $url =~ m,^//, || $url =~ /^[A-Za-z][A-Za-z0-9+.-]*:/;
    my $ans = $opt{url_prefix};
    $ans = $opt{img_prefix}
	if $opt{img_prefix} ne '' && $url =~ /\.(?:png|gif|jpe?g|svg?z)$/i;
    return $url unless $ans ne '';
    $ans .= '/' if substr($ans, -1, 1) ne '/';
    $ans .= substr($url, 0, 1) eq '/' ? substr($url, 1) : $url;
    return $ans;
}


BEGIN {
    $g_style_sheet = <<'STYLESHEET';

<style type="text/css">
/* <![CDATA[ */

/* Markdown.pl fancy style sheet
** Copyright (C) 2017 Kyle J. McKay.
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**
**   1. Redistributions of source code must retain the above copyright notice,
**      this list of conditions and the following disclaimer.
**
**   2. Redistributions in binary form must reproduce the above copyright
**      notice, this list of conditions and the following disclaimer in the
**      documentation and/or other materials provided with the distribution.
**
**   3. Neither the name of the copyright holder nor the names of its
**      contributors may be used to endorse or promote products derived from
**      this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
** LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
** SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
** INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
** CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
** ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
** POSSIBILITY OF SUCH DAMAGE.
*/

div.%(base)code-bt > pre, div.%(base)code > pre {
	margin: 0 3ex;
	padding: 1ex;
	background-color: #eee;
	overflow: auto;
}

li.%(base)checkbox-on,
li.%(base)checkbox-off {
	list-style-type: none;
	display: block;
}
li.%(base)checkbox-on > span:first-child + span + span,
li.%(base)checkbox-off > span:first-child + span + span {
	position: absolute;
	clip: rect(0,0,0,0);
}
li.%(base)checkbox-on > span:first-child,
li.%(base)checkbox-off > span:first-child,
li.%(base)checkbox-on > span:first-child + span,
li.%(base)checkbox-off > span:first-child + span {
	display: block;
	position: absolute;
	margin-left: -3ex;
	width: 1em;
	height: 1em;
}
li.%(base)checkbox-on > span:first-child > span:first-child,
li.%(base)checkbox-off > span:first-child > span:first-child {
	display: block;
	position: absolute;
	left: 0.75pt; top: 0.75pt; right: 0.75pt; bottom: 0.75pt;
}
li.%(base)checkbox-on > span:first-child > span:first-child:before,
li.%(base)checkbox-off > span:first-child > span:first-child:before {
	display: inline-block;
	position: relative;
	right: 1pt;
	width: 100%;
	height: 100%;
	border: 1pt solid;
	content: "";
}
li.%(base)checkbox-on > span:first-child + span:before {
	position: relative;
	left: 2pt;
	bottom: 1pt;
	font-size: 125%;
	line-height: 80%;
	content: "\2713";
}

/* ]]> */
</style>

STYLESHEET
    $g_style_sheet =~ s/^\s+//g;
    $g_style_sheet =~ s/\s+$//g;
    $g_style_sheet .= "\n";
}

1;

__DATA__

=head1 NAME

Markdown.pl - convert Markdown format text files to HTML

=head1 SYNOPSIS

B<Markdown.pl> [B<--help>] [B<--html4tags>] [B<--htmlroot>=I<prefix>]
    [B<--imageroot>=I<prefix>] [B<--version>] [B<--shortversion>]
    [B<--tabwidth>=I<num>] [B<--stylesheet>] [--] [I<file>...]

 Options:
   -h                                   show short usage help
   --help                               show long detailed help
   --html4tags                          use <br> instead of <br />
   --tabwidth=num                       expand tabs to num instead of 8
   -r prefix | --htmlroot=prefix        append relative non-img URLs
                                        to prefix
   -i prefix | --imageroot=prefix	append relative img URLs to
                                        prefix
   -V | --version                       show version, authors, license
                                        and copyright
   -s | --shortversion                  show just the version number
   --stylesheet                         output the fancy style sheet
   --                                   end options and treat next
                                        argument as file

=head1 DESCRIPTION

Markdown is a text-to-HTML filter; it translates an easy-to-read /
easy-to-write structured text format into HTML. Markdown's text format
is most similar to that of plain text email, and supports features such
as headers, *emphasis*, code blocks, blockquotes, and links.

Markdown's syntax is designed not as a generic markup language, but
specifically to serve as a front-end to (X)HTML. You can  use span-level
HTML tags anywhere in a Markdown document, and you can use block level
HTML tags (like <div> and <table> as well).

For more information about Markdown's syntax, see the F<basics.md>
and F<syntax.md> files included with F<Markdown.pl>.


=head1 OPTIONS

Use "--" to end switch parsing. For example, to open a file named "-z", use:

    Markdown.pl -- -z

=over


=item B<--html4tags>

Use HTML 4 style for empty element tags, e.g.:

    <br>

instead of Markdown's default XHTML style tags, e.g.:

    <br />


=item B<--tabwidth>=I<num>

Expand tabs to I<num> character wide tab stop positions instead of the default
8.  Don't use this; physical tabs should always be expanded to 8-character
positions.  This option does I<not> affect the number of spaces needed to
start a new "indent level".  That will always be 4 no matter what value is
used (or implied by default) with this option.  Also note that tabs inside
backticks-delimited code blocks will always be expanded to 8-character tab
stop positions no matter what value is used for this option.

The value must be S<2 <= I<num> <= 32>.


=item B<-r> I<prefix>, B<--htmlroot>=I<prefix>

Any non-absolute URLs have I<prefix> prepended.


=item B<-i> I<prefix>, B<--imageroot>=I<prefix>

Any non-absolute URLs have I<prefix> prepended (overriding the B<-r> prefix
if any) but only if they end in an image suffix.


=item B<-V>, B<--version>

Display Markdown's version number and copyright information.


=item B<-s>, B<--shortversion>

Display the short-form version number.


=item B<--stylesheet>

Include the fancy style sheet at the beginning of the output.  This style
sheet makes fancy checkboxes and list numbering work.  Without it things
will still look fine except that the fancy stuff won't be there.

Use this option with no other arguments and redirect standard input to
/dev/null to get just the style sheet and nothing else.


=item B<-h>, B<--help>

Display Markdown's help.  With B<--help> full help is shown, with B<-h> only
the usage and options are shown.


=back


=head1 VERSION HISTORY

Z<> See the F<README> file for detailed release notes for this version.

=over

=item Z<> 1.0.4 - 05 Jun 2016

=item Z<> 1.0.3 - 06 Sep 2015

=item Z<> 1.0.2 - 03 Sep 2015

=item Z<> 1.0.1 - 14 Dec 2004

=item Z<> 1.0.0 - 28 Aug 2004

=back

=head1 AUTHORS

=over

=item John Gruber

=item L<http://daringfireball.net>

=item L<http://daringfireball.net/projects/markdown/>

=item E<160>

=back

=over

=item PHP port and other contributions by Michel Fortin

=item L<http://michelf.com>

=item E<160>

=back

=over

=item Additional enhancements and tweaks by Kyle J. McKay

=item mackyle<at>gmail.com

=back

=head1 COPYRIGHT AND LICENSE

=over

=item Copyright (C) 2003-2004 John Gruber

=item Copyright (C) 2015,2016 Kyle J. McKay

=item All rights reserved.

=back

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

=over

=item *

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

=item *

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

=item *

Neither the name "Markdown" nor the names of its contributors may
be used to endorse or promote products derived from this software
without specific prior written permission.

=back

This software is provided by the copyright holders and contributors "as
is" and any express or implied warranties, including, but not limited
to, the implied warranties of merchantability and fitness for a
particular purpose are disclaimed. In no event shall the copyright owner
or contributors be liable for any direct, indirect, incidental, special,
exemplary, or consequential damages (including, but not limited to,
procurement of substitute goods or services; loss of use, data, or
profits; or business interruption) however caused and on any theory of
liability, whether in contract, strict liability, or tort (including
negligence or otherwise) arising in any way out of the use of this
software, even if advised of the possibility of such damage.

=cut
