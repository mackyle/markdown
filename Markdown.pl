#!/usr/bin/env perl

#
# Markdown -- A text-to-HTML conversion tool for web writers
#
# Copyright (C) 2004 John Gruber
# Copyright (C) 2015,2016,2017,2018 Kyle J. McKay
# All rights reserved.
# License is Modified BSD (aka 3-clause BSD) License\n";
# See LICENSE file (or <https://opensource.org/licenses/BSD-3-Clause>)
#

package Markdown;

require 5.008;
use strict;
use warnings;

use Encode;

use vars qw($COPYRIGHT $VERSION @ISA @EXPORT_OK);

BEGIN {*COPYRIGHT =
\"Copyright (C) 2004 John Gruber
Copyright (C) 2015,2016,2017,2018 Kyle J. McKay
All rights reserved.
";
*VERSION = \"1.1.7"
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

my $encoder;
BEGIN {
	$encoder = Encode::find_encoding('Windows-1252') ||
		   Encode::find_encoding('ISO-8859-1') or
		   die "failed to load ISO-8859-1 encoder\n";
}

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
    $g_escape_table{""} = "\2\3";
    foreach my $char (split //, "\\\`*_~{}[]()>#+-.!|:") {
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

sub _strip {
	my $str = shift;
	defined($str) or return undef;
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	$str =~ s/\s+/ /g;
	$str;
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
	'deprecated',
	'htmlroot|r=s',
	'imageroot|i=s',
	'tabwidth|tab-width=s',
	'stylesheet|style-sheet',
	'no-stylesheet|no-style-sheet',
	'stub',
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
    my $stub = 0;
    if ($cli_opts{'stub'}) {
	$stub = 1;
    }
    if ($cli_opts{'html4tags'}) {	 # Use HTML tag style instead of XHTML
	$options{empty_element_suffix} = ">";
	$stub = -$stub;
    }
    if ($cli_opts{'deprecated'}) {	 # Allow <dir> and <menu> tags to pass through
	_SetAllowedTag("dir");
	_SetAllowedTag("menu");
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
    if ($cli_opts{'no-stylesheet'}) {  # Do not display the style sheet
	$options{show_styles} = 0;
    }
    $options{show_styles} = 1 if $stub && !defined($options{show_styles});
    $options{tab_width} = 8 unless defined($options{tab_width});

    my $hdr = sub {
	if ($stub > 0) {
	    print <<'HTML5';
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta charset="utf-8" />
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
HTML5
	} elsif ($stub < 0) {
	    print <<'HTML4';
<html>
<head>
<meta charset="utf-8">
<meta http-equiv="content-type" content="text/html; charset=utf-8">
HTML4
	}
	if ($stub && ($options{title} || $options{h1})) {
	    my $title = $options{title};
	    defined($title) && $title ne "" or $title = $options{h1};
	    if (defined($title) && $title ne "") {
		$title =~ s/&/&amp;/g;
		$title =~ s/</&lt;/g;
		print "<title>$title</title>\n";
	    }
	}
	if ($options{show_styles}) {
	    my $stylesheet = $g_style_sheet;
	    $stylesheet =~ s/%\(base\)/$g_style_prefix/g;
	    print $stylesheet;
	}
	if ($stub) {
	    print "</head>\n<body style=\"text-align:center\">\n",
		"<div style=\"display:inline-block;text-align:left;max-width:42pc\">\n";
	}
    };

    #### Process incoming text: ###########################
    my $didhdr;
    for (;;) {
	local $_;
	{
	    local $/; # Slurp the whole file
	    $_ = <>;
	}
	defined($_) or last;
	my $result = Markdown($_, \%options);
	if ($result ne "") {
	    if (!$didhdr) {
		&$hdr();
		$didhdr = 1;
	    }
	    print $result;
	}
    }
    &$hdr() unless $didhdr;
    print "</div>\n</body>\n</html>\n" if $stub;

    exit 0;
}


sub Markdown {
#
# Primary function. The order in which other subs are called here is
# essential. Link and image substitutions need to happen before
# _EscapeSpecialChars(), so that any *'s or _'s in the <a>
# and <img> tags get encoded.
#
    my $_text = shift;
    defined $_text or $_text='';

    my $text;
    if (Encode::is_utf8($_text) || utf8::decode($_text)) {
	$text = $_text;
    } else {
	$text = $encoder->decode($_text, Encode::FB_DEFAULT);
    }
    $_text = undef;

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

    $text = _RunBlockGamut($text, 1);

    # Unhashify code blocks
    $text =~ s/(\005\d+\006)/$g_code_blocks{$1}/g;

    $text = _UnescapeSpecialChars($text);

    $text .= "\n" unless $text eq "";

    utf8::encode($text);
    if (defined($opt{h1}) && $opt{h1} ne "" && ref($_[0]) eq "HASH") {
	utf8::encode($opt{h1});
	${$_[0]}{h1} = $opt{h1}
    }
    return $text;
}


sub _HashBTCodeBlocks {
#
#   Process Markdown backticks (```) delimited code blocks
#
    my $text = shift;
    my $less_than_indent = $opt{indent_width} - 1;

    $text =~ s{
	    (?:(?<=\n)|\A)
		([ ]{0,$less_than_indent})``(`+)[ \t]*(?:([\w.+-]+)[ \t]*)?\n
	     ( # $4 = the code block -- one or more lines, starting with ```
	      (?:
		.*\n+
	      )+?
	     )
	    # and ending with ``` or end of document
	    (?:(?:[ ]{0,$less_than_indent}``\2[ \t]*(?:\n|\Z))|\Z)
	}{
	    # $2 contains syntax highlighting to use if defined
	    my $leadsp = length($1);
	    my $codeblock = $4;
	    $codeblock =~ s/[ \t]+$//mg; # trim trailing spaces on lines
	    $codeblock = _Detab($codeblock, 8); # physical tab stops are always 8
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace
	    $codeblock =~ s/^ {1,$leadsp}//mg if $leadsp; # trim leading space(s)
	    $codeblock = _EncodeCode($codeblock); # or run highlighter here
	    $codeblock = "<div class=\"$opt{style_prefix}code-bt\"><pre style=\"display:none\"></pre><pre><code>"
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
	my $id = _strip(lc $1); # Link IDs are case-insensitive
	my $url = $2;
	my $title = _strip($3);
	if ($id ne "") {
		$g_urls{$id} = _EncodeAmpsAndAngles($url);
		if (defined($title) && $title ne "") {
		    $g_titles{$id} = $title;
		    $g_titles{$id} =~ s/\042/&quot;/g;
		}
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
			<!--
			(?:[^-]|(?:-(?!-)))*
			-->
		    )
		    [ ]*
		    (?=\n{1,}|\Z)   # followed by end of line or end of document
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

    $text = _DoTables($text);

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


sub _ProcessWikiLink {
    my ($link_text, $link_loc) = @_;
    if (defined($link_loc) && $link_loc =~ m{^(?:http|ftp)s?://\S+$}i) {
	# Just rewrite it to [...](...) form
	return "[".$link_text."](".$link_loc.")";
    }
    if (defined($link_loc)) {
	# We don't handle any other kind of "bar" links yet
	return undef;
    }
    if ($link_text =~ m{^(?:http|ftp)s?://\S+$}i) {
	# Just rewrite it to [...](...) form
	return "[".$link_text."](".$link_text.")";
    }
    # We don't handle any other wiki-style links yet
    return undef;
}


sub _DoAnchors {
#
# Turn Markdown link shortcuts into XHTML <a> tags.
#
    my $text = shift;

    #
    # First, handle wiki-style links: [[wiki style link]]
    #
    $text =~ s{
	(		    # wrap whole match in $1
	  \[\[
	    ($g_nested_brackets) # link text and id = $2
	  \]\]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my $link_loc    = undef;

	if ($link_text =~ /^(.*)\|(.*)$/s) {
	    $link_text = $1;
	    $link_loc = $2;
	}

	$result = _ProcessWikiLink($link_text, $link_loc);
	defined($result) or $result = $whole_match;
	$result;
    }xsge;

    #
    # Next, handle reference-style links: [link text] [id]
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
	my $link_id	= _strip(lc $3);

	if ($link_id eq "") {
	    $link_id = _strip(lc $link_text);	  # for shortcut links like [this][].
	}

	if (defined($g_urls{$link_id}) || defined($g_anchors{$link_id})) {
	    my $url = $g_urls{$link_id};
	    $url = defined($url) ? _PrefixURL($url) : $g_anchors{$link_id};
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<a href=\"$url\"";
	    if ( defined $g_titles{$link_id} ) {
		my $title = _EncodeAttText($g_titles{$link_id});
		$result .=  " title=\"$title\"";
	    }
	    $link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	    $result .= ">$link_text</a>";
	}
	else {
	    $result = $whole_match;
	}
	$result;
    }xsge;

    #
    # Subsequently, inline-style links: [link text](url "optional title")
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
	my $title	= _EncodeAttText($6);

	$url = _PrefixURL($url);
	# We've got to encode these to avoid conflicting
	# with italics, bold and strike through.
	$url =~ s!([*_~])!$g_escape_table{$1}!g;
	$result = "<a href=\"$url\"";

	if (defined $title) {
	    $result .= " title=\"$title\"";
	}

	$link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	$result .= ">$link_text</a>";

	$result;
    }xsge;

    #
    # Finally, handle reference-style implicit shortcut links: [link text]
    #
    $text =~ s{
	(		    # wrap whole match in $1
	  \[
	    ($g_nested_brackets) # link text = $2
	  \]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my $link_id	= _strip(lc $2);

	if (defined($g_urls{$link_id}) || defined($g_anchors{$link_id})) {
	    my $url = $g_urls{$link_id};
	    $url = defined($url) ? _PrefixURL($url) : $g_anchors{$link_id};
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<a href=\"$url\"";
	    if ( defined $g_titles{$link_id} ) {
		my $title = _EncodeAttText($g_titles{$link_id});
		$result .=  " title=\"$title\"";
	    }
	    $link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	    $result .= ">$link_text</a>";
	}
	else {
	    $result = $whole_match;
	}
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
	my $alt_text	= _strip($2);
	my $link_id	= _strip(lc $3);

	if ($link_id eq "") {
	    $link_id = lc $alt_text; # for shortcut links like ![this][].
	}

	$alt_text = _EncodeAttText($alt_text);
	if (defined $g_urls{$link_id}) {
	    my $url = _PrefixURL($g_urls{$link_id});
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<img src=\"$url\" alt=\"$alt_text\"";
	    if (defined $g_titles{$link_id}) {
		my $title = _EncodeAttText($g_titles{$link_id});
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
	my $alt_text	= _EncodeAttText($2);
	my $url		= $3;
	my $title	= '';
	if (defined($6)) {
	    $title	= _EncodeAttText($6);
	}

	$url = _PrefixURL($url);
	# We've got to encode these to avoid conflicting
	# with italics, bold and strike through.
	$url =~ s!([*_~])!$g_escape_table{$1}!g;
	$result = "<img src=\"$url\" alt=\"$alt_text\"";
	if (defined $title) {
	    $result .= " title=\"$title\"";
	}
	$result .= $opt{empty_element_suffix};

	$result;
    }xsge;

    #
    # Finally, handle reference-style implicitly labeled links: ![alt text]
    #
    $text =~ s{
	(		# wrap whole match in $1
	  !\[
	    (.*?)	# alt text = $2
	  \]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $alt_text	= _strip($2);
	my $link_id	= lc $alt_text;

	$alt_text = _EncodeAttText($alt_text);
	if (defined $g_urls{$link_id}) {
	    my $url = _PrefixURL($g_urls{$link_id});
	    # We've got to encode these to avoid conflicting
	    # with italics, bold and strike through.
	    $url =~ s!([*_~])!$g_escape_table{$1}!g;
	    $result = "<img src=\"$url\" alt=\"$alt_text\"";
	    if (defined $g_titles{$link_id}) {
		my $title = _EncodeAttText($g_titles{$link_id});
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

    return $text;
}

sub _EncodeAttText {
    my $text = shift;
    defined($text) or return undef;
    $text = _EncodeAmps(_strip($text));
    $text =~ s/\042/&quot;/g;
    $text =~ s/</&lt;/g;
    $text =~ s!([*_~:])!$g_escape_table{$1}!g;
    return $text;
}


sub _MakeAnchorId {
    use bytes;
    my $link = shift;
    $link =~ tr/-a-z0-9_/_/cs;
    return '' unless $link ne '';
    $link = md5_hex($link) if length($link) > 64;
    "_".$link."_";
}


sub _GetNewAnchorId {
    my $link = _strip(lc(shift));
    return '' if defined($g_anchors{$link});
    my $id = _MakeAnchorId($link);
    return '' unless $id;
    $g_anchors{$link} = '#'.$id;
    $id;
}


sub _DoHeaders {
    my ($text, $anchors) = @_;
    my $h1;
    my $geth1 = $anchors && !defined($opt{h1}) ? sub {
	return unless !defined($h1);
	my $h = shift;
	$h =~ s/^\s+//;
	$h =~ s/\s+$//;
	$h =~ s/\s+/ /g;
	$h1 = $h if $h ne "";
    } : sub {};

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
    $text =~ s{ ^(?:=+[ ]*\n)?[ ]*(.+?)[ ]*\n=+[ ]*\n+ }{
	my $h = $1;
	my $id = _GetNewAnchorId($h);
	&$geth1($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h1$id>" . _RunSpanGamut($h) . "</h1>\n\n";
    }egmx;

    $text =~ s{ ^(?:-+[ ]*\n)?[ ]*(.+?)[ ]*\n-+[ ]*\n+ }{
	my $h = $1;
	my $id = _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h2$id>" . _RunSpanGamut($h) . "</h2>\n\n";
    }egmx;

    $text =~ s{ ^(?:~+[ ]*\n)?[ ]*(.+?)[ ]*\n~+[ ]*\n+ }{
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
	    &$geth1($h) if $h_level == 1;
	    $id = " id=\"$id\"" if $id ne "";
	    "<h$h_level$id>" . _RunSpanGamut($h) . "</h$h_level>\n\n";
	}egmx;

    $opt{h1} = $h1 if defined($h1) && $h1 ne "";
    return $text;
}


my ($marker_ul, $marker_ol, $marker_any, $roman_numeral, $greek_lower);
BEGIN {
    # Re-usable patterns to match list item bullets and number markers:
    $roman_numeral = qr/(?:
	[IiVvXx]|[Ii]{2,3}|[Ii][VvXx]|[VvXx][Ii]{1,3}|[Xx][Vv][Ii]{0,3}|
	[Xx][Ii][VvXx]|[Xx]{2}[Ii]{0,3}|[Xx]{2}[Ii]?[Vv]|[Xx]{2}[Vv][Ii]{1,2})/ox;
    $greek_lower = qr/(?:[\x{03b1}-\x{03c9}])/o;
    $marker_ul  = qr/[*+-]/o;
    $marker_ol  = qr/(?:\d+|[A-Za-z]|$roman_numeral|$greek_lower)[.\)]/o;
    $marker_any = qr/(?:$marker_ul|$marker_ol)/o;
}


sub _GetListMarkerType {
    my ($list_type, $list_marker, $last_marker) = @_;
    return "" unless $list_type && $list_marker && lc($list_type) eq "ol";
    my $last_marker_type = '';
    $last_marker_type = _GetListMarkerType($list_type, $last_marker)
	if defined($last_marker) &&
	    # these are roman unless $last_marker type case matches and is 'a' or 'A'
	    $list_marker =~ /^[IiVvXx][.\)]?$/;
    return "I" if $list_marker =~ /^[IVX]/ && $last_marker_type ne 'A';
    return "i" if $list_marker =~ /^[ivx]/ && $last_marker_type ne 'a';
    return "A" if $list_marker =~ /^[A-Z]/;
    return "a" if $list_marker =~ /^[a-z]/ || $list_marker =~ /^$greek_lower/o;
    return "1";
}


sub _GetListItemTypeClass {
    my ($list_type, $list_marker, $last_marker) = @_;
    my $list_marker_type = _GetListMarkerType($list_type, $list_marker, $last_marker);
    my $ans = &{sub{
	return "" unless length($list_marker) >= 2 && $list_marker_type =~ /^[IiAa1]$/;
	return "lower-greek" if $list_marker_type eq "a" && $list_marker =~ /^$greek_lower/o;
	return "" unless $list_marker =~ /\)$/;
	return "upper-roman" if $list_marker_type eq "I";
	return "lower-roman" if $list_marker_type eq "i";
	return "upper-alpha" if $list_marker_type eq "A";
	return "lower-alpha" if $list_marker_type eq "a";
	return "decimal";
    }};
    return ($list_marker_type, $ans);
}


my %_roman_number_table;
BEGIN {
    %_roman_number_table = (
	i	=>  1,
	ii	=>  2,
	iii	=>  3,
	iv	=>  4,
	v	=>  5,
	vi	=>  6,
	vii	=>  7,
	viii	=>  8,
	ix	=>  9,
	x	=> 10,
	xi	=> 11,
	xii	=> 12,
	xiii	=> 13,
	xiv	=> 14,
	xv	=> 15,
	xvi	=> 16,
	xvii	=> 17,
	xviii	=> 18,
	xix	=> 19,
	xx	=> 20,
	xxi	=> 21,
	xxii	=> 22,
	xxiii	=> 23,
	xxiv	=> 24,
	xxv	=> 25,
	xxvi	=> 26,
	xxvii	=> 27
    );
}


# Necessary because ς and σ are the same value grrr
my %_greek_number_table;
BEGIN {
    %_greek_number_table = (
	"\x{03b1}" =>  1, # α
	"\x{03b2}" =>  2, # β
	"\x{03b3}" =>  3, # γ
	"\x{03b4}" =>  4, # δ
	"\x{03b5}" =>  5, # ε
	"\x{03b6}" =>  6, # ζ
	"\x{03b7}" =>  7, # η
	"\x{03b8}" =>  8, # θ
	"\x{03b9}" =>  9, # ι
	"\x{03ba}" => 10, # κ
	"\x{03bb}" => 11, # λ
	#"\x{00b5}"=> 12, # µ is "micro" not "mu"
	"\x{03bc}" => 12, # μ
	"\x{03bd}" => 13, # ν
	"\x{03be}" => 14, # ξ
	"\x{03bf}" => 15, # ο
	"\x{03c0}" => 16, # π
	"\x{03c1}" => 17, # ρ
	"\x{03c2}" => 18, # ς
	"\x{03c3}" => 18, # σ
	"\x{03c4}" => 19, # τ
	"\x{03c5}" => 20, # υ
	"\x{03c6}" => 21, # φ
	"\x{03c7}" => 22, # χ
	"\x{03c8}" => 23, # ψ
	"\x{03c9}" => 24  # ω
    );
}


sub _GetMarkerIntegerNum {
    my ($list_marker_type, $marker_val) = @_;
    my $ans = &{sub{
	return 0 + $marker_val if $list_marker_type eq "1";
	$list_marker_type = lc($list_marker_type);
	return $_greek_number_table{$marker_val}
	    if $list_marker_type eq "a" &&
	    defined($_greek_number_table{$marker_val});
	$marker_val = lc($marker_val);
	return ord($marker_val) - ord("a") + 1 if $list_marker_type eq "a";
	return 1 unless $list_marker_type eq "i";
	defined($_roman_number_table{$marker_val}) and
	    return $_roman_number_table{$marker_val};
	return 1;
    }};
    return $ans if $ans == 0 && $list_marker_type eq "1";
    return $ans >= 1 ? $ans : 1;
}


sub _IncrList {
    my ($from, $to, $extra) = @_;
    $extra = defined($extra) ? " $extra" : "";
    my $result = "";
    while ($from + 10 <= $to) {
	$result .= "<span$extra class=\"$opt{style_prefix}ol-incr-10\"></span>\n";
	$from += 10;
    }
    while ($from + 5 <= $to) {
	$result .= "<span$extra class=\"$opt{style_prefix}ol-incr-5\"></span>\n";
	$from += 5;
    }
    while ($from + 2 <= $to) {
	$result .= "<span$extra class=\"$opt{style_prefix}ol-incr-2\"></span>\n";
	$from += 2;
    }
    while ($from < $to) {
	$result .= "<span$extra class=\"$opt{style_prefix}ol-incr\"></span>\n";
	++$from;
    }
    return $result;
}


sub _DoLists {
#
# Form HTML ordered (numbered) and unordered (bulleted) lists.
#
    my $text = shift;
    my $indent = $opt{indent_width};
    my $less_than_indent = $indent - 1;
    my $less_than_double_indent = 2 * $indent - 1;

    # Re-usable pattern to match any entire ul or ol list:
    my $whole_list = qr{
	(			    # $1 (or $_[0]) = whole list
	  (			    # $2 (or $_[1])
	    (?:(?<=\n)|\A)
	    [ ]{0,$less_than_indent}
	    (${marker_any})	    # $3 (or $_[2]) = first list item marker
	    [ ]+
	  )
	  (?s:.+?)
	  (			    # $4 (or $_[3])
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

    my $list_item_sub = sub {
	my $list = $_[0];
	my $list_type = ($_[2] =~ m/$marker_ul/) ? "ul" : "ol";
	my $list_att = "";
	my $list_class = "";
	my $list_incr = "";
	# Turn double returns into triple returns, so that we can make a
	# paragraph for the last item in a list, if necessary:
	$list =~ s/\n\n/\n\n\n/g;
	my ($result, $first_marker, $fancy) = _ProcessListItems($list_type, $list);
	my $list_marker_type = _GetListMarkerType($list_type, $first_marker);
	if ($list_marker_type) {
		$first_marker =~ s/[.\)]$//;
		my $first_marker_num = _GetMarkerIntegerNum($list_marker_type, $first_marker);
		$list_att = $list_marker_type eq "1" ? "" : " type=\"$list_marker_type\"";
		if ($fancy) {
		    $list_class = " class=\"$opt{style_prefix}ol\"";
		    my $start = $first_marker_num;
		    $start = 10 if $start > 10;
		    $start = 5 if $start > 5 && $start < 10;
		    $start = 1 if $start > 1 && $start < 5;
		    $list_att .= " start=\"$start\"" unless $start == 1;
		    $list_incr = _IncrList($start, $first_marker_num);
		} else {
		    $list_class = " class=\"$opt{style_prefix}lc-greek\""
			if $list_marker_type eq "a" && $first_marker =~ /^$greek_lower/o;
		    $list_att .= " start=\"$first_marker_num\"" unless $first_marker_num == 1;
		}
	}
	$result = "<$list_type$list_att$list_class>\n$list_incr" . $result . "</$list_type>\n";
	$result;
    };

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
    # different now, but the duplication has pretty much been eliminated via
    # use of a separate sub which has the side-effect of making the below
    # two cases much easier to grok all at once.

    if ($g_list_level) {
	$text =~ s{
		^
		$whole_list
	    }{
		&$list_item_sub($1, $2, $3, $4);
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
		&$list_item_sub($1, $2, $3, $4);
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
    my $marker_kind = $list_type eq "ul" ? $marker_ul : $marker_ol;
    my $first_marker;
    my $first_marker_type;
    my $first_marker_num;
    my $last_marker;
    my $fancy;
    my $skipped;
    my $typechanged;
    my $next_num = 1;

    # trim trailing blank lines:
    $list_str =~ s/\n{2,}\z/\n/;

    my $result = "";
    my $oldpos = 0;
    pos($list_str) = 0;
    while ($list_str =~ m{\G		# start where we left off
	(\n+)?				# leading line = $1
	(^[ ]*)				# leading whitespace = $2
	($marker_any) [ ] ([ ]*)	# list marker = $3 leading item space = $4
    }cgmx) {
	my $leading_line = $1;
	my $leading_space = $2;
	my $list_marker = $3;
	my $list_marker_len = length($list_marker);
	my $leading_item_space = $4;
	if ($-[0] > $oldpos) {
	    $result .= substr($list_str, $oldpos, $-[0] - $oldpos); # Sort-of $`
	    $oldpos = $-[0]; # point at start of this entire match
	}
	if (!defined($first_marker)) {
	    $first_marker = $list_marker;
	    $first_marker_type = _GetListMarkerType($list_type, $first_marker);
	    if ($first_marker_type) {
		(my $marker_val = $first_marker) =~ s/[.\)]$//;
		$first_marker_num = _GetMarkerIntegerNum($first_marker_type, $marker_val);
		$next_num = $first_marker_num;
		$skipped = 1 if $next_num != 1;
	    }
	} elsif ($list_marker !~ /$marker_kind/) {
	    # Wrong marker kind, "fix up" the marker to a correct "lazy" marker
	    # But keep the old length in $list_marker_len
	    $list_marker = $last_marker;
	}

	# Now grab the rest of this item's data upto but excluding the next
	# list marker at the SAME indent level, but sublists must be INCLUDED

	my $item = "";
	while ($list_str =~ m{\G
	    ((?:.+?)(?:\n{1,2}))	# list item text = $1
	    (?= \n* (?: \z |		# end of string OR
		    (^[ ]*)		# leading whitespace = $2
		    ($marker_any)	# next list marker = $3
		    ([ ]+) ))		# one or more spaces after marker = $4
	}cgmxs) {

	    # If $3 has a left edge that is at the left edge of the previous
	    # marker OR $3 has a right edge that is at the right edge of the
	    # previous marker then we stop; otherwise we go on

	    $item .= substr($list_str, $-[0], $+[0] - $-[0]); # $&
	    last if !defined($4) || length($2) == length($leading_space) ||
		length($2) + length($3) == length($leading_space) + $list_marker_len;
	    # move along, you're not the marker droid we're looking for...
	    $item .= substr($list_str, $+[0], $+[4] - $+[0]);
	    pos($list_str) = $+[4]; # ...move along over the marker droid
	}
	# Remember where we parked
	$oldpos = pos($list_str);

	# Process the $list_marker $item

	my $liatt = '';
	my $checkbox = '';
	my $incr = '';

	if ($list_type eq "ul" && !$leading_item_space && $item =~ /^\[([ xX])\] +(.*)$/s) {
	    my $checkmark = lc $1;
	    $item = $2;
	    my ($checkbox_class, $checkbox_val);
	    if ($checkmark eq "x") {
		($checkbox_class, $checkbox_val) = ("checkbox-on", "x");
	    } else {
		($checkbox_class, $checkbox_val) = ("checkbox-off", "&#160;");
	    }
	    $liatt = " class=\"$opt{style_prefix}$checkbox_class\"";
	    $checkbox = "<span><span></span></span><span></span><span>[<tt>$checkbox_val</tt>]&#160;</span>";
	} else {
	    my $list_marker_type;
	    ($list_marker_type, $liatt) = _GetListItemTypeClass($list_type, $list_marker, $last_marker);
	    if ($list_type eq "ol" && defined($first_marker)) {
		my $styled = $fancy = 1 if $liatt && $list_marker =~ /\)$/;
		my ($sfx, $dash) = ("", "");
		($sfx, $dash) = ("li", "-") if $styled;
		if ($liatt =~ /lower/) {
		    $sfx .= "${dash}lc";
		} elsif ($liatt =~ /upper/) {
		    $sfx .= "${dash}uc";
		}
		$sfx .= "-greek" if $liatt =~ /greek/;
		$liatt = " class=\"$opt{style_prefix}$sfx\"" if $sfx;
		$typechanged = 1 if $list_marker_type ne $first_marker_type;
		(my $marker_val = $list_marker) =~ s/[.\)]$//;
		my $marker_num = _GetMarkerIntegerNum($list_marker_type, $marker_val);
		$marker_num = $next_num if $marker_num < $next_num;
		$skipped = 1 if $next_num < $marker_num;
		$incr = _IncrList($next_num, $marker_num, "incrlevel=$g_list_level");
		$liatt = " value=\"$marker_num\"$liatt" if $fancy || $skipped;
		$liatt = " type=\"$list_marker_type\"$liatt" if $styled || $typechanged;
		$next_num = $marker_num + 1;
	    }
	}
	$last_marker = $list_marker;

	if ($leading_line or ($item =~ m/\n{2,}/)) {
	    $item = _RunBlockGamut(_Outdent($item));
	}
	else {
	    # Recursion for sub-lists:
	    $item = _DoLists(_Outdent($item));
	    chomp $item;
	    $item = _RunSpanGamut($item);
	}

	# Append to $result
	$result .= "$incr<li$liatt>" . $checkbox . $item . "</li>\n";
    }
    if ($fancy) {
	# remove "incrlevel=$g_list_level " parts
	$result =~ s{<span incrlevel=$g_list_level class="$opt{style_prefix}ol-incr((?:-\d{1,2})?)">}
	    {<span class="$opt{style_prefix}ol-incr$1">}g;
    } else {
	# remove the $g_list_level incr spans entirely
	$result =~ s{<span incrlevel=$g_list_level class="$opt{style_prefix}ol-incr(?:-\d{1,2})?"></span>\n}{}g;
	# remove the class="$opt{style_prefix}lc-greek" if first_marker is greek
	$result =~ s{(<li[^>]*?) class="$opt{style_prefix}lc-greek">}{$1>}g
	    if defined($first_marker_type) && $first_marker_type eq "a" && $first_marker =~ /^$greek_lower/o;
    }

    # Anything left over (similar to $') goes into result, but this should always be empty
    $result .= _RunBlockGamut(substr($list_str, pos($list_str)));

    $g_list_level--;
    return ($result, $first_marker, $fancy);
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

	    my $result = "<div class=\"$opt{style_prefix}code\"><pre style=\"display:none\"></pre><pre><code>"
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


my ($LEAD, $TRAIL, $LEADBAR, $LEADSP, $COLPL, $SEP);
BEGIN {
    $LEAD = qr/(?>[ ]*(?:\|[ ]*)?)/o;
    $TRAIL = qr/\|[ ]*/o;
    $LEADBAR = qr/(?>[ ]*\|[ ]*)/o;
    $LEADSP = qr/(?>[ ]*)/o;
    $COLPL = qr/(?:[^\n|\\]|\\[^\n])+/o;
    $SEP = qr/[ ]*:?-+:?[ ]*/o;
}

sub _DoTables {
    my $text = shift;

    $text =~ s{
	(				# Wrap whole thing to avoid $&
	 (?: (?<=\n\n) | \A\n? )	# Preceded by blank line or beginning of string
	 ^(				# Header line
	    $LEADBAR \| [^\n]* |
	    $LEADBAR $COLPL [^\n]* |
	    $LEADSP $COLPL \| [^\n]*
	  )\n
	  (				# Separator line
	    $LEADBAR $SEP (?: \| $SEP )* (?: \| [ ]*)? |
	    $SEP (?: \| $SEP )+ (?: \| [ ]*)? |
	    $SEP \| [ ]*
	  )\n
	  ((?:				# Rows (0+)
	    $LEADBAR \| [^\n]* \n |
	    $LEADBAR $COLPL [^\n]* \n |
	    $LEADSP $COLPL \| [^\n]* \n
	  )*)
	)
    } {
	my ($w, $h, $s, $rows) = ($1, $2, $3, $4);
	my @heads = _SplitTableRow($h);
	my @seps = _SplitTableRow($s);
	if (@heads == @seps) {
	    my @align = map {
		if (/^:-+:$/) {" align=\"center\""}
		elsif (/^:/) {" align=\"left\""}
		elsif (/:$/) {" align=\"right\""}
		else {""}
	    } @seps;
	    my $headers = _MakeTableRow("th", \@align, @heads);
	    my $tab ="\n<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\" class=\"$opt{style_prefix}table\">\n" .
		"  <tr class=\"$opt{style_prefix}row-hdr\">" . _MakeTableRow("th", \@align, @heads) . "</tr>\n";
	    my $cnt = 0;
	    my @classes = ("class=\"$opt{style_prefix}row-even\"", "class=\"$opt{style_prefix}row-odd\"");
	    $tab .= "  <tr " . $classes[++$cnt % 2] . ">" . _MakeTableRow("td", \@align, _SplitTableRow($_)) . "</tr>\n"
		    foreach split(/\n/, $rows);
	    $tab .= "</table>\n\n";
	} else {
	    $w;
	}
    }egmx;

    return $text;
}


sub _SplitTableRow {
    my $row = shift;
    $row =~ s/^$LEAD//;
    $row =~ s/$TRAIL$//;
    $row =~ s!\\\\!$g_escape_table{'\\'}!go; # Must process escaped backslashes first.
    $row =~ s!\\\|!$g_escape_table{'|'}!go; # Then do \|
    my @elems = map {
      s!$g_escape_table{'|'}!|!go;
      s!$g_escape_table{'\\'}!\\!go;
      s/^[ ]+//;
      s/[ ]+$//;
      $_;
    } split(/[ ]*\|[ ]*/, $row, -1);
    @elems or push(@elems, "");
    return @elems;
}


sub _MakeTableRow {
    my $etype = shift;
    my $align = shift;
    my $row = "";
    for (my $i = 0; $i < @$align; ++$i) {
	my $data = $_[$i];
	defined($data) or $data = "";
	$row .= "<" . $etype . $$align[$i] . ">" .
	    _RunSpanGamut($data) . "</" . $etype . ">";
    }
    return $row;
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


my $g_possible_tag_name;
my %ok_tag_name;
BEGIN {
    # note: length("blockquote") == 10
    $g_possible_tag_name = qr/(?i:[a-z]{1,10}|h[1-6])/o;
    %ok_tag_name = map({$_ => 1} qw(
	a abbr acronym address
	b basefont bdo big blockquote br
	caption center cite code col colgroup
	dd del dfn div dl dt
	em
	font
	h1 h2 h3 h4 h5 h6 hr
	i img ins
	kbd
	li
	ol
	p pre
	q
	s samp small span strike strong sub sup
	table tbody td tfoot th thead tr tt
	u ul
	var
    ));
    $ok_tag_name{$_} = 0 foreach (qw(
	dir menu
    ));
}


sub _SetAllowedTag {
	my ($tag, $forbid) = @_;
	$ok_tag_name{$tag} = $forbid ? 0 : 1
		if defined($tag) && exists($ok_tag_name{$tag});
}


# Encode leading '<' of any non-tags
# However, "<?", "<!" and "<$" are passed through (legacy on that "<$" thing)
sub _DoTag {
	my $tag = shift;
	return $tag if $tag =~ /^<[?\$!]/;
	if (($tag =~ m{^<($g_possible_tag_name)(?:[\s>]|/>$)} || $tag =~ m{^</($g_possible_tag_name)\s*>}) &&
	    $ok_tag_name{lc($1)}) {

	    return _ProcessURLTag("href", $tag) if $tag =~ /^<a\s/i;
	    return _ProcessURLTag("src", $tag) if $tag =~ /^<img\s/i;
	    return $tag;
	}
	$tag =~ s/</&lt;/g;
	return $tag;
}


sub _ProcessURLTag {
    my $att = shift;
    my $tag = shift;

    $att = lc($att) . "=";
    if ($tag =~ /^(<[^\s>]+\s+)/g) {
	    my $out = $1;
	    while ($tag =~ /\G([^\s\042\047>]+=)([\042\047])((?:(?!\2)(?!>).)*)(\2\s*)/gc) {
		    my ($p, $q, $v, $s) = ($1, $2, $3, $4);
		    if (lc($p) eq $att && $v ne "") {
			    $v = _HTMLEncode(_PrefixURL($v));
		    }
		    $out .= $p . $q . $v . $s;
	    }
	    $out .= substr($tag, pos($tag));
	    return $out;
    }

    return $tag;
}


sub _HTMLEncode {
    my $text = shift;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    # Remaining entities now
    $text =~ s/\042/&quot;/g;
    $text =~ s/\047/&apos;/g;
    $text =~ s/</&lt;/g;
    $text =~ s/>/&gt;/g;

    return $text;
}


sub _EncodeAmps {
    my $text = shift;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    return $text;
}


sub _EncodeAmpsAndAngles {
# Smart processing for ampersands and angle brackets that need to be encoded.

    my $text = shift;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    # Encode naked <'s
    $text =~ s{<(?![a-z/?\$!])}{&lt;}gi;
    $text =~ s{<(?=[^>]*$)}{&lt;}g;

    # Encode <'s that cannot possibly be a start or end tag
    $text =~ s{(<[^>]*>)}{_DoTag($1)}ige;

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
    $url =~ s/^\s+//;
    $url =~ s/\s+$//;
    $url = "#" unless $url ne "";

    return $url unless $opt{url_prefix} ne '' || $opt{img_prefix} ne '';
    return $url if $url =~ m"^\002\003" || $url =~ m"^#" ||
	    $url =~ m,^//, || $url =~ /^[A-Za-z][A-Za-z0-9+.-]*:/;
    my $ans = $opt{url_prefix};
    $ans = $opt{img_prefix}
	if $opt{img_prefix} ne '' && $url =~ m"^[^#]*\.(?:png|gif|jpe?g|svgz?)(?:#|$)"i;
    return $url unless $ans ne '';
    $ans .= '/' if substr($ans, -1, 1) ne '/';
    $ans .= substr($url, 0, 1) eq '/' ? substr($url, 1) : $url;
    return "\2\3".$ans;
}


BEGIN {
    $g_style_sheet = <<'STYLESHEET';

<style type="text/css">
/* <![CDATA[ */

/* Markdown.pl fancy style sheet
** Copyright (C) 2017,2018 Kyle J. McKay.
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
	margin: 0;
	padding: 0;
	overflow: auto;
}

div.%(base)code-bt > pre > code, div.%(base)code > pre > code {
	display: inline-block;
	margin: 0;
	padding: 0.5em 0;
	border-top: thin dotted;
	border-bottom: thin dotted;
}

table.%(base)table {
	margin-bottom: 0.5em;
}
table.%(base)table, table.%(base)table th, table.%(base)table td {
	border-collapse: collapse;
	border-spacing: 0;
	border: thin solid;
}

ol.%(base)ol {
	counter-reset: %(base)item;
}
ol.%(base)ol[start="0"] {
	counter-reset: %(base)item -1;
}
ol.%(base)ol[start="5"] {
	counter-reset: %(base)item 4;
}
ol.%(base)ol[start="10"] {
	counter-reset: %(base)item 9;
}
ol.%(base)ol > span.%(base)ol-incr {
	counter-increment: %(base)item;
}
ol.%(base)ol > span.%(base)ol-incr-2 {
	counter-increment: %(base)item 2;
}
ol.%(base)ol > span.%(base)ol-incr-5 {
	counter-increment: %(base)item 5;
}
ol.%(base)ol > span.%(base)ol-incr-10 {
	counter-increment: %(base)item 10;
}
ol.%(base)lc-greek, li.%(base)lc-greek {
	list-style-type: lower-greek;
}
ol.%(base)ol > li {
	counter-increment: %(base)item;
}
ol.%(base)ol > li.%(base)li,
ol.%(base)ol > li.%(base)li-lc,
ol.%(base)ol > li.%(base)li-lc-greek,
ol.%(base)ol > li.%(base)li-uc {
	list-style-type: none;
	display: block;
}
ol.%(base)ol > li.%(base)li:before,
ol.%(base)ol > li.%(base)li-lc:before,
ol.%(base)ol > li.%(base)li-lc-greek:before,
ol.%(base)ol > li.%(base)li-uc:before {
	position: absolute;
	text-align: right;
	white-space: nowrap;
	margin-left: -9ex;
	width: 9ex;
}
ol.%(base)ol > li.%(base)li[type="1"]:before {
	content: counter(%(base)item, decimal) ")\A0 \A0 ";
}
ol.%(base)ol > li.%(base)li-lc[type="i"]:before,
ol.%(base)ol > li.%(base)li-lc[type="I"]:before {
	content: counter(%(base)item, lower-roman) ")\A0 \A0 ";
}
ol.%(base)ol > li.%(base)li-uc[type="I"]:before,
ol.%(base)ol > li.%(base)li-uc[type="i"]:before {
	content: counter(%(base)item, upper-roman) ")\A0 \A0 ";
}
ol.%(base)ol > li.%(base)li-lc[type="a"]:before,
ol.%(base)ol > li.%(base)li-lc[type="A"]:before {
	content: counter(%(base)item, lower-alpha) ")\A0 \A0 ";
}
ol.%(base)ol > li.%(base)li-lc-greek[type="a"]:before,
ol.%(base)ol > li.%(base)li-lc-greek[type="A"]:before {
	content: counter(%(base)item, lower-greek) ")\A0 \A0 ";
}
ol.%(base)ol > li.%(base)li-uc[type="A"]:before,
ol.%(base)ol > li.%(base)li-uc[type="a"]:before {
	content: counter(%(base)item, upper-alpha) ")\A0 \A0 ";
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
    [B<--tabwidth>=I<num>] [B<--stylesheet>] [B<--stub>] [--]
    [I<file>...]

 Options:
   -h                                   show short usage help
   --help                               show long detailed help
   --html4tags                          use <br> instead of <br />
   --deprecated                         allow <dir> and <menu> tags
   --tabwidth=num                       expand tabs to num instead of 8
   -r prefix | --htmlroot=prefix        append relative non-img URLs
                                        to prefix
   -i prefix | --imageroot=prefix	append relative img URLs to
                                        prefix
   -V | --version                       show version, authors, license
                                        and copyright
   -s | --shortversion                  show just the version number
   --stylesheet                         output the fancy style sheet
   --no-stylesheet                      do not output fancy style sheet
   --stub                               wrap output in stub document
                                        implies --stylesheet
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

Input (auto-detected) may be either ISO-8859-1 or UTF-8.  Output is always
converted to the UTF-8 character set.


=head1 OPTIONS

Use "--" to end switch parsing. For example, to open a file named "-z", use:

    Markdown.pl -- -z

=over


=item B<--html4tags>

Use HTML 4 style for empty element tags, e.g.:

    <br>

instead of Markdown's default XHTML style tags, e.g.:

    <br />


=item B<--deprecated>

Both "<dir>" and "<menu>" are normally taken as literal text and the leading
"<" will be automatically escaped.

If this option is used, they are recognized as valid tags and passed through
without being escaped.

When dealing with program argument descriptions "<dir>" can be particularly
problematic therefore use of this option is not recommended.

Other deprecated tags (such as "<font>" and "<center>" for example) continue
to be recognized and passed through even without using this option.


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

Include the fancy style sheet at the beginning of the output (or in the
C<head> section with B<--stub>).  This style sheet makes fancy checkboxes
and makes a right parenthesis C<)> show instead of a C<.> for ordered lists
that use them.  Without it things will still look fine except that the
fancy stuff won't be there.

Use this option with no other arguments and redirect standard input to
/dev/null to get just the style sheet and nothing else.


=item B<--no-stylesheet>

Overrides a previous B<--stylesheet> and disables implicit inclusion
of the style sheet by the B<--stub> option.


=item B<--stub>

Wrap the output in a full document stub (i.e. has C<html>, C<head> and C<body>
tags).  The style sheet I<will> be included in the C<head> section unless the
B<--no-stylesheet> option is also used.


=item B<-h>, B<--help>

Display Markdown's help.  With B<--help> full help is shown, with B<-h> only
the usage and options are shown.


=back


=head1 VERSION HISTORY

Z<> See the F<README> file for detailed release notes for this version.

=over

=item Z<> 1.1.7 - 14 Feb 2018

=item Z<> 1.1.6 - 03 Jan 2018

=item Z<> 1.1.5 - 07 Dec 2017

=item Z<> 1.1.4 - 24 Jun 2017

=item Z<> 1.1.3 - 13 Feb 2017

=item Z<> 1.1.2 - 19 Jan 2017

=item Z<> 1.1.1 - 12 Jan 2017

=item Z<> 1.1.0 - 11 Jan 2017

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

=item Copyright (C) 2015-2018 Kyle J. McKay

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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut
