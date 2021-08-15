#!/usr/bin/env perl

#
# Markdown -- A text-to-HTML conversion tool for web writers
#
# Copyright (C) 2004 John Gruber
# Copyright (C) 2015,2016,2017,2018,2019,2020,2021 Kyle J. McKay
# All rights reserved.
# License is Modified BSD (aka 3-clause BSD) License\n";
# See LICENSE file (or <https://opensource.org/licenses/BSD-3-Clause>)
#

package Markdown;

use 5.008;
use strict;
use warnings;

use Encode;

use vars qw($COPYRIGHT $DATE $VERSION @ISA @EXPORT_OK);

BEGIN {*COPYRIGHT =
\"Copyright (C) 2004 John Gruber
Copyright (C) 2015,2016,2017,2018,2019,2020,2021 Kyle J. McKay
All rights reserved.
";
*VERSION = \"1.1.15";
*DATE = \"2021-08-15";
}

use Exporter ();
use Digest::MD5 qw(md5 md5_hex);
use File::Basename qw(basename);
use Scalar::Util qw(refaddr looks_like_number);
my ($hasxml, $hasxml_err); BEGIN { ($hasxml, $hasxml_err) = (0, "") }
my ($hasxmlp, $hasxmlp_err); BEGIN { ($hasxmlp, $hasxmlp_err) = (0, "") }
BEGIN {
@ISA = qw(Exporter);
@EXPORT_OK = qw(Markdown ProcessRaw GenerateStyleSheet SetWikiOpts SplitURL
		escapeXML unescapeXML ResolveFragment ConvertNamedCharacterEntities);
$INC{__PACKAGE__.'.pm'} = $INC{basename(__FILE__)} unless exists $INC{__PACKAGE__.'.pm'};
}

close(DATA) if fileno(DATA);
exit(&_main(@ARGV)||0) unless caller;

sub fauxdie($) {
    my $msg = join(" ", @_);
    $msg =~ s/\s+$//os;
    printf STDERR "%s: fatal: %s\n", basename($0), $msg;
    exit 1;
}

my $encoder;
BEGIN {
	$encoder = Encode::find_encoding('Windows-1252') ||
		   Encode::find_encoding('ISO-8859-1') or
		   die "failed to load ISO-8859-1 encoder\n";
}

#
# Global default settings:
#
my ($g_style_prefix, $g_empty_element_suffix, $g_indent_width,
    $g_start_p, $g_close_p);
BEGIN {
    $g_style_prefix = "_markdown-";	# Prefix for markdown css class styles
    $g_empty_element_suffix = " />";	# Change to ">" for HTML output
    $g_indent_width = 4;		# Number of spaces considered new level
    $g_start_p = "<p>";			# _FormParagraphs open paragraph tag
    $g_close_p = "</p>";		# _FormParagraphs close paragraph tag
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
my %g_anchors_id;
my %g_block_ids;
my %g_code_block_ids;
my %g_html_blocks;
my %g_code_blocks;
my @g_xml_comments;
my %opt;
my @autonum;

# Return a "block id" to use to identify the block that does not contain
# any characters that could be misinterpreted by the rest of the code
# Originally this used md5_hex but that's unnecessarily slow
# Instead just use the refaddr of the scalar ref of the entry for that
# key in either the global or, if the optional second argument is true,
# permanent table.  To avoid the result being confused with anything
# else, it's prefixed with a control character and suffixed with another
# both of which are not allowed by the XML standard or Unicode.
sub block_id {
    $_[1] or return "\5".refaddr(\$g_block_ids{$_[0]})."\6";
    $_[1] == 1 and return "\2".refaddr(\$g_perm_block_ids{$_[0]})."\3";
    $_[1] == 2 and return "\25".refaddr(\$g_code_block_ids{$_[0]})."\26";
    die "programmer error: bad block_id type $_[1]";
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

# Regex to match balanced (parentheses)
my $g_nested_parens;
BEGIN {
    $g_nested_parens = qr{
    (?>					# Atomic matching
	[^\(\)]+			# Anything other than parentheses
     |
	\(
	    (??{ $g_nested_parens })	# Recursive set of nested parentheses
	\)
    )*
    }ox
}

# Table of hash values for escaped characters:
my %g_escape_table;
BEGIN {
    $g_escape_table{""} = "\2\3";
    foreach my $char (split //, "\\\`*_~{}[]()>#+-.!|:<") {
	$g_escape_table{$char} = block_id($char,1);
    }
}

# Used to track when we're inside an ordered or unordered list
# (see _ProcessListItems() for details):
my $g_list_level;
BEGIN {
    $g_list_level = 0;
}

# Entity conversion table
my %named_character_entity;
BEGIN { %named_character_entity = (
    'Aacute'   =>   '193',
    'aacute'   =>   '225',
    'Acirc'    =>   '194',
    'acirc'    =>   '226',
    'acute'    =>   '180',
    'AElig'    =>   '198',
    'aelig'    =>   '230',
    'Agrave'   =>   '192',
    'agrave'   =>   '224',
    'alefsym'  => 'x2135',
    'Alpha'    =>   '913',
    'alpha'    =>   '945',
    'and'      => 'x2227',
    'ang'      => 'x2220',
    'apos'     =>    '39',
    'Aring'    =>   '197',
    'aring'    =>   '229',
    'asymp'    => 'x2248',
    'Atilde'   =>   '195',
    'atilde'   =>   '227',
    'Auml'     =>   '196',
    'auml'     =>   '228',
    'bdquo'    => 'x201e',
    'Beta'     =>   '914',
    'beta'     =>   '946',
    'brvbar'   =>   '166',
    'bull'     => 'x2022',
    'cap'      => 'x2229',
    'Ccedil'   =>   '199',
    'ccedil'   =>   '231',
    'cedil'    =>   '184',
    'cent'     =>   '162',
    'Chi'      =>   '935',
    'chi'      =>   '967',
    'circ'     =>   '710',
    'clubs'    => 'x2663',
    'cong'     => 'x2245',
    'copy'     =>   '169',
    'crarr'    => 'x21b5',
    'cup'      => 'x222a',
    'curren'   =>   '164',
    'Dagger'   => 'x2021',
    'dagger'   => 'x2020',
    'dArr'     => 'x21d3',
    'darr'     => 'x2193',
    'deg'      =>   '176',
    'Delta'    =>   '916',
    'delta'    =>   '948',
    'diams'    => 'x2666',
    'divide'   =>   '247',
    'Eacute'   =>   '201',
    'eacute'   =>   '233',
    'Ecirc'    =>   '202',
    'ecirc'    =>   '234',
    'Egrave'   =>   '200',
    'egrave'   =>   '232',
    'empty'    => 'x2205',
    'emsp'     => 'x2003',
    'ensp'     => 'x2002',
    'Epsilon'  =>   '917',
    'epsilon'  =>   '949',
    'equiv'    => 'x2261',
    'Eta'      =>   '919',
    'eta'      =>   '951',
    'ETH'      =>   '208',
    'eth'      =>   '240',
    'Euml'     =>   '203',
    'euml'     =>   '235',
    'euro'     => 'x20ac',
    'exist'    => 'x2203',
    'fnof'     =>   '402',
    'forall'   => 'x2200',
    'frac12'   =>   '189',
    'frac14'   =>   '188',
    'frac34'   =>   '190',
    'frasl'    => 'x2044',
    'Gamma'    =>   '915',
    'gamma'    =>   '947',
    'ge'       => 'x2265',
    'hArr'     => 'x21d4',
    'harr'     => 'x2194',
    'hearts'   => 'x2665',
    'hellip'   => 'x2026',
    'Iacute'   =>   '205',
    'iacute'   =>   '237',
    'Icirc'    =>   '206',
    'icirc'    =>   '238',
    'iexcl'    =>   '161',
    'Igrave'   =>   '204',
    'igrave'   =>   '236',
    'image'    => 'x2111',
    'infin'    => 'x221e',
    'int'      => 'x222b',
    'Iota'     =>   '921',
    'iota'     =>   '953',
    'iquest'   =>   '191',
    'isin'     => 'x2208',
    'Iuml'     =>   '207',
    'iuml'     =>   '239',
    'Kappa'    =>   '922',
    'kappa'    =>   '954',
    'Lambda'   =>   '923',
    'lambda'   =>   '955',
    'lang'     => 'x2329',
    'laquo'    =>   '171',
    'lArr'     => 'x21d0',
    'larr'     => 'x2190',
    'lceil'    => 'x2308',
    'ldquo'    => 'x201c',
    'le'       => 'x2264',
    'lfloor'   => 'x230a',
    'lowast'   => 'x2217',
    'loz'      => 'x25ca',
    'lrm'      => 'x200e',
    'lsaquo'   => 'x2039',
    'lsquo'    => 'x2018',
    'macr'     =>   '175',
    'mdash'    => 'x2014',
    'micro'    =>   '181',
    'middot'   =>   '183',
    'minus'    => 'x2212',
    'Mu'       =>   '924',
    'mu'       =>   '956',
    'nabla'    => 'x2207',
    'nbsp'     =>   '160',
    'ndash'    => 'x2013',
    'ne'       => 'x2260',
    'ni'       => 'x220b',
    'not'      =>   '172',
    'notin'    => 'x2209',
    'nsub'     => 'x2284',
    'Ntilde'   =>   '209',
    'ntilde'   =>   '241',
    'Nu'       =>   '925',
    'nu'       =>   '957',
    'Oacute'   =>   '211',
    'oacute'   =>   '243',
    'Ocirc'    =>   '212',
    'ocirc'    =>   '244',
    'OElig'    =>   '338',
    'oelig'    =>   '339',
    'Ograve'   =>   '210',
    'ograve'   =>   '242',
    'oline'    => 'x203e',
    'Omega'    =>   '937',
    'omega'    =>   '969',
    'Omicron'  =>   '927',
    'omicron'  =>   '959',
    'oplus'    => 'x2295',
    'or'       => 'x2228',
    'ordf'     =>   '170',
    'ordm'     =>   '186',
    'Oslash'   =>   '216',
    'oslash'   =>   '248',
    'Otilde'   =>   '213',
    'otilde'   =>   '245',
    'otimes'   => 'x2297',
    'Ouml'     =>   '214',
    'ouml'     =>   '246',
    'para'     =>   '182',
    'part'     => 'x2202',
    'permil'   => 'x2030',
    'perp'     => 'x22a5',
    'Phi'      =>   '934',
    'phi'      =>   '966',
    'Pi'       =>   '928',
    'pi'       =>   '960',
    'piv'      =>   '982',
    'plusmn'   =>   '177',
    'pound'    =>   '163',
    'Prime'    => 'x2033',
    'prime'    => 'x2032',
    'prod'     => 'x220f',
    'prop'     => 'x221d',
    'Psi'      =>   '936',
    'psi'      =>   '968',
    'radic'    => 'x221a',
    'rang'     => 'x232a',
    'raquo'    =>   '187',
    'rArr'     => 'x21d2',
    'rarr'     => 'x2192',
    'rceil'    => 'x2309',
    'rdquo'    => 'x201d',
    'real'     => 'x211c',
    'reg'      =>   '174',
    'rfloor'   => 'x230b',
    'Rho'      =>   '929',
    'rho'      =>   '961',
    'rlm'      => 'x200f',
    'rsaquo'   => 'x203a',
    'rsquo'    => 'x2019',
    'sbquo'    => 'x201a',
    'Scaron'   =>   '352',
    'scaron'   =>   '353',
    'sdot'     => 'x22c5',
    'sect'     =>   '167',
    'shy'      =>   '173',
    'Sigma'    =>   '931',
    'sigma'    =>   '963',
    'sigmaf'   =>   '962',
    'sim'      => 'x223c',
    'spades'   => 'x2660',
    'sub'      => 'x2282',
    'sube'     => 'x2286',
    'sum'      => 'x2211',
    'sup'      => 'x2283',
    'sup1'     =>   '185',
    'sup2'     =>   '178',
    'sup3'     =>   '179',
    'supe'     => 'x2287',
    'szlig'    =>   '223',
    'Tau'      =>   '932',
    'tau'      =>   '964',
    'there4'   => 'x2234',
    'Theta'    =>   '920',
    'theta'    =>   '952',
    'thetasym' =>   '977',
    'thinsp'   => 'x2009',
    'THORN'    =>   '222',
    'thorn'    =>   '254',
    'tilde'    =>   '732',
    'times'    =>   '215',
    'trade'    => 'x2122',
    'Uacute'   =>   '218',
    'uacute'   =>   '250',
    'uArr'     => 'x21d1',
    'uarr'     => 'x2191',
    'Ucirc'    =>   '219',
    'ucirc'    =>   '251',
    'Ugrave'   =>   '217',
    'ugrave'   =>   '249',
    'uml'      =>   '168',
    'upsih'    =>   '978',
    'Upsilon'  =>   '933',
    'upsilon'  =>   '965',
    'Uuml'     =>   '220',
    'uuml'     =>   '252',
    'weierp'   => 'x2118',
    'Xi'       =>   '926',
    'xi'       =>   '958',
    'Yacute'   =>   '221',
    'yacute'   =>   '253',
    'yen'      =>   '165',
    'Yuml'     =>   '376',
    'yuml'     =>   '255',
    'Zeta'     =>   '918',
    'zeta'     =>   '950',
    'zwj'      => 'x200d',
    'zwnj'     => 'x200c'
) }


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

if ($_haveMT) {
    require MT;
    import  MT;
    require MT::Template::Context;
    import  MT::Template::Context;

    if ($_haveMT3) {
	require MT::Plugin;
	import  MT::Plugin;
	my $plugin = new MT::Plugin({
	    name => "Markdown",
	    description => "A plain-text-to-HTML formatting plugin. (Version: $VERSION)",
	    doc_link => 'https://daringfireball.net/projects/markdown/'
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
	docs      => 'https://daringfireball.net/projects/markdown/',
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
	    docs      => 'https://daringfireball.net/projects/markdown/',
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

sub _tabDefault {
	return $_haveBX || $_haveMT ? 4 : 8;
}

sub _strip {
	my $str = shift;
	defined($str) or return undef;
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	$str =~ s/\s+/ /g;
	$str;
}

my %_yamlmode;
BEGIN {%_yamlmode = (
    disable => 0,
    reveal => 1,
    enable => 1,
    conceal => 1,
    show => -1,
    unknown => -1,
    strip => -1
)}
my %_yamlvis;
BEGIN {%_yamlvis = (
    disable => 0,
    reveal => 1,
    enable => -1,
    conceal => 0,
    show => 1,
    unknown => -1,
    strip => 0
)}

sub _require_pod_usage() {
	require Pod::Usage;
	eval 'require Pod::Text::Termcap; 1;' and
		@Pod::Usage::ISA = (qw( Pod::Text::Termcap ));
	defined($ENV{PERLDOC}) && $ENV{PERLDOC} ne "" or
		$ENV{PERLDOC} = "-oterm -oman";
}

#### BBEdit/command-line text filter interface ##########################
sub _main {
    local *ARGV = \@_;


    #### Check for command-line switches: #################
    my %options = ();
    my %cli_opts;
    my $raw = 0;
    use Getopt::Long;
    Getopt::Long::Configure(qw(bundling require_order pass_through));
    GetOptions(
	'help' => sub {
			_require_pod_usage;
			Pod::Usage::pod2usage(-verbose => 2, -exitval => 0)},
	'h' => sub {
			_require_pod_usage;
			Pod::Usage::pod2usage(-verbose => 0, -exitval => 0)},
	'version|V' => sub { # Version info
			print "\nThis is Markdown, version $VERSION $DATE.\n", $COPYRIGHT;
			print "License is Modified BSD (aka 3-clause BSD) License\n";
			print "<https://opensource.org/licenses/BSD-3-Clause>\n";
			exit 0},
	'shortversion|short-version|s' => sub { # Just the version number string
			print $VERSION;
			exit 0},
	'html4tags' => \$cli_opts{'html4tags'},
	'deprecated' => \$cli_opts{'deprecated'},
	'sanitize' => \$cli_opts{'sanitize'},
	'no-sanitize' => sub {$cli_opts{'sanitize'} = 0},
	'validate-xml' => sub {$cli_opts{'validate-xml'} = 1},
	'validate-xml-internal' => sub {$cli_opts{'validate-xml'} = 2},
	'no-validate-xml' => sub {$cli_opts{'validate-xml'} = 0},
	'stripcommentsstrict|stripcomments-strict|strip-comments-strict' =>
		sub {$cli_opts{'stripcomments'} = 1},
	'stripcomments|stripcommentslax|stripcomments-lax|strip-comments|strip-comments-lax' =>
		sub {$cli_opts{'stripcomments'} = 2},
	'stripcommentslaxonly|stripcomments-laxonly|stripcomments-lax-only|strip-comments-lax-only' =>
		sub {$cli_opts{'stripcomments'} = 3},
	'nostripcomments|no-stripcomments|no-strip-comments' => sub {$cli_opts{'stripcomments'} = 0},
	'keepabs|keep-abs|k' => \$cli_opts{'keepabs'},
	'absroot|a=s' => \$cli_opts{'absroot'},
	'base|b=s' => \$cli_opts{'base'},
	'htmlroot|r=s' => \$cli_opts{'htmlroot'},
	'imageroot|i=s' => \$cli_opts{'imageroot'},
	'div:s' => \$cli_opts{'divname'},
	'wiki|w:s' => \$cli_opts{'wiki'},
	'tabwidth|tab-width=s' => \$cli_opts{'tabwidth'},
	'autonumber|auto-number' => \$cli_opts{'autonumber'},
	'raw' => sub { $cli_opts{'raw'} = 1 },
	'raw-xml' => sub { $cli_opts{'raw'} = 1 },
	'raw-html' => sub { $cli_opts{'raw'} = 2 },
	'stylesheet|style-sheet' => \$cli_opts{'stylesheet'},
	'no-stylesheet|no-style-sheet' => sub {$cli_opts{'stylesheet'} = 0},
	'keep-named-character-entities' => \$cli_opts{'keepcharents'},
	'no-keep-named-character-entities' => sub {$cli_opts{'keepcharents'} = 0},
	'us-ascii|ascii' => \$cli_opts{'us_ascii'},
	'no-us-ascii|no-ascii' => sub {$cli_opts{'us_ascii'} = 0},
	'stub' => \$cli_opts{'stub'},
	'yaml:s' => \$cli_opts{'yaml'},
    );
    defined($cli_opts{'raw'}) or $cli_opts{'raw'} = 0;
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
    my $xmlcheck;
    $options{'keep_named_character_entities'} = $cli_opts{'keepcharents'} ? "1" : 0;
    $options{'us_ascii'} = $cli_opts{'us_ascii'} ? "1" : 0;
    $options{divwrap} = defined($cli_opts{'divname'});
    $options{divname} = defined($cli_opts{'divname'}) ? $cli_opts{'divname'} : "";
    $options{sanitize} = 1; # sanitize by default
    $options{sanitize} = $cli_opts{'sanitize'} if defined($cli_opts{'sanitize'});
    $xmlcheck = $options{sanitize} ? 2 : 0;
    $xmlcheck = $cli_opts{'validate-xml'} if defined($cli_opts{'validate-xml'});
    $options{stripcomments} = $cli_opts{'stripcomments'} if defined($cli_opts{'stripcomments'});
    die "--html4tags and --validate-xml are incompatible\n"
	if $cli_opts{'html4tags'} && $xmlcheck == 1;
    die "--no-sanitize and --validate-xml-internal are incompatible\n"
	if !$options{'sanitize'} && $xmlcheck == 2;
    die "--no-sanitize and --strip-comments are incompatible\n"
	if !$options{'sanitize'} && $options{stripcomments};
    die "--raw-html requires --validate-xml-internal\n"
	if $cli_opts{'raw'} == 2 && $xmlcheck != 2;
    if ($xmlcheck == 1) {
	eval { require XML::Simple; 1 } and $hasxml = 1 or $hasxml_err = $@;
	eval { require XML::Parser; 1 } and $hasxmlp = 1 or $hasxmlp_err = $@ unless $hasxml;
	die "$hasxml_err$hasxmlp_err" unless $hasxml || $hasxmlp;
    }
    if ($cli_opts{'tabwidth'}) {
	my $tw = $cli_opts{'tabwidth'};
	die "invalid tab width (must be integer)\n" unless looks_like_number $tw;
	die "invalid tab width (must be >= 2 and <= 32)\n" unless $tw >= 2 && $tw <= 32;
	$options{tab_width} = int(0+$tw);
    }
    $options{auto_number} = 6 if $cli_opts{'autonumber'};
    $options{keepabs} = $cli_opts{'keepabs'};
    $options{abs_prefix} = "";  	# no abs prefix by default
    if ($cli_opts{'absroot'}) { 	# Use abs prefix for absolute path URLs
	my $abs = $cli_opts{'absroot'};
	$abs =~ s{/+$}{};
	$options{abs_prefix} = $abs;
    }
    $options{base_prefix} = ""; 	# no base prefix by default
    if ($cli_opts{'base'}) {		# Use base prefix for fragment URLs
	$options{base_prefix} = $cli_opts{'base'};
    }
    if ($cli_opts{'htmlroot'}) {	 # Use URL prefix
	$options{url_prefix} = $cli_opts{'htmlroot'};
    }
    if ($cli_opts{'imageroot'}) {	 # Use image URL prefix
	$options{img_prefix} = $cli_opts{'imageroot'};
    }
    SetWikiOpts(\%options, $cli_opts{'wiki'}); # Set wiki links options
    if (ref($options{wikiopt}) eq 'HASH') {
	my $o = $options{wikiopt};
	$o->{"f"} && $o->{"%"} and
	    die "--wiki sub-options 'f' and '%' are mutually exclusive\n"
    }
    if ($cli_opts{'raw'}) {
	$raw = 1;
	$options{htmlauto} = 1 if $cli_opts{'raw'} == 2;
    }
    $options{show_styles} = $cli_opts{'stylesheet'} if defined($cli_opts{'stylesheet'});
    $options{show_styles} = 1 if $stub && !defined($options{show_styles});
    $options{tab_width} = 8 unless defined($options{tab_width});
    my $ym = $cli_opts{'yaml'};
    defined($ym) && $ym ne "" or $ym = "enable";
    my $lcym = lc($ym);
    exists($_yamlmode{$lcym}) or die "invalid --yaml= value '$ym'\n";
    $options{yamlmode} = $_yamlmode{$lcym};
    $options{yamlvis} = $_yamlvis{$lcym};

    my $hdrf = sub {
	my $out = "";
	if ($stub > 0) {
	    $out .= <<'HTML5';
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta charset="utf-8" />
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
HTML5
	} elsif ($stub < 0) {
	    $out .= <<'HTML4';
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
		$out .= "<title>$title</title>\n";
	    }
	}
	$out .= GenerateStyleSheet($g_style_prefix) if $options{show_styles};
	if ($stub) {
	    $out .= "</head>\n<body style=\"text-align:center\">\n" .
		"<div style=\"display:inline-block;text-align:left;max-width:42pc\">\n";
	}
	$out;
    };

    #### Process incoming text: ###########################
    my ($didhdr, $hdr, $result, $ftr) = (0, "", "", "");
    @ARGV or push(@ARGV, "-");
    foreach (@ARGV) {
	my ($fh, $contents, $oneresult);
	$_ eq "-" or open $fh, '<', $_ or fauxdie "could not open \"$_\": $!\n";
	{
	    local $/; # Slurp the whole file
	    $_ eq "-" and $contents = <STDIN>;
	    $_ ne "-" and $contents = <$fh>;
	}
	defined($contents) or fauxdie "could not read \"$_\": $!\n";
	$_ eq "-" or close($fh);
	$options{xmlcheck} = ($xmlcheck == 2) ? 2 : 0;
	$oneresult = $raw ? ProcessRaw($contents, \%options) : Markdown($contents, \%options);
	$oneresult =~ s/\s+$//os;
	if ($oneresult ne "") {
	    if (!$didhdr && !$raw) {
		$hdr = &$hdrf();
		$didhdr = 1;
	    }
	    $result .= $oneresult . "\n";
	}
    }
    $hdr = &$hdrf() unless $didhdr || $raw;
    $ftr = "</div>\n</body>\n</html>\n" if $stub && !$raw;
    if ($xmlcheck == 1) {
	my ($good, $errs);
	if ($stub && !$raw) {
	    ($good, $errs) = _xmlcheck($hdr.$result.$ftr);
	} else {
	    ($good, $errs) = _xmlcheck("<div>".$result."</div>");
	}
	$good or die $errs;
    }
    print $hdr, $result, $ftr;

    exit 0;
}


# INPUT
#   $1: HASH ref
#   $2: value of --wiki= option (see docs) except
#       that a value of undef turns off wiki links
# OUTPUT
#   $1->{wikipat}
#   $1->{wikiopt}
#
sub SetWikiOpts {
    my ($o, $wpat) = @_;
    ref($o) eq "HASH" or die "internal error: first arg to SetWikiOpts must be HASH ref";
    delete $o->{wikipat};
    delete $o->{wikiopt};
    defined($wpat) or return;
    # Parse wiki links option setting
    my $wopt = "s(:md)";
    if ($wpat =~ /^(.*?)%\{((?:[%0-9A-Za-z]|[Ss]\([^)]*\))*)\}(.*)$/) {
	$o->{wikipat} = $1 . "%{}" . $3;
	$wopt = $2;
    } else {
        $o->{wikipat} = $wpat . "%{}.html";
    }
    my $sval = 1;
    while ($wopt =~ /^(.*?)s\(([^)]*)\)(.*)$/i) {
	my $sarg = $2;
	$wopt = $1 . "s" . $3;
	$sarg =~ s/^\s+//; $sarg =~ s/\s+$//;
	$sval = {} unless ref($sval) eq "HASH";
	s/^\.//, $sval->{lc($_)}=1 foreach split(/(?:\s*,\s*)|(?:(?<!,)\s+(?!,))/, $sarg);
	$sval = 1 unless scalar(keys(%$sval));
    }
    $o->{wikiopt} = { map({$_ => 1} split(//,lc($wopt))) };
    if (ref($sval) eq "HASH" && $sval->{':md'}) {
	delete $sval->{':md'};
	$sval->{$_} = 1 foreach qw(md rmd mkd mkdn mdwn mdown markdown litcoffee);
    }
    $o->{wikiopt}->{'s'} = $sval if $o->{wikiopt}->{'s'};
}


# Return a copy of the fancy CSS style sheet that uses the
# passed in prefix as a prefix for the CSS style names.
# If no argument is passed in, use $g_style_prefix
# as the CSS style name prefix.
sub GenerateStyleSheet {
    my $prefix = shift;
    defined($prefix) or $prefix = $g_style_prefix;
    my $stylesheet = $g_style_sheet;
    $stylesheet =~ s/%\(base\)/$prefix/g;
    return $stylesheet;
}


sub _xmlcheck {
	my $text = shift;
	my ($good, $errs);
	($hasxml ? eval { XML::Simple::XMLin($text, KeepRoot => 1) && 1 } :
	 eval {
		my $p = XML::Parser->new(Style => 'Tree', ErrorContext => 1);
		$p->parse($text) && 1;
	}) and $good = 1 or $errs = _trimerr($@);
	($good, $errs);
}


sub _trimerr {
	my $err = shift;
	1 while $err =~ s{\s+at\s+\.?/[^,\s\n]+\sline\s+[0-9]+\.?(\n|$)}{$1}is;
	$err =~ s/\s+$//os;
	$err . "\n";
}


sub _PrepareInput {
    my ($input,$parseyaml) = @_;
    defined $input or $input = "";
    {
	use bytes;
	$input =~ s/[\x00-\x08\x0B\x0E-\x1F\x7F]+//gso;
    }
    my $output;
    if (Encode::is_utf8($input) || utf8::decode($input)) {
	$output = $input;
    } else {
	$output = $encoder->decode($input, Encode::FB_DEFAULT);
    }
    # Standardize line endings:
    $output =~ s{\r\n}{\n}g;  # DOS to Unix
    $output =~ s{\r}{\n}g;    # Mac to Unix

    # Extract YAML front matter if requested
    my $yaml = undef;
    if ($parseyaml) {
	$yaml = {};
	if ($output =~ /^---[ \t]*(?:\n|\z)/g) {
	    until ($output =~ /\G(?:(?:(?:---)|(?:\.\.\.))[ \t]*(?:\n|\z)|\z)/gc) {
		next if $output =~ m"\G[ \t]*(?:#[^\n]*)?\n"gc; # skip comment lines
		next if $output =~ m"\G[ \t]*(?:#[^\n]*)\z"gc; # skip final no EOL comment
		last unless $output =~ /\G([^\n]+)(?:\n|\z)/gc;
		my $yl = $1;
		if ($yl =~ /^([A-Za-z_][A-Za-z_0-9.-]*):[ \t]+(.*)$/os) {
			my ($k, $v) = ($1, $2);
			$yaml->{lc($k)} = _YAMLvalue($2);
		}
	    }
	    $output = substr($output, pos($output));
	}
    }
    return wantarray ? ($output, $yaml) : $output;
}


sub _YAMLvalue {
	my $v = shift;
	$v =~ s/^\s+//;
	if (substr($v, 0, 1) eq '"') {
		# only $ and/or @ present issues, map them
		$v =~ tr/\@\$/\036\037/;
		eval '{$v='.$v."\n}1" or $v = undef;
		$v =~ tr/\036\037/\@\$/ if defined($v);
	} else {
		$v =~ s"#.*$""os;
		$v =~ s/\s+$//os;
		$v ne "" or $v = undef;
	}
	return $v;
}


sub ProcessRaw {
    my $text = _PrepareInput(shift);

    # Any remaining arguments after the first are options; either a single
    # hashref or a list of name, value pairs.  See _SanitizeOpts comments.

    %opt = (
	empty_element_suffix	=> $g_empty_element_suffix,
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
    _SanitizeOpts(\%opt);

    # Sanitize all '<'...'>' tags if requested
    $text = _SanitizeTags($text, $opt{xmlcheck}, $opt{htmlauto}) if $opt{sanitize};

    # Eliminate known named character entities
    $opt{keep_named_character_entities} or
	$text = ConvertNamedCharacterEntities($text);

    # Convert to US-ASCII only if requested
    $opt{us_ascii} and
	$text = ConvertToASCII($text);

    utf8::encode($text);
    if ($opt{divwrap}) {
	my $id = $opt{divname};
	defined($id) or $id = "";
	$id eq "" or $id = ' id="'.escapeXML($id).'"';
	chomp($text);
	return "<div$id>\n".$text."\n</div>\n";
    }
    return $text;
}


# $1: HASH ref with the following key value semantics
#
#   sanitize => any-false-value (no action), any-true-value (sanitize).
#               note that a true value of xmlcheck or a true value of
#               stripcomments or a urlfunc value that is a CODE ref
#               always forces sanitize to activate.
#               tag attributes are sanitized by removing all "questionable"
#               attributes (such as script attributes, unknown attributes
#               and so forth) and normalizing the remaining ones (i.e.
#               adding missing quotes and/or values etc.).
#               effective for both ProcessRaw and Markdown.
#   xmlcheck => 0 (no check), any-true-value (internal check).
#               note that the default if xmlcheck is not set/valid is 2.
#               note that a true value is effective for both ProcessRaw
#               and Markdown.  note that a true value automatically inserts
#               the closing tag for auto-closing tags and converts empty tags
#               to the correct format converting empty tags that shouldn't be
#               to an open and close pair; since xmlcheck is a function of the
#               sanitizer, tag attributes are also always sanitized whenever
#               xmlcheck has a true value.
#               note that a true xmlcheck value WILL call "die" with a
#               detailed indication of the error(s) if xml validation fails
#               in which case any line/column numbers refer to the text that
#               would be produced by a sanitize=>0, xmlcheck=>0 call to
#               either ProcessRaw or Markdown, NOT the original input text.
#   htmlauto => any-false-value (no auto close), any-true-value (auto-close)
#               only effective for ProcessRaw; always enabled for Markdown.
#               when xmlcheck is set to 2 provide html automatic closing tag
#               and optional closing tag semantics where closing tags are
#               automatically inserted when encountering an opening tag that
#               auto closes a currently open tag and tags with an optional
#               closing tag that's missing have that inserted as appropriate.
#               a true value may result in some texts being rejected that
#               would be otherwise be accepted (e.g. "<p><pre></pre></p>"
#               which gets turned into "<p></p><pre></pre></p>" which then
#               no longer validates).
#   stripcomments => any-false-value (no action), any-true-value (strip).
#                 => 1 (strip-strict), 2 (strip-lax), 3 (strip-lax-only)
#               a non-numeric true value will be forced to 2.
#               a numeric value < 0 will be forced to 2.
#               a numeric value > 0 and < 1 will be forced to 2.
#               a numeric value > 3 will be forced to 3.
#               a non-integer value will forced to an integral value.
#               1, 2, and 3 correspond to the command line options
#               --strip-comments-strict, --strip-comments-lax and
#               --strip-comments-lax-only respectively.
#               since the strip comments mechanism is a function of the
#               sanitizer, if stripcomments is set to any-true-value then
#               tag attributes will also always be sanitized.
#               if stripcomments is not set or is set to the empty string,
#               then it will be set to 3 if sanitize is true and 0 otherwise.
#               effective for both ProcessRaw and Markdown.
#   empty_element_suffix => " />" or ">"
#               will be forced to " />" if not valid or defined.
#               effective for both ProcessRaw and Markdown.
#   keep_named_character_entities => "1" (keep them), any-other-value (convert).
#               unless this option is present and has exactly the value "1"
#               then known named character entities will be converted to
#               their equivalent numerical entity.  Use of this option is
#               strongly discouraged to avoid strict XML validation failures.
#   us_ascii    => if true, non-US-ASCII characters will be converted to
#               numerical character entities making the output US-ASCII only.
#   divwrap     => if true, wrap output contents in <div>...</div>
#   divname     => if defined and non-empty will be id of divwrap div tag
#   urlfunc     => if set to a CODE ref, the function will be called with
#               seven arguments like so:
#                 $result = &$urlfunc($iresult, \%opts, $tag, $uhost, $uabs, $q, $f)
#               where on input $iresult is the result that would be produced
#               if no urlfunc was provided and on output $result will be
#               used as url value.  $tag is either "img" or "a" to indicate the
#               source of the url.  $uhost.$uabs is the result of stripping off
#               any query string and/or fragment from $iresult.  $q contains either
#               an empty string or the stripped off query string and $f contains
#               an empty string or the stripped off fragment if they were originally
#               present where a non-empty $q always starts with '?' and a non-empty
#               $f always starts with '#'.  $uhost contains the scheme and
#               host+port if present (it may be the empty string).  $uabs contains
#               the path portion which may or may not start with a "/" depending on
#               whether or not it's a relative path.  The $iresult value is related
#               to the other arguments like so:
#                 $iresult = $uhost . $uabs . $q . $f;
#               All values passed to the urlfunc function have already been HTML
#               unescaped and the returned value will be automatically HTML escaped.
#               Any provided urlfunc should treat the %opts HASH as
#               read-only.  Modifying the %opts HASH in urlfunc will
#               likely result in unpredictable behavior!  Don't do it!
#               If urlfunc is set to a CODE ref then tags will also always be
#               sanitized.
#
#  The remaining key value pairs are ignored by ProcessRaw and are only
#  effective when using Markdown or _main
#
#   tab_width => 1..32 which is how many spaces tabs are expanded to.
#               will be forced to 8 if not in range.
#   indent_width => 1..32 how many spaces make a new "indent" level.
#               will be forced to 4 if not in range.
#   style_prefix => prefix to prepend to all CSS style names in the
#               fancy CSS style sheet.
#               defaults to $g_style_prefix if not defined.
#               note that _main actually adds the style sheet (when
#               requested); use GenerateStyleSheet to retrieve the
#               fancy style sheet when calling Markdown directly.
#   auto_number => <= 0 (default) no numbering, 1 number h1s,
#               2 number h1s, h2s, 3 number h1-h3s, ... >= 6 number h1-h6s
#   anchors    => existence of this key triggers return of anchors HASH
#   yamlmode   => 0 (no YAML processing), > 0 (YAML on), < 0 (YAML ignore)
#               if 0, the YAML front matter processor is completely
#               disabled and any YAML front matter that might be present
#               will be treated as markup.  if > 0 any YAML front matter
#               will be processed and any recognized options applied.
#               if < 0 any YAML front matter will be parsed but no
#               options will be applied at all.  When != 0 the parsed
#               YAML front matter can be retrieved via the 'yaml' key.
#               defaults to 1 if not defined or not a number.
#   yamlvis    => 0 (invisible), > 0 (visible), < 0 (vis if unknown)
#               if yamlmode == 0 then yamlvis has no effect.  if > 0
#               then any parsed YAML front matter options will be shown
#               in the formatted output.  if 0 then NO YAML front
#               matter options will be shown in the formatted output.
#               if < 0 then YAML front matter options will be shown in
#               the formatted output only if there are any unrecognized
#               options present.
#               defaults to -1 if not defined or not a number.
#   keepabs    => any-false-value (no action), any-true-value (keep)
#               if true, any absolute path URLs remaining after applying
#               any abs_prefix value will be kept and not be subject
#               to modification by any url_prefix or img_prefix value.
#   abs_prefix => value to prefix to absolute path URLs (i.e. start with /).
#               note that this does NOT get prepended to //host/path URLs.
#   url_prefix => value to prefix to non-absolute URLs.
#               note that this does NOT get prepended to //host/path URLs.
#   img_prefix => value to prefix to non-absolute image URLs.
#               note that this does NOT get prepended to //host/path URLs.
#               note that if img_prefix is undef or empty ("") then
#               url_prefix will be prepended to image URLs.
#   base_prefix => value to prefix to fragment-only URLs (i.e. start with #).
#               note that fragment-only URLs are always left undisturbed
#               if this is not set.  Fragment-only URLs are NOT affected by
#               any of abs_prefix, url_prefix or img_prefix.
#   wikipat     => non-empty pattern string to enable wiki links.
#               best set with SetWikiOpts (see SetWikiOpts comments).
#   wikiopt     => HASH ref of options affecting wiki links processing.
#               best set with SetWikiOpts (see SetWikiOpts comments).
#   wikifunc    => if set to a CODE ref, the function will be called with
#               six arguments like so:
#                 $result = &$wikifunc($iresult, \%opts, $link, $wbase, $qf, $io)
#               where on input $iresult is the result that would be produced
#               if no wikifunc was provided and on output $result will be
#               used as the wiki expansion.  $link is the original wiki
#               destination as specified in the source, $wbase is the result
#               of stripping off any query string and/or fragment from $link
#               and then transforming that according to the wikiopt HASH ref.
#               $qf contains either an empty string or the stripped off
#               query string and/or fragment if one was originally present.
#               If $io is a HASH ref (otherwise it will be undef), then it's
#               a wiki image link and %$io contains the options (if any) where
#               the keys that might be present include "width", "height", "align"
#               and "alt".  If present, width and height are guaranteed to be
#               positive integers and align is guaranteed to be "left", "right"
#               or "center".  The value for "alt" may be the empty string.
#               The "imgflag" key has a SCALAR ref value and if the value it
#               refers to is changed to any false value the result will become
#               an A tag rather than an IMG tag.
#               The $iresult value is related to the other arguments like so:
#                 $iresult = $opts->{wikipat};
#                 $iresult =~ s/%\{\}.+$/%{}/ if ref($io) eq "HASH";
#                 $iresult = s/%\{\}/$wbase/;
#                 $iresult .= $qf;
#               Any provided wikifunc should treat the %opts HASH as
#               read-only.  Modifying the %opts HASH in wikifunc will
#               likely result in unpredictable behavior!  Don't do it!
#
#  Special handling for abs_prefix, url_prefix, img_prefix and/or base_prefix
#  may be activated by setting any subset (or all) of the values for these
#  keys to a CODE ref.  The single argument is the URL and the result must
#  be the adjusted URL.  For example, the equivalent CODE ref to setting
#  url_prefix to $string is simply sub { $string.$_[0] }.  By using a
#  CODE ref, behavior other than simply performing a prepend operation
#  can be realized when necessary for unusual situations.
#
#  The following are OUTPUT values that can only be retrieved when
#  Markdown is called with a HASH ref as the second argument
#
#   anchors    => if the 'anchors' key exists in the input HASH ref
#               will be set to a HASH ref containing lookup keys
#               for valid fragment ids in the document (only those
#               created from Markdown markup) with the value the
#               actual fragment link to use.  Do not use this directly
#               but pass it as the first argument to the ResolveFragment
#               function to resolve a "fuzzy" fragment name to its
#               actual fragment name in the generated output.
#               NOTE: to activate return of anchors the 'anchors' key
#               simply must exist in the input HASH ref passed to the
#               Markdown function, its value will be replaced on output.
#
#   h1         => will be set to the tag-stripped value of the first
#               non-empty H1 generated by Markdown-style markup.
#               note that literal <h1>...</h1> values are NOT picked up.
#               will be left unchanged if no Markdown-style H1 detected.
#               note that the value is NOT xml escaped but should be
#               before embedding in an XHTML document.  If yamlmode > 0
#               and a 'title' value has been encountered, then this
#               will be set to that 'title' value instead (and the
#               'title' key and value will still be present in %$yaml).
#
#   yaml       => if yamlmode is != 0 then this will be set to a HASH
#               ref containing any parsed YAML front matter or left
#               unchanged if no YAML front matter was found.  If the
#               parsed YAML front matter contains only whitespace and/or
#               comments then this will be set to a HASH ref that has
#               no keys or values.
#
sub _SanitizeOpts {
    my $o = shift; # hashref
    ref($o) eq "HASH" or return;

    $o->{firstline} = 0;
    $o->{keep_named_character_entities} = 0 unless
	defined($o->{keep_named_character_entities}) && $o->{keep_named_character_entities} eq "1";
    $o->{xmlcheck} = looks_like_number($o->{xmlcheck}) && $o->{xmlcheck} == 0 ? 0 : 2;
    $o->{sanitize} = 1 if $o->{xmlcheck} && !$o->{sanitize};
    $o->{sanitize} = 1 if ref($o->{urlfunc}) eq 'CODE' && !$o->{sanitize};
    !looks_like_number($o->{stripcomments}) and
	$o->{stripcomments} = $o->{stripcomments} ? 2 :
	    ($o->{sanitize} && (!defined($o->{stripcomments}) || $o->{stripcomments} eq "") ? 3 : 0);
    $o->{stripcomments} && $o->{stripcomments} < 1 and $o->{stripcomments} = 2;
    $o->{stripcomments} = int($o->{stripcomments});
    $o->{stripcomments} > 3 and $o->{stripcomments} = 3;
    $o->{stripcomments} && !$o->{sanitize} and $o->{sanitize} = 1;

    # this is gross, but having the globals avoids unnecessary slowdown
    if ($o->{sanitize} && $o->{xmlcheck}) {
	$g_start_p = "<\20>";
	$g_close_p = "</\20>";
    } else {
	$g_start_p = "<p>";
	$g_close_p = "</p>";
    }

    defined($o->{empty_element_suffix}) &&
    ($o->{empty_element_suffix} eq " />" || $o->{empty_element_suffix} eq ">")
	or $o->{empty_element_suffix} = " />";

    $o->{tab_width} = 8 unless looks_like_number($o->{tab_width}) &&
	1 <= $o->{tab_width} && $o->{tab_width} <= 32;
    $o->{tab_width} = int($o->{tab_width});

    $o->{indent_width} = 4 unless looks_like_number($o->{indent_width}) &&
	1 <= $o->{indent_width} && $o->{indent_width} <= 32;
    $o->{indent_width} = int($o->{indent_width});

    defined($o->{auto_number}) or $o->{auto_number} = '';
    $o->{auto_number} eq '' || looks_like_number($o->{auto_number})
	or $o->{auto_number} = 6;
    if ($o->{auto_number} ne '') {
	$o->{auto_number} = int(0+$o->{auto_number});
	$o->{auto_number} >= 0 or $o->{auto_number} = 0;
	$o->{auto_number} <= 6 or $o->{auto_number} = 6;
    }

    defined($o->{style_prefix}) or $o->{style_prefix} = $g_style_prefix;

    $o->{abs_prefix} = _MakePrefixCODERef($o->{abs_prefix}, 1)
	unless ref($o->{abs_prefix}) eq 'CODE';
    $o->{url_prefix} = _MakePrefixCODERef($o->{url_prefix}, 0)
	unless ref($o->{url_prefix}) eq 'CODE';
    $o->{img_prefix} = _MakePrefixCODERef($o->{img_prefix}, 0)
	unless ref($o->{img_prefix}) eq 'CODE';
    $o->{base_prefix} = _MakePrefixCODERef($o->{base_prefix}, -1)
	unless ref($o->{base_prefix}) eq 'CODE';

    ref($o->{wikiopt}) eq "HASH" or $o->{wikiopt} = {};

    # Note that because Markdown makes a copy of the options
    # before calling this function, this does not actually remove
    # any "h1" key that might have been set by the caller of
    # the Markdown function.  However, by deleting it here,
    # this guarantees that any found value will actually be
    # picked up and stored (which will not happen if the key
    # already exists).
    delete $o->{h1};

    # Default is to silently strip any known YAML front matter
    # Same comment about "yaml" key as above for "h1" key
    $o->{yamlmode} = 1 unless looks_like_number($o->{yamlmode});
    $o->{yamlvis} = -1 unless looks_like_number($o->{yamlvis});
    delete $o->{yaml};

    # The anchors hash will only be returned if the key exists
    # (the key's value doesn't matter), set the value to an empty
    # HASH ref just in case to make sure it's always a HASH ref.
    $o->{anchors} = {} if exists($o->{anchors});
}

my %_yamlopts;
BEGIN {%_yamlopts = map({$_ => 1} qw(
	display_metadata
	header_enum
	title
))}


sub _HasUnknownYAMLOptions {
    do { return 1 unless exists($_yamlopts{$_}) } foreach keys(%{$_[0]});
    return 0;
}


sub _ApplyYAMLOpts {
    my ($yaml, $opt) = @_;
    if (defined($yaml->{display_metadata}) && $opt->{yamlvis} < 0) {
	# ignore display_metadata except in --yaml=enable mode
	$opt->{yamlvis} = _YAMLTrueValue($yaml->{display_metadata}) ? 1 : 0;
    }
    $opt->{h1} = $yaml->{title} if defined($yaml->{title});
    if (defined($yaml->{header_enum}) && $opt->{auto_number} eq '') {
	$opt->{auto_number} = _YAMLTrueValue($yaml->{header_enum}) ? 6 : 0;
    }
}


sub _YAMLTrueValue {
    my $v = shift;
    defined($v) or $v = "";
    $v = lc($v);
    return !($v eq "" || $v eq "0" || $v eq "false" || $v eq "disable" || $v eq "off" || $v eq "no");
}


# Actually returns an empty string rather than a CODE ref
# if an empty prefix is passed in.  Trailing "/"s are trimmed
# off if the second argument is positive or the string does NOT
# consist of only "/"s.  A trailing "/" is added unless the
# trimmed prefix already has one or the second argument is true.
# If the second argument is negative, the prefix is used as-is.
sub _MakePrefixCODERef {
    my ($prefix, $mtok) = @_;
    defined($prefix) or $prefix = "";
    looks_like_number($mtok) or $mtok = $mtok ? 1 : 0;
    if ($mtok > 0) {
	$prefix =~ s,/+$,,;
    } elsif (!$mtok) {
	$prefix =~ s,//+$,/,;
    }
    $prefix ne "" or return "";
    $prefix .= '/' if !$mtok && substr($prefix, -1, 1) ne '/';
    return sub { $prefix . $_[0] };
}


sub Markdown {
#
# Primary function. The order in which other subs are called here is
# essential. Link and image substitutions need to happen before
# _EscapeSpecialChars(), so that any *'s or _'s in the <a>
# and <img> tags get encoded.
#
    my $text = shift;

    # Any remaining arguments after the first are options; either a single
    # hashref or a list of name, value pairs.  See _SanitizeOpts comments.
    %opt = (
	# set initial defaults
	style_prefix		=> $g_style_prefix,
	empty_element_suffix	=> $g_empty_element_suffix,
	tab_width		=> _tabDefault,
	indent_width		=> $g_indent_width,
	abs_prefix		=> "", # Prefixed to absolute path URLs
	url_prefix		=> "", # Prefixed to non-absolute URLs
	img_prefix		=> "", # Prefixed to non-absolute image URLs
	base_prefix		=> "", # Prefixed to fragment-only URLs
    );
    @autonum = ();
    my %args = ();
    if (ref($_[0]) eq "HASH") {
	%args = %{$_[0]};
    } else {
	%args = @_;
    }
    while (my ($k,$v) = each %args) {
	$opt{$k} = $v;
    }
    _SanitizeOpts(\%opt);

    my $yaml;
    ($text, $yaml) = _PrepareInput($text, $opt{yamlmode});
    _ApplyYAMLOpts($yaml, \%opt) if ref($yaml) eq "HASH" && $opt{yamlmode} > 0;
    my $yamltable = "";
    if (ref($yaml) eq "HASH" && %$yaml && $opt{yamlmode} && $opt{yamlvis}) {
	if ($opt{yamlvis} > 0 || _HasUnknownYAMLOptions($yaml)) {
	    my ($hrows, $drows) = ("", "");
	    foreach (sort(keys(%$yaml))) {
		my $v = $yaml->{$_};
		my $rspn = '';
		if (defined($v)) {
		    $v =~ s/&/&amp;/g;
		    $v =~ s/</&lt;/g;
		    utf8::encode($v);
		    $drows .= "<td>" . $v . "</td>\n";
		} else {
		    $rspn = " class=\"$opt{style_prefix}yaml-undef-value\" rowspan=\"2\" valign=\"top\"";
		}
		$hrows .= "<th$rspn>" . $_ . "</th>\n";
	    }
	    $yamltable = "<table class=\"$opt{style_prefix}yaml-table\" border=\"1\">\n" .
		"<tr>\n$hrows</tr>\n<tr>\n$drows</tr>\n</table>\n";
	    $opt{firstline} = scalar(@{[$yamltable =~ /\n/g]});
	}
    }

    # Clear the globals. If we don't clear these, you get conflicts
    # from other articles when generating a page which contains more than
    # one article (e.g. an index page that shows the N most recent
    # articles):
    %g_urls = ();
    %g_titles = ();
    %g_anchors = ();
    %g_anchors_id = ();
    %g_block_ids = ();
    %g_code_block_ids = ();
    %g_html_blocks = ();
    %g_code_blocks = ();
    @g_xml_comments = ();
    $g_list_level = 0;

    # Make sure $text ends with a couple of newlines:
    $text .= "\n\n";

    # Handle backticks-delimited code blocks
    $text = _HashBTCodeBlocks($text);

    # Convert all tabs to spaces.
    $text = _DeTab($text);

    # Strip any lines consisting only of spaces.
    # This makes subsequent regexen easier to write, because we can
    # match consecutive blank lines with /\n+/ instead of something
    # contorted like / *\n+/ .
    $text =~ s/^ +$//mg;

    # Turn block-level HTML blocks into hash entries
    $text = _HashHTMLBlocks($text, 1);

    # Strip link definitions, store in hashes.
    $text = _StripLinkDefinitions($text);

    $text = _RunBlockGamut($text, 1);

    # Remove indentation markers
    $text =~ s/\027+//gs;

    # Expand auto number flags
    $text =~ s/\034([1-6])/_AutoHeaderNum(ord($1)&0x7)/gse
	if $opt{auto_number} ne '' && $opt{auto_number} > 0;

    # Unhashify code blocks
    $text =~ s/(\025\d+\026)/$g_code_blocks{$1}/g;

    $text = _UnescapeSpecialChars($text);

    $text .= "\n" unless $text eq "";

    # Sanitize all '<'...'>' tags if requested
    $text = _SanitizeTags($text, $opt{xmlcheck}, 1) if $opt{sanitize};

    # Eliminate known named character entities
    $opt{keep_named_character_entities} or do {
	$yamltable = ConvertNamedCharacterEntities($yamltable);
	$text = ConvertNamedCharacterEntities($text);
    };

    # Convert to US-ASCII only if requested
    $opt{us_ascii} and do {
	utf8::decode($yamltable);
	$yamltable = ConvertToASCII($yamltable);
	utf8::encode($yamltable);
	$text = ConvertToASCII($text);
    };

    utf8::encode($text);
    if (ref($_[0]) eq "HASH") {
	${$_[0]}{anchors} = {%g_anchors_id} if exists(${$_[0]}{anchors});
	if (defined($opt{h1}) && $opt{h1}) {
	    utf8::encode($opt{h1});
	    ${$_[0]}{h1} = $opt{h1};
	}
	${$_[0]}{yaml} = $yaml if ref($yaml) eq "HASH";
    }

    if ($opt{divwrap}) {
	my $id = $opt{divname};
	defined($id) or $id = "";
	$id eq "" or $id = ' id="'.escapeXML($id).'"';
	chomp($text);
	return "<div$id>\n".$yamltable.$text."\n</div>\n";
    }
    return $yamltable.$text;
}


sub _HashBTCodeBlocks {
#
#   Process Markdown backticks (```) delimited code blocks
#   Process some (limited recognition) tilde (~~~) delimited code blocks
#
    my $text = shift;
    my $less_than_indent = $opt{indent_width} - 1;

    $text =~ s{
	    (?:(?<=\n)|\A)
		([ ]{0,$less_than_indent})``(`+)[ \t]*(?:([\w.+-]+[#]?)(?:[ \t][ \t\w.+-]*)?)?\n
	     ( # $4 = the code block -- one or more lines, starting with ```
	      (?:
		.*\n
	      )+?
	     )
	    # and ending with ``` or end of document
	    (?:(?:[ ]{0,$less_than_indent}``\2`*[ \t]*(?:\n|\Z))|\Z)
	}{
	    # $2 contains syntax highlighting to use if defined
	    my $leadsp = length($1);
	    my $codeblock = $4;
	    $codeblock =~ s/[ \t]+$//mg; # trim trailing spaces on lines
	    $codeblock = _DeTab($codeblock, 8, $leadsp); # physical tab stops are always 8
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace
	    $codeblock = _EncodeCode($codeblock); # or run highlighter here
	    $codeblock = "<div class=\"$opt{style_prefix}code-bt\"><pre style=\"display:none\"></pre><pre><code>"
		. $codeblock . "\n</code></pre></div>";

	    my $key = block_id($codeblock, 2);
	    $g_code_blocks{$key} = $codeblock;
	    "\n\n" . $key . "\n\n";
	}egmx;

    $text =~ s{
	    (?:(?<=\n)|\A)
		([ ]{0,$less_than_indent})~~(~)[ \t]*(?:([\w.+-]+[#]?)(?:[ \t][ \t\w.+-]*)?)?\n
	     ( # $4 = the code block -- one or more lines, starting with ~~~
	      (?:
		.*\n
	      )+?
	     )
	    # and ending with ~~~ or end of document
	    (?:(?:[ ]{0,$less_than_indent}~~\2~*[ \t]*(?:\n|\Z))|\Z)
	}{
	    # $2 contains syntax highlighting to use if defined
	    my $leadsp = length($1);
	    my $codeblock = $4;
	    $codeblock =~ s/[ \t]+$//mg; # trim trailing spaces on lines
	    $codeblock = _DeTab($codeblock, 8, $leadsp); # physical tab stops are always 8
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace
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
			<?((?:\S(?:\\\n\s*[^\s"(])?)+?)>? # url = $2
			  [ ]*
			  \n?		    # maybe one newline
			  [ ]*
			(?:
			    (?<=\s)	    # lookbehind for whitespace
			    (?:(['"])|(\()) # title quote char
			    (.+?)	    # title = $5
			    (?(4)\)|\3)	    # match same quote
			    [ ]*
			)?  # title is optional
			(?:\n+|\Z)
		    }
		    {}mx) {
	my $id = _strip(lc $1); # Link IDs are case-insensitive
	my $url = $2;
	my $title = _strip($5);
	$url =~ s/\\\n\s*//gs;
	if ($id ne "") {
		# These values always get passed through _MakeATag or _MakeIMGTag later
		$g_urls{$id} = $url;
		if (defined($title) && $title ne "") {
		    $g_titles{$id} = $title;
		}
	}
    }

    return $text;
}

my %ok_tag_name; # initialized later
my ($block_tags_a, $block_tags_b, $block_tags_c);
BEGIN {
    $block_tags_a = qr/\020|p|div|center|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del/io;
    $block_tags_b = qr/\020|p|div|center|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math/io;
    $block_tags_c = qr/div|center|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math/io;
}

sub _HashHTMLBlocks {
    my ($text, $toplevel) = @_;
    my $less_than_indent = $opt{indent_width} - 1;
    my $idt = "\027" x $g_list_level;
    my $blkprc = $toplevel ?
	sub { return $ok_tag_name{$_[1]} ? _EncodeAmpsAndAngles($_[0]) : $_[0] } :
	sub { return $_[0] };

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
		    ((?:\Q$idt\E)?)	# optional lead in = $2
		    <($block_tags_a)	# start tag = $3
		    \b			# word break
		    (?:.*\n)*?		# any number of lines, minimally matching
		    \2</\3\s*>		# the matching end tag
		    [ ]*		# trailing spaces
		    (?=\n+|\Z) # followed by a newline or end of document
		)
	    }{
		my $blk = &$blkprc($1, $3);
		my $key = block_id($blk);
		$g_html_blocks{$key} = $blk;
		"\n\n" . $key . "\n\n";
	    }eigmx;


    #
    # Now match more liberally, simply from `\n<tag>` to `</tag>\n`
    #
    $text =~ s{
		(			# save in $1
		    ^			# start of line (with /m)
		    (?:\Q$idt\E)?	# optional lead in
		    <($block_tags_b)	# start tag = $2
		    \b			# word break
		    (?:.*\n)*?		# any number of lines, minimally matching
		    .*</\2\s*>		# the matching end tag
		    [ ]*		# trailing spaces
		    (?=\n+|\Z) # followed by a newline or end of document
		)
	    }{
		my $blk = &$blkprc($1, $2);
		my $key = block_id($blk);
		$g_html_blocks{$key} = $blk;
		"\n\n" . $key . "\n\n";
	    }eigmx;

    #
    # Now match any empty block tags that should have been paired
    #
    $text =~ s{
		(			# save in $1
		    ^			# start of line (with /m)
		    (?:\Q$idt\E)?	# optional lead in
		    <($block_tags_c)	# start tag = $2
		    \b			# word break
		    (?:[^<>])*?		#
		    /?>			# the matching end tag
		    [ ]*		# trailing spaces
		    (?=\n+|\Z) # followed by a newline or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }eigmx;

    # Special case just for <hr />. It was easier to make a special case than
    # to make the other regex more complicated.
    $text =~ s{
		(?:
		    (?<=\n)	    # Starting after end of line
		    |		    # or
		    \A		    # the beginning of the doc
		)
		(			# save in $1
		    [ ]{0,$less_than_indent}
		    <(?:hr)		# start tag
		    \b			# word break
		    (?:[^<>])*?		#
		    /?>			# the matching end tag
		    [ ]*
		    (?=\n{1,}|\Z)	# followed by end of line or end of document
		)
	    }{
		my $key = block_id($1);
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }eigx;

    # Special case for standalone XML comments:
    $opt{stripcomments} != 2 &&
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
			(?:
			    (?:[ \t]*\n[ \t]*)?
			    <!--
			    (?:[^-]|(?:-(?!-)))*
			    -->
			)*
		    )
		    [ ]*
		    (?=\n{1,}|\Z)   # followed by end of line or end of document
		)
	    }{
		my $key = block_id($1);
		push(@g_xml_comments, $key)
			if $opt{stripcomments} && $opt{stripcomments} < 3 &&
			   !exists($g_html_blocks{$key});
		$g_html_blocks{$key} = $1;
		"\n\n" . $key . "\n\n";
	    }egx;

    # Special case for standalone XML-like comments:
    $opt{stripcomments} >= 2 &&
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
			(?:[^-]|(?:-(?!->)))*
			-->
			(?:
			    (?:[ \t]*\n[ \t]*)?
			    <!--
			    (?:[^-]|(?:-(?!->)))*
			    -->
			)*
		    )
		    [ ]*
		    (?=\n{1,}|\Z)   # followed by end of line or end of document
		)
	    }{
		my $key = block_id($1);
		push(@g_xml_comments, $key) unless exists($g_html_blocks{$key});
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

    $text = _DoListsAndBlocks($text);

    $text = _DoTables($text);

    # We already ran _HashHTMLBlocks() before, in Markdown(), but that
    # was to escape raw HTML in the original Markdown source. This time,
    # we're escaping the markup we've just created, so that we don't wrap
    # <p> tags around block-level tags.
    $text = _HashHTMLBlocks($text);

    $text = _FormParagraphs($text, $anchors);

    return $text;
}


sub _DoBTListBlocks {
    return _DoBlockQuotes(_DoCodeBlocks(_HashBTCodeBlocks($_[0]))) if $_[0] ne "";
}


sub _DoListBlocks {
    return _DoBlockQuotes(_DoCodeBlocks($_[0])) if $_[0] ne "";
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
    $text =~ s/ {3,}(\n|\z)/<br clear=\"all\"$opt{empty_element_suffix}$1/g;
    $text =~ s/ {2,}\n/<br$opt{empty_element_suffix}\n/g;
    $text =~ s/ ?\\\n/<br$opt{empty_element_suffix}\n/g;
    $text =~ s/ {2,}\z//g;

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
	    $cur_token->[1] =~ s!([*_~])!$g_escape_table{$1}!go;
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
    if (defined($link_loc) &&
	($link_loc =~ m{^#\S*$} || $link_loc =~ m{^(?:http|ftp)s?://\S+$}i)) {
	# Return the new link
	return _MakeATag(_FindFragmentMatch($link_loc), $link_text);
    }
    if (!defined($link_loc)) {
	$link_loc = _RunSpanGamut($link_text);
	$link_loc = _strip(unescapeXML(_StripTags(_UnescapeSpecialChars($link_loc))));
	$link_loc =~ m{^(?:http|ftp)s?://\S+$}i and
	    # Return the new link
	    return _MakeATag($link_loc, $link_text);
    }
    return undef if $link_loc eq "" || $link_text eq "";
    if ($link_loc =~ /^[A-Za-z][A-Za-z0-9+.-]*:/os) {
	# Unrecognized scheme
	return undef;
    }
    if ($opt{wikipat}) {
	my $o = $opt{wikiopt};
	my $img_link = _strip($link_text);
	my $img = 0;
	my $qsfrag = "";
	my $base;
	my $imgopts = undef;
	if ($img_link =~ /^[^#?\s]+\.(?:png|gif|jpe?g|svgz?)$/i) {
	    $base = _wxform($img_link, 1);
	    $img = 1;
	    $imgopts = _ParseWikiImgOpts($link_loc);
	    $imgopts->{imgflag} = \$img;
	} else {
	    $base = $link_loc;
	    if ($link_loc =~ /^(.*?)([?#].*)$/os) {
		($base, $qsfrag) = ($1, $2);
	    }
	    $base = _wxform($base);
	}
	my $result = $opt{wikipat};
	$result =~ s/%\{\}.+$/%{}/os if $img;
	$result =~ s/%\{\}/$base/;
	if ($qsfrag =~ /^([^#]*)(#.+)$/os) {
	    my ($q,$f) = ($1,$2);
	    #$f = _wxform($f) if $f =~ / /;
	    $qsfrag = $q . $f;
	}
	$result .= $qsfrag;
	$result = &{$opt{wikifunc}}($result, \%opt, ($img?$img_link:$link_loc), $base, $qsfrag, $imgopts)
		if ref($opt{wikifunc}) eq 'CODE';
	{
	    use bytes;
	    $result =~ s/%(?![0-9A-Fa-f]{2})/%25/sog;
	    if ($o->{r}) {
		$result =~
		s/([\x00-\x1F <>"{}|\\^`x7F])/sprintf("%%%02X",ord($1))/soge;
	    } else {
		$result =~
		s/([\x00-\x1F <>"{}|\\^`\x7F-\xFF])/sprintf("%%%02X",ord($1))/soge;
	    }
	    $result =~ s/(%(?![0-9A-F]{2})[0-9A-Fa-f]{2})/uc($1)/soge;
	}
	# Return the new link
	return $img ? _MakeIMGTag($result, undef, undef, $imgopts) : _MakeATag($result, $link_text);
    }
    # leave it alone
    return undef;
}


sub _ParseWikiImgOpts {
    my $alts = shift;
    my %o = ();
    # alt= consumes the rest of the line, do it first
    if ($alts =~ /(?:^|,)\s*alt\s*=\s*(.*)$/ios) {
	my $atext = $1;
	$alts = substr($alts, 0, $-[0]);
	$o{alt} = _strip($atext);
    }
    foreach my $kv (split(/\s*,\s*/, lc($alts))) {
	if ($kv =~ /^\s*([^\s]+)\s*=\s*([^\s]+)\s*$/os) {
	    my ($k, $v) = ($1, $2);
	    if (($k eq "width" || $k eq "height") && $v =~ /^\d+$/) {
		$o{$k} = 0+$v if $v > 0;
		next;
	    }
	    if ($k eq "align" && ($v eq "left" || $v eq "right" || $v eq "center")) {
		$o{$k} = $v;
		next;
            }
	}
    }
    return \%o;
}


sub _wxform {
    my ($w, $img) = @_;
    my $o = $opt{wikiopt};
    my $opt_s = $o->{s};
    if (!$img && $opt_s) {
	if (ref($opt_s)) {
	    if ($w =~ m{^(.*)[.]([^./]*)$}) {
		my ($base, $ext) = ($1, $2);
		$w = $base if $opt_s->{lc($ext)};
	    }
	} else {
	    $w =~ s{[.][^./]*$}{};
	}
    }
    $w = uc($w) if $o->{u};
    $w = lc($w) if $o->{l};
    $w =~ s{/+}{%252F}gos if $o->{"%"};
    $w =~ s/ +/%20/gos if $o->{b};
    $w =~ tr{/}{ } if $o->{f};
    $w =~ s{/+}{/}gos if !$o->{f} && !$o->{v};
    if ($o->{d}) {
	$w =~ tr{ }{-};
	$w =~ s/-+/-/gos unless $o->{v};
    } else {
	$w =~ tr{ }{_};
	$w =~ s/_+/_/gos unless $o->{v};
    }
    return $w;
}


# Return a suitably encoded <a...> tag string
# On input NONE of $url, $text or $title should be xmlencoded
# but $url should already be url-encoded if needed, but NOT g_escape_table'd
sub _MakeATag {
    my ($url, $text, $title) = @_;
    defined($url) or $url="";
    defined($text) or $text="";
    defined($title) or $title="";

    $url =~ m"^#" && ref($opt{base_prefix}) eq 'CODE' and $url = &{$opt{base_prefix}}($url);
    my $result = $g_escape_table{'<'}."a href=\"" . _EncodeAttText($url) . "\"";
    $title = _strip($title);
    $text =~ s{<(/?a)}{&lt;$1}sogi;
    $text = _DoItalicsAndBoldAndStrike($text);
    # We've got to encode any of these remaining to avoid
    # conflicting with other italics, bold and strike through and links.
    $text =~ s!([]*_~[])!$g_escape_table{$1}!go;
    $result .= " title=\"" . _EncodeAttText($title) . "\"" if $title ne "";
    return $result . $g_escape_table{'>'} .
	$text . $g_escape_table{'<'}."/a".$g_escape_table{'>'};
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
	    $link_loc = _strip($2);
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
	    ($g_nested_brackets) # id = $3
	  \]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my $link_id	= $3;

	if ($link_id eq "") {
	    # for shortcut links like [this][].
	    $link_id = _RunSpanGamut($link_text);
	    $link_id = unescapeXML(_StripTags(_UnescapeSpecialChars($link_id)));
	}
	$link_id = _strip(lc $link_id);

	if (defined($g_urls{$link_id}) || defined($g_anchors{$link_id})) {
	    my $url = $g_urls{$link_id};
	    defined($url) or $url = $g_anchors{$link_id};
	    $url = _FindFragmentMatch($url);
	    $link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	    $result = _MakeATag(_PrefixURL($url), $link_text, $g_titles{$link_id});
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
	    ($g_nested_parens) # href and optional title = $3
	  \)
	)
    }{
	#my $result;
	my $whole_match = $1;
	my $link_text	= $2;
	my ($url, $title) = _SplitUrlTitlePart($3);

	if (defined($url)) {
	    $url = _FindFragmentMatch($url);
	    $link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	    _MakeATag(_PrefixURL($url), $link_text, $title);
	} else {
	    # The href/title part didn't match the pattern
	    $whole_match;
	}
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
	my $link_id	= _RunSpanGamut($2);
	$link_id	= _strip(lc(unescapeXML(_StripTags(_UnescapeSpecialChars($link_id)))));

	if (defined($g_urls{$link_id}) || defined($g_anchors{$link_id})) {
	    my $url = $g_urls{$link_id};
	    defined($url) or $url = $g_anchors{$link_id};
	    $url = _FindFragmentMatch($url);
	    $link_text = '[' . $link_text . ']' if $link_text =~ /^\d{1,3}$/;
	    $result = _MakeATag(_PrefixURL($url), $link_text, $g_titles{$link_id});
	}
	else {
	    $result = $whole_match;
	}
	$result;
    }xsge;

    return $text;
}


sub _PeelWrapped {
    defined($_[0]) or return undef;
    if (substr($_[0],0,1) eq "(") {
	return substr($_[0], 1, length($_[0]) - (substr($_[0], -1, 1) eq ")" ? 2 : 1));
    }
    return $_[0];
}


sub _SplitUrlTitlePart {
    return ("", undef) if $_[0] =~ m{^\s*$}; # explicitly allowed
    my $u = $_[0];
    $u =~ s/^\s*(['\042])/# $1/;
    if ($u =~ m{
	^		# match beginning
	\s*?
	<?([^\s'\042]\S*?)>? # URL = $1
	(?:		# optional grouping
	  \s+		# must be distinct from URL
	  (['\042]?)	# quote char = $2
	  (.*?)		# Title = $3
	  \2?		# matching quote
	)?		# title is optional
	\s*
	\z		# match end
    }osx) {
	return (undef, undef) if $_[1] && ($1 eq "" || $1 eq "#");
	return (_PeelWrapped($1), $2 ? $3 : _PeelWrapped($3));
    } else {
	return (undef, undef);
    }
}


sub _FindFragmentMatchInternal {
    my ($anchors_id, $url, $undefifnomatch) = @_;
    if (defined($url) && $url =~ /^#\S/) {
	# try very hard to find a match
	my $idbase = _strip(lc(substr($url, 1)));
	my $idbase0 = $idbase;
	my $id = _MakeAnchorId($idbase);
	$undefifnomatch and $url = undef;
	if (defined($$anchors_id{$id})) {
	    $url = $$anchors_id{$id};
	} else {
	    $idbase =~ s/-/_/gs;
	    $id = _MakeAnchorId($idbase);
	    if (defined($$anchors_id{$id})) {
		$url = $$anchors_id{$id};
	    } else {
		$id = _MakeAnchorId($idbase0, 1);
		if (defined($$anchors_id{$id})) {
		    $url = $$anchors_id{$id};
		} else {
		    $id = _MakeAnchorId($idbase, 1);
		    if (defined($$anchors_id{$id})) {
			$url = $$anchors_id{$id};
		    }
		}
	    }
	}
    }
    return $url;
}


sub _FindFragmentMatch {
    return _FindFragmentMatchInternal(\%g_anchors_id, @_);
}


sub _ToUTF8 {
    my $input = shift;
    my $output;
    if (Encode::is_utf8($input) || utf8::decode($input)) {
	$output = $input;
    } else {
	$output = $encoder->decode($input, Encode::FB_DEFAULT);
    }
    return $output;
}


# $_[0] -> HASH ref of anchors (e.g. the "anchors" OUTPUT from Markdown)
# $_[1] -> fragment to resolve, may optionally start with '#'
# An empty string ("") or hash ("#") is returned as-is.
# returns undef if no match otherwise resolved fragment name
# which will start with a '#' if $_[1] started with '#' otherwise will not.
# This function can be used to connect up links to "implicit" anchors.
# All Markdown-format H1-H6 headers have an implicit anchor added
# based on the header item text.  Passing that text to this function
# will cough up the matching implicit anchor if there is one.
sub ResolveFragment
{
    my ($anchors, $frag) = @_;
    defined($frag) or return undef;
    $frag eq "" || $frag eq "#" and return $frag;
    my $hadhash = ($frag =~ s/^#//);
    $frag =~ /^\S/ or return undef;
    ref($anchors) eq 'HASH' or return undef;
    my $ans = _FindFragmentMatchInternal($anchors, '#'._ToUTF8($frag), 1);
    $hadhash || !defined($ans) or $ans =~ s/^#//;
    defined($ans) and utf8::encode($ans);
    return $ans;
}


# Return a suitably encoded <img...> tag string
# On input NONE of $url, $alt or $title should be xmlencoded
# but $url should already be url-encoded if needed, but NOT g_escape_table'd
sub _MakeIMGTag {
    my ($url, $alt, $title, $iopts) = @_;
    defined($url) or $url="";
    defined($alt) or $alt="";
    defined($title) or $title="";
    ref($iopts) eq "HASH" or $iopts = {};
    return "" unless $url ne "";

    my ($w, $h, $lf, $rt) = (0, 0, '', '');
    ($alt, $title) = (_strip($alt), _strip($title));
    if ($title =~ /^(.*)\((<?)([1-9][0-9]*)[xX\xd7]([1-9][0-9]*)(>?)\)$/os) {
	($title, $w, $h, $lf, $rt) = (_strip($1), $3, $4, $2, $5);
    } elsif ($title =~ /^(.*)\((<?)\?[xX\xd7]([1-9][0-9]*)(>?)\)$/os) {
	($title, $h, $lf, $rt) = (_strip($1), $3, $2, $4);
    } elsif ($title =~ /^(.*)\((<?)([1-9][0-9]*)[xX\xd7]\?(>?)\)$/os) {
	($title, $w, $lf, $rt) = (_strip($1), $3, $2, $4);
    } elsif ($title =~ /^(.*)\((?!\))(<?)(>?)\)$/os) {
	($title, $lf, $rt) = (_strip($1), $2, $3);
    }
    $iopts->{align} = "center" if $lf && $rt;
    $iopts->{align} = "left"   if $lf && !$rt;
    $iopts->{align} = "right"  if !$lf && $rt;
    $iopts->{width}  = $w if $w != 0;
    $iopts->{height} = $h if $h != 0;
    $iopts->{alt} = $alt if $alt ne "";
    $iopts->{title} = $title if $title ne "";
    my $iopt = sub { defined($iopts->{$_[0]}) ? $iopts->{$_[0]} : (@_ > 1 ? $_[1] : "") };
    my $result = '';
    $result .= $g_escape_table{'<'}."center".$g_escape_table{'>'}
	if &$iopt("align") eq "center";
    $result .= $g_escape_table{'<'}."img src=\"" . _EncodeAttText($url) . "\"";
    $result .= " align=\"left\"" if &$iopt("align") eq "left";
    $result .= " align=\"right\"" if &$iopt("align") eq "right";
    $result .= " alt=\"" . _EncodeAttText($iopts->{alt}) . "\"" if &$iopt("alt") ne "";
    $result .= " width=\"" . $iopts->{width} . "\"" if &$iopt("width",0) != 0;
    $result .= " height=\"" . $iopts->{height} . "\"" if &$iopt("height",0) != 0;
    $result .= " title=\"" . _EncodeAttText($iopts->{title}) . "\"" if &$iopt("title") ne "";
    $result .= " /" unless $opt{empty_element_suffix} eq ">";
    $result .= $g_escape_table{'>'};
    $result .= $g_escape_table{'<'}."/center".$g_escape_table{'>'}
	if &$iopt("align") eq "center";
    return $result;
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
	    ($g_nested_brackets) # alt text = $2
	  \]

	  [ ]?		# one optional space
	  (?:\n[ ]*)?	# one optional newline followed by spaces

	  \[
	    ($g_nested_brackets) # id = $3
	  \]

	)
    }{
	my $result;
	my $whole_match = $1;
	my $alt_text	= $2;
	my $link_id	= $3;

	$link_id ne "" or $link_id = $alt_text; # for shortcut links like ![this][].
	$link_id = _strip(lc $link_id);

	if (defined $g_urls{$link_id}) {
	    $result = _MakeIMGTag(
		_PrefixURL($g_urls{$link_id}), $alt_text, $g_titles{$link_id});
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
	    ($g_nested_brackets) # alt text = $2
	  \]
	  \(		# literal paren
	    ($g_nested_parens) # src and optional title = $3
	  \)
	)
    }{
	my $whole_match = $1;
	my $alt_text	= $2;
	my ($url, $title) = _SplitUrlTitlePart($3, 1);
	defined($url) ?  _MakeIMGTag(_PrefixURL($url), $alt_text, $title) : $whole_match;
    }xsge;

    #
    # Finally, handle reference-style implicitly labeled links: ![alt text]
    #
    $text =~ s{
	(		# wrap whole match in $1
	  !\[
	    ($g_nested_brackets) # alt text = $2
	  \]
	)
    }{
	my $result;
	my $whole_match = $1;
	my $alt_text	= $2;
	my $link_id	= lc(_strip($alt_text));

	if (defined $g_urls{$link_id}) {
	    $result = _MakeIMGTag(
		_PrefixURL($g_urls{$link_id}), $alt_text, $g_titles{$link_id});
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
    $text = escapeXML(_strip($text));
    # We've got to encode these to avoid conflicting
    # with italics, bold and strike through.
    $text =~ s!([*_~:])!$g_escape_table{$1}!go;
    return $text;
}


sub _MakeAnchorId {
    use bytes;
    my ($link, $strip) = @_;
    $link = lc($link);
    if ($strip) {
	$link =~ s/\s+/_/gs;
	$link =~ tr/-a-z0-9_//cd;
    } else {
	$link =~ tr/-a-z0-9_/_/cs;
    }
    return '' unless $link ne '';
    $link = "_".$link."_";
    $link =~ s/__+/_/gs;
    $link = "_".md5_hex($link)."_" if length($link) > 66;
    return $link;
}


sub _GetNewAnchorId {
    my $link = _strip(lc(shift));
    return '' if $link eq "" || defined($g_anchors{$link});
    my $id = _MakeAnchorId($link);
    return '' unless $id;
    $g_anchors{$link} = '#'.$id;
    $g_anchors_id{$id} = $g_anchors{$link};
    if ($id =~ /-/) {
	my $id2 = $id;
	$id2 =~ s/-/_/gs;
	$id2 =~ s/__+/_/gs;
	defined($g_anchors_id{$id2}) or $g_anchors_id{$id2} = $g_anchors{$link};
    }
    my $idd = _MakeAnchorId($link, 1);
    if ($idd) {
	defined($g_anchors_id{$idd}) or $g_anchors_id{$idd} = $g_anchors{$link};
	if ($idd =~ /-/) {
	    my $idd2 = $idd;
	    $idd2 =~ s/-/_/gs;
	    $idd2 =~ s/__+/_/gs;
	    defined($g_anchors_id{$idd2}) or $g_anchors_id{$idd2} = $g_anchors{$link};
	}
    }
    $id;
}


sub _DoHeaders {
    my ($text, $anchors) = @_;
    my $h1;
    my $geth1 = $anchors && !defined($opt{h1}) ? sub {
	return unless !defined($h1);
	my $h = shift;
	$h1 = $h if $h ne "";
    } : sub {};

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
	    ((?:(?:(?<![#])[^\s]|[^#\s]).*?)?) # $2 = Header text
	    [ ]*
	    \n+
	}{
	    my $h_level = length($1);
	    my $h = $2;
	    $h =~ s/#+$//;
	    $h =~ s/\s+$//;
	    my $rsg = _RunSpanGamut($h);
	    $h = _strip(unescapeXML(_StripTags(_UnescapeSpecialChars($rsg))));
	    my $id = $h eq "" ? "" : _GetNewAnchorId($h);
	    $id = " id=\"$id\"" if $id ne "";
	    &$geth1($h) if $h_level == 1;
	    "<h$h_level$id>" . _AutoHeaderFlag($h_level) . $rsg . "</h$h_level>\n\n";
	}egmx;

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
	my $rsg = _RunSpanGamut($h);
	$h = _strip(unescapeXML(_StripTags(_UnescapeSpecialChars($rsg))));
	my $id = $h eq "" ? "" : _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	&$geth1($h);
	"<h1$id>" . _AutoHeaderFlag(1) . $rsg . "</h1>\n\n";
    }egmx;

    $text =~ s{ ^(?:-+[ ]*\n)?[ ]*(.+?)[ ]*\n-+[ ]*\n+ }{
	my $h = $1;
	my $rsg = _RunSpanGamut($h);
	$h = _strip(unescapeXML(_StripTags(_UnescapeSpecialChars($rsg))));
	my $id = $h eq "" ? "" : _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h2$id>" . _AutoHeaderFlag(2) . $rsg . "</h2>\n\n";
    }egmx;

    $text =~ s{ ^(?:~+[ ]*\n)?[ ]*(.+?)[ ]*\n~+[ ]*\n+ }{
	my $h = $1;
	my $rsg = _RunSpanGamut($h);
	$h = _strip(unescapeXML(_StripTags(_UnescapeSpecialChars($rsg))));
	my $id = $h eq "" ? "" : _GetNewAnchorId($h);
	$id = " id=\"$id\"" if $id ne "";
	"<h3$id>" . _AutoHeaderFlag(3) . $rsg . "</h3>\n\n";
    }egmx;

    $opt{h1} = $h1 if defined($h1) && $h1 ne "";
    return $text;
}


sub _AutoHeaderFlag {
    my $level = shift;
    my $auto = $opt{auto_number} || 0;
    return '' unless 1 <= $level && $level <= $auto;
    return "\34".chr(0x30+$level);
}


sub _AutoHeaderNum {
    my $level = shift;
    my $auto = $opt{auto_number} || 0;
    return '' unless 1 <= $level && $level <= $auto;
    pop(@autonum) while @autonum > $level;
    push(@autonum, 1) while @autonum < $level - 1;
    $autonum[$level - 1] += 1;
    return join('.', @autonum).' ';
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


sub _DoListsAndBlocks {
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
	defined($first_marker) or return $list;
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
	my $idt = "\027" x $g_list_level;
	$result = "$idt<$list_type$list_att$list_class>\n$list_incr" . $result . "$idt</$list_type>\n\n";
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
	my $parse = $text;
	$text = "";
	pos($parse) = 0;
	while ($parse =~ /\G(?s:.)*?^$whole_list/gmc) {
	    my @captures = ($1, $2, $3, $4);
	    if ($-[1] > $-[0]) {
		$text .= _DoBTListBlocks(substr($parse, $-[0], $-[1] - $-[0]));
	    }
	    $text .= &$list_item_sub(@captures);
	}
	$text .= _DoBTListBlocks(substr($parse, pos($parse))) if pos($parse) < length($parse);
    }
    else {
	my $parse = $text;
	$text = "";
	pos($parse) = 0;
	while ($parse =~ m{\G(?s:.)*?
		(?: (?<=\n\n) |
		    \A\n? |
		    (?<=:\n) |
		    (?:(?<=\n) # a list starts with one unordered marker line
		       (?=[ ]{0,$less_than_indent}$marker_ul[ ])) |
		    (?:(?<=\n) # or two ordered marker lines in a row
		       (?=[ ]{0,$less_than_indent}$marker_ol[ ].*\n\n?
		          [ ]{0,$less_than_indent}$marker_ol[ ])) |
		    (?:(?<=\n) # or any marker and a sublist marker
		       (?=[ ]{0,$less_than_indent}$marker_any[ ].*\n\n?
		          [ ]{$indent,$less_than_double_indent}$marker_any[ ]))
		)
		$whole_list
	    }gmcx) {
	    my @captures = ($1, $2, $3, $4);
	    if ($-[1] > $-[0]) {
		$text .= _DoListBlocks(substr($parse, $-[0], $-[1] - $-[0]));
	    }
	    $text .= &$list_item_sub(@captures);
	}
	$text .= _DoListBlocks(substr($parse, pos($parse))) if pos($parse) < length($parse);
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
    my $idt = "\027" x $g_list_level;
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

	if ($list_type eq "ul" && !$leading_item_space && $item =~ /^\[([ xX\xd7])\] +(.*)$/s) {
	    my $checkmark = $1;
	    $item = $2;
	    my ($checkbox_class, $checkbox_val);
	    if ($checkmark ne " ") {
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

	if ($item =~ /^(.+)/) {
	    my $ml_text = $1;
	    my $ml_len = length($1);
	    my $ml_sub = sub {my $ml_mk = shift; $ml_mk =~ s!([-+*.\)])!$g_escape_table{$1}!go; $ml_mk};
	    $ml_text =~ s/(?:(?<= )|\A)(${marker_any})(?= )/&$ml_sub($1)/ge;
	    $item = $ml_text . substr($item, $ml_len);
	}
	if ($leading_line or ($item =~ m/\n{2,}/)) {
	    $item = _RunBlockGamut(_Outdent($item));
	    $item =~ s{(</[OUou][Ll]>)\s*\z}{$1} and $item .= "\n$idt<span style=\"display:none\">&#160;</span>";
	}
	else {
	    # Recursion for sub-lists:
	    $item = _DoListsAndBlocks(_Outdent($item));
	    chomp $item;
	    $item = _RunSpanGamut($item);
	}

	# Append to $result
	$result .= "$incr$idt<li$liatt>" . $checkbox . $item . "$idt</li>\n";
    }
    if ($fancy) {
	# remove "incrlevel=$g_list_level " parts
	$result =~ s{<span incrlevel=$g_list_level class="$opt{style_prefix}ol-incr((?:-\d{1,2})?)">}
	    {$idt<span class="$opt{style_prefix}ol-incr$1">}g;
    } else {
	# remove the $g_list_level incr spans entirely
	$result =~ s{<span incrlevel=$g_list_level class="$opt{style_prefix}ol-incr(?:-\d{1,2})?"></span>\n}{}g;
	# remove the class="$opt{style_prefix}lc-greek" if first_marker is greek
	$result =~ s{(<li[^>]*?) class="$opt{style_prefix}lc-greek">}{$1>}g
	    if defined($first_marker_type) && $first_marker_type eq "a" && $first_marker =~ /^$greek_lower/o;
    }

    # Anything left over (similar to $') goes into result, but this should always be empty
    $result .= _RunBlockGamut(substr($list_str, pos($list_str))) if pos($list_str) < length($list_str);

    $g_list_level--;

    # After all that, if we only got an ordered list with a single item
    # and its first marker is a four-digit number >= 1492 and <= 2999
    # or an UPPERCASE letter, then pretend we didn't see any list at all.

    if ($first_marker_type && $first_marker_num + 1 == $next_num) {
	if (($first_marker_type eq "1" && $first_marker_num >= 1492 && $first_marker_num <= 2999) ||
	    ($first_marker_type eq "A" && !$fancy)) {
	    return (undef, undef, undef);
	}
    }

    return ($result, $first_marker, $fancy);
}


sub _DoCodeBlocks {
#
#   Process Markdown `<pre><code>` blocks.
#

    my $text = shift;
    my $less_than_indent = $opt{indent_width} - 1;

    $text =~ s{
	    (\n\n|\A\n?)
	    (		# $2 = the code block -- one or more lines, starting with indent_width spaces
	      (?:
		(?:[ ]{$opt{indent_width}})  # Lines must start with indent_width of spaces
		.*\n+
	      )+
	    )
	    (?:(?=(^[ ]{0,$less_than_indent}\S.*))|\Z) # Lookahead for non-space at line-start, or end of doc
	}{&{sub{
	    my ($prefix, $codeblock, $n) = ($1, $2, $3);

	    if (defined($n) && length($n) && (()=($codeblock =~ /\n/g)) == 1 && _IsTableStart($codeblock.$n."\n")) {
		return $prefix.$codeblock;
	    }

	    $codeblock =~ s/\n\n\n/\n\n/g; # undo "paragraph for last list item" change
	    $codeblock = _EncodeCode(_Outdent($codeblock));
	    $codeblock =~ s/\A\n+//; # trim leading newlines
	    $codeblock =~ s/\s+\z//; # trim trailing whitespace

	    my $result = "<div class=\"$opt{style_prefix}code\"><pre style=\"display:none\"></pre><pre><code>"
		. $codeblock . "\n</code></pre></div>";
	    my $key = block_id($result, 2);
	    $g_code_blocks{$key} = $result;
	    "\n\n" . $key . "\n\n";
	}}}egmx;

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
    s!([*_~{}\[\]\\])!$g_escape_table{$1}!go;

    return $_;
}


sub _DoItalicsAndBoldAndStrike {
    my $text = shift;

    my $doital1 = sub {
	my $text = shift;
	$text =~ s{ \* (?=\S) (.+?) (?<=\S) \* }
	    {<em>$1</em>}gsx;
	# We've got to encode any of these remaining to
	# avoid conflicting with other italics and bold.
	$text =~ s!([*])!$g_escape_table{$1}!go;
	$text;
    };
    my $doital2 = sub {
	my $text = shift;
	$text =~ s{ (?<!\w) _ (?=\S) (.+?) (?<=\S) _ (?!\w) }
	    {<em>$1</em>}gsx;
	# We've got to encode any of these remaining to
	# avoid conflicting with other italics and bold.
	$text =~ s!([_])!$g_escape_table{$1}!go;
	$text;
    };

    # <strong> must go first:
    $text =~ s{ \*\* (?=\S) (.+?[*_]*) (?<=\S) \*\* }
	{"<strong>".&$doital1($1)."</strong>"}gsex;
    $text =~ s{ (?<!\w) __ (?=\S) (.+?[*_]*) (?<=\S) __ (?!\w) }
	{"<strong>".&$doital2($1)."</strong>"}gsex;

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
		.*\n		# rest of the first line
	      (.+\n)*		# subsequent consecutive lines
	      \n*		# blanks
	    )+
	  )
	}{
	    my $bq = $1;
	    $bq =~ s/^[ ]*>[ ]?//gm; # trim one level of quoting
	    $bq =~ s/^[ ]+$//mg;	 # trim whitespace-only lines
	    $bq = _RunBlockGamut($bq);	 # recurse

	    $bq =~ s/^/\027/mg;
	    "<blockquote>\n$bq\n</blockquote>\n\n";
	}egmx;


    return $text;
}


my ($LEAD, $TRAIL, $LEADBAR, $LEADSP, $COLPL, $SEP);
BEGIN {
    $LEAD = qr/(?>[ ]*(?:\|[ ]*)?)/o;
    $TRAIL = qr/[ ]*(?<!\\)\|[ ]*/o;
    $LEADBAR = qr/(?>[ ]*\|[ ]*)/o;
    $LEADSP = qr/(?>[ ]*)/o;
    $COLPL = qr/(?:[^\n|\\]|\\(?:(?>[^\n])|(?=\n|$)))+/o;
    $SEP = qr/[ ]*:?-+:?[ ]*/o;
}

sub _IsTableStart {
    my $text = shift;
    my $ans = 0;

    if ($text =~ m{
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
			}mx) {
	my ($h, $s) = ($1, $2);
	_SplitTableRow($h) == _SplitTableRow($s) and $ans = 1;
    }

    return $ans;
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
	    my $nohdr = "";
	    $nohdr = " $opt{style_prefix}table-nohdr" if join("", @heads) eq "";
	    my $tab ="\n<table border=\"1\" cellspacing=\"0\" cellpadding=\"2\" class=\"$opt{style_prefix}table$nohdr\">\n";
	    $tab .=
		"  <tr class=\"$opt{style_prefix}row-hdr\">" . _MakeTableRow("th", \@align, @heads) . "</tr>\n"
		unless $nohdr;
	    my $cnt = 0;
	    my @classes = ("class=\"$opt{style_prefix}row-even\"", "class=\"$opt{style_prefix}row-odd\"");
	    $tab .= "  <tr " . $classes[++$cnt % 2] . ">" . _MakeTableRow("td", \@align, @$_) . "</tr>\n"
		    foreach (_SplitMergeRows($rows));
	    $tab .= "</table>\n\n";
	} else {
	    $w;
	}
    }egmx;

    return $text;
}


sub _SplitMergeRows {
    my @rows = ();
    my ($mergeprev, $mergenext) = (0,0);
    foreach (split(/\n/, $_[0])) {
	$mergeprev = $mergenext;
	$mergenext = 0;
	my @cols = _SplitTableRow($_);
	if (_endswithbareslash($cols[$#cols])) {
	    my $last = $cols[$#cols];
	    substr($last, -1, 1) = "";
	    $last =~ s/[ ]+$//;
	    $cols[$#cols] = $last;
	    $mergenext = 1;
	}
	if ($mergeprev) {
	    for (my $i = 0; $i <= $#cols; ++$i) {
		my $cell = $rows[$#rows]->[$i];
		defined($cell) or $cell = "";
		$rows[$#rows]->[$i] = _MergeCells($cell, $cols[$i]);
	    }
	} else {
	    push(@rows, [@cols]);
	}
    }
    return @rows;
}


sub _endswithbareslash {
    return 0 unless substr($_[0], -1, 1) eq "\\";
    my @parts = split(/\\\\/, $_[0], -1);
    return substr($parts[$#parts], -1, 1) eq "\\";
}


sub _MergeCells {
    my ($c1, $c2) = @_;
    return $c1 if $c2 eq "";
    return $c2 if $c1 eq "";
    return $c1 . " " . $c2;
}


sub _SplitTableRow {
    my $row = shift;
    $row =~ s/^$LEAD//;
    $row =~ s/$TRAIL$//;
    $row =~ s!\\\\!$g_escape_table{'\\'}!go; # Must process escaped backslashes first.
    $row =~ s!\\\|!$g_escape_table{'|'}!go; # Then do \|
    my @elems = map {
      s!$g_escape_table{'|'}!|!go;
      s!$g_escape_table{'\\'}!\\\\!go;
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
    my ($text, $anchors) = @_;

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
	    s/^([ ]*)/$g_start_p/;
	    $_ .= $g_close_p;
	}
    }

    #
    # Strip standalone XML comments if requested
    #
    if ($anchors && $opt{stripcomments} && @g_xml_comments) {
	my %xml_comment = ();
	$xml_comment{$_} = 1 foreach @g_xml_comments;
	my @grafs2 = ();
	do { push(@grafs2, $_) unless $xml_comment{$_} } foreach @grafs;
	@grafs = @grafs2;
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


# %ok_tag_name declared previously
my $g_possible_tag_name;
BEGIN {
    # note: length("blockquote") == 10
    $g_possible_tag_name = qr/(?i:[a-z]{1,10}|h[1-6]|\020)/o;
    %ok_tag_name = map({$_ => 1} "\20", qw(
	a abbr acronym address area
	b basefont bdo big blockquote br
	caption center cite code col colgroup
	dd del dfn div dl dt
	em
	font
	h1 h2 h3 h4 h5 h6 hr
	i img ins
	kbd
	li
	map
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

	return _ProcessURLTag("href", $tag, 1) if $tag =~ /^<a\s/i;
	return _ProcessURLTag("src", $tag) if $tag =~ /^<img\s/i;
	return $tag;
    }
    $tag =~ s/^</&lt;/;
    return $tag;
}

# Strip out all tags that _DoTag would match
sub _StripTags {
    my $text = shift;
    my $_StripTag = sub {
	my $tag = shift;
	return $tag if $tag =~ /^<[?\$!]/;
	if (($tag =~ m{^<($g_possible_tag_name)(?:[\s>]|/>$)} || $tag =~ m{^</($g_possible_tag_name)\s*>}) &&
	    $ok_tag_name{lc($1)}) {

	    return ""; # strip it out
	}
	return $tag;
    };
    $text =~ s{(<[^>]*>)}{&$_StripTag($1)}ige;
    return $text;
}

my %univatt;	# universally allowed attribute names
my %tagatt;	# per-element allowed attribute names
my %tagmt;	# empty element tags
my %tagocl;	# non-empty elements with optional closing tag
my %tagacl;	# which %tagocl an opening %tagocl will close
my %tagblk;	# block elements
my %taginl;	# inline markup tags which trigger an auto <p> reopen
my %taga1p;	# open tags which require at least one attribute
my %lcattval;	# names of attribute values to lowercase
my %impatt;	# names of "implied" attributes
BEGIN {
    %univatt = map({$_ => 1} qw(class dir id lang style title xml:lang));
    %tagatt = (
	'a' => { map({$_ => 1} qw(href name rel target)) },
	'area' => { map({$_ => 1} qw(alt coords href nohref shape)) },
	'basefont' => { map({$_ => 1} qw(color face size)) },
	'br' => { map({$_ => 1} qw(clear)) },
	'caption' => { map({$_ => 1} qw(align)) },
	'col' => { map({$_ => 1} qw(align char charoff span width valign)) },
	'colgroup' => { map({$_ => 1} qw(align char charoff span width valign)) },
	'dir' => { map({$_ => 1} qw(compact)) },
	'div' => { map({$_ => 1} qw(align)) },
	'dl' => { map({$_ => 1} qw(compact)) },
	'font' => { map({$_ => 1} qw(color face size)) },
	'h1' => { map({$_ => 1} qw(align)) },
	'h2' => { map({$_ => 1} qw(align)) },
	'h3' => { map({$_ => 1} qw(align)) },
	'h4' => { map({$_ => 1} qw(align)) },
	'h5' => { map({$_ => 1} qw(align)) },
	'h6' => { map({$_ => 1} qw(align)) },
	'hr' => { map({$_ => 1} qw(align noshade size width)) },
	# NO server-side image maps, therefore NOT ismap !
	'img' => { map({$_ => 1} qw(align alt border height hspace src usemap vspace width)) },
	'li' => { map({$_ => 1} qw(compact type value)) },
	'map' => { map({$_ => 1} qw(name)) },
	'menu' => { map({$_ => 1} qw(compact)) },
	'ol' => { map({$_ => 1} qw(compact start type)) },
	'p' => { map({$_ => 1} qw(align)) },
	'pre' => { map({$_ => 1} qw(width)) },
	'table' => { map({$_ => 1} qw(align bgcolor border cellpadding cellspacing frame rules summary width)) },
	'tbody' => { map({$_ => 1} qw(align char charoff valign)) },
	'tfoot' => { map({$_ => 1} qw(align char charoff valign)) },
	'thead' => { map({$_ => 1} qw(align char charoff valign)) },
	'td' => { map({$_ => 1} qw(align bgcolor char charoff colspan height nowrap rowspan valign width)) },
	'th' => { map({$_ => 1} qw(align bgcolor char charoff colspan height nowrap rowspan valign width)) },
	'tr' => { map({$_ => 1} qw(align bgcolor char charoff valign)) },
	'ul' => { map({$_ => 1} qw(compact type)) }
    );
    %tagmt = map({$_ => 1} qw(area basefont br col hr img));
    %tagocl = map({$_ => 1} qw(colgroup dd dt li p tbody td tfoot th thead tr));
    %tagacl = (
	'colgroup' => \%tagocl,
	'dd' => { map({$_ => 1} qw(colgroup dd dt li p)) },
	'dt' => { map({$_ => 1} qw(colgroup dd dt li p)) },
	'li' => { map({$_ => 1} qw(colgroup dd dt li p)) },
	'tbody' => \%tagocl,
	'td' => { map({$_ => 1} qw(colgroup dd dt li p td th)) },
	'tfoot' => \%tagocl,
	'th' => { map({$_ => 1} qw(colgroup dd dt li p td th)) },
	'thead' => \%tagocl,
	'tr' => { map({$_ => 1} qw(colgroup dd dt li p td th tr)) },
    );
    %tagblk = map({$_ => 1} qw(address blockquote center div dl h1 h2 h3 h4 h5 h6 hr ol p pre table ul));
    %taginl = map({$_ => 1} qw(a abbr acronym b basefont bdo big br cite code dfn em font i
			       img kbd map q s samp small span strike strong sub sup tt u var));
    %impatt = map({$_ => 1} qw(checked compact ismap nohref noshade nowrap));
    %lcattval = map({$_ => 1} qw(
	align border cellpadding cellspacing checked clear color colspan
	compact coords height hspace ismap nohref noshade nowrap rowspan size
	span shape valign vspace width
    ));
    %taga1p = map({$_ => 1} qw(a area bdo img map));
}


# _SanitizeTags
#
# Inspect all '<'...'>' tags in the input and HTML encode those things
# that cannot possibly be tags and at the same time sanitize them.
#
# $1 => text to process
# <= sanitized text
sub _SanitizeTags {
    my ($text, $validate, $htmlauto) = @_;
    $text =~ s/\s+$//;
    $text ne "" or return "";
    my @stack = ();
    my $ans = "";
    my $end = length($text);
    pos($text) = 0;
    my ($autoclose, $autoclopen);
    my $lastmt = "";
    my $reopenp = 0;
    $autoclose = $htmlauto ? sub {
	my $s = $_[0] || "";
	while (@stack &&
	       ($stack[$#stack]->[0] ne $s || $_[1] && !$stack[$#stack]->[2]) &&
	       $tagocl{$stack[$#stack]->[0]}) {
	    $ans .= "</" . $stack[$#stack]->[0] . ">";
	    pop(@stack);
	}
    } : sub {} if $validate;
    $autoclopen = $htmlauto ? sub {
	my $s = $_[0] || "";
	my $c;
	if ($tagblk{$s}) {$c = {p=>1}}
	elsif ($tagocl{$s}) {$c = $tagacl{$s}}
	else {return}
	my $clp = 0;
	while (@stack && $c->{$stack[$#stack]->[0]}) {
	    $clp = 0;
	    if ($stack[$#stack]->[2] &&
		$stack[$#stack]->[1]+3 eq $_[1]) {
		$ans .= "</\20>";
	    } else {
		$ans .= "</" . $stack[$#stack]->[0] . ">";
	    }
	    if ($stack[$#stack]->[2]) {
		$stack[$#stack]->[0] = "\20";
	    } else {
		$clp = $s ne "p" && $stack[$#stack]->[0] eq "p";
		pop(@stack);
	    }
	}
	$clp;
    } : sub {} if $validate;
    while (pos($text) < $end) {
	if ($text =~ /\G(\s+)/gc) {
	    $ans .= $1;
	    next;
        }
	if ($text =~ /\G([^<]+)/gc) {
	    if ($validate && @stack && $stack[$#stack]->[0] eq "\20") {
		push(@stack,["p",pos($text)-length($1)]);
		$reopenp = 0;
		$ans .= "<p>";
	    }
	    $reopenp && do {
		push(@stack,["p",pos($text)-length($1)]);
		$reopenp = 0;
		$ans .= "<p>";
	    };
	    $ans .= _EncodeAmps($1);
	    $lastmt = "";
	    next;
	}
	my $tstart = pos($text);
	if ($opt{stripcomments} != 2 &&
	    $text =~ /\G(<!--(?:[^-]|(?:-(?!-)))*-->)/gc) {
	    # pass "comments" through unless stripping them
	    if ($opt{stripcomments} && $opt{stripcomments} < 3) {
		# strip any trailing whitespace + \n after comment if present
		$text =~ /\G[ \t]*\n/gc;
	    } else {
		# pass the "comment" on through
		$ans .= $1;
	    }
	    next;
	}
	if ($opt{stripcomments} >= 2 &&
	    $text =~ /\G(<!--(?:[^-]|(?:-(?!->)))*-->)/gc) {
	    # strip any trailing whitespace + \n after lax comment if present
	    $text =~ /\G[ \t]*\n/gc;
	    next;
	}
	if ($text =~ /\G(<[^>]*>)/gc) {
	    my $tag = $1;
	    my $tt;
	    if (($tag =~ m{^<($g_possible_tag_name)(?:[\s>]|/>$)} ||
		 $tag =~ m{^</($g_possible_tag_name)\s*>}) &&
		$ok_tag_name{$tt=lc($1)})
	    {
		my ($stag, $styp, $autocloseflag) = _Sanitize($tag);
		if ($styp == 2 && $lastmt eq $tt) {
		    $lastmt = "";
		    next;
		}
		$lastmt = $styp == -3 ? $tt : "";
		$tt = "p" if $autocloseflag;
		if ($validate && $styp) {
		    my $clp = &$autoclopen($tt, $tstart) if $styp != 2;
		    if ($styp == 1) {
			$reopenp && $taginl{$tt} and do {
			    push(@stack,["p",$tstart]);
			    $ans .= "<p>";
			};
			push(@stack,[$tt,$tstart,$autocloseflag,$clp]);
			$reopenp = 0;
		    } elsif ($styp == 2) {
			$reopenp && ($tt eq "p" || $tt eq "\20") and do {
			    $reopenp = 0;
			    next;
			};
			&$autoclose($tt, $autocloseflag);
			my $mtstkchk = sub {
			    !@stack and _xmlfail("closing tag $tt without matching open at " .
				_linecol($tstart, $text));
			};
			&$mtstkchk;
			if ($autocloseflag && $stack[$#stack]->[0] eq "\20") {
			    pop(@stack);
			    $stag = "";
			} elsif ($stack[$#stack]->[0] eq $tt) {
			    $stack[$#stack]->[3] and $reopenp = 1;
			    pop(@stack);
			} else {
			    pop(@stack) while @stack && $stack[$#stack]->[0] eq "\20";
			    &$mtstkchk;
			    my @i = @{$stack[$#stack]};
			    _xmlfail("opening tag $i[0] at " . _linecol($i[1], $text) .
				" mismatch with closing tag $tt at " . _linecol($tstart, $text));
			}
		    }
		}
		$ans .= $stag;
		next;
	    } else {
		$tag =~ s/^</&lt;/;
		$ans .= _EncodeAmps($tag);
		$lastmt = "";
		next;
	    }
	}
	# can only get here if "\G" char is an unmatched "<"
	pos($text) += 1;
	$ans .= "&lt;";
	$lastmt = "";
    }
    &$autoclose if $validate;
    if ($validate && @stack) {
	my @errs;
	my $j;
	for ($j = 0; $j <= $#stack; ++$j) {
		my @i = @{$stack[$j]};
		next if $i[0] eq "\20";
		unshift(@errs, "opening tag $i[0] without matching close at " .
			    _linecol($i[1], $text));
	}
	_xmlfail(@errs) unless !@errs;
    }
    # Remove any unwanted extra leading <p></p> sections
    $ans =~ s{<p></\020>}{}gs if $validate;

    return $ans."\n";
}


sub _linecol {
	my ($pos, $txt) = @_;
	pos($txt) = 0;
	my ($l, $p);
	$l = 1 + $opt{firstline};
	++$l while ($p = pos($txt)), $txt =~ /\G[^\n]*\n/gc && pos($txt) <= $pos;
	return "line $l col " . (1 + ($pos - $p));
}


sub _xmlfail {
	die join("", map("$_\n", @_));
}


sub _Sanitize {
    my $tag = shift;
    my $seenatt = {};
    if ($tag =~ m{^</}) {
	my $autocloseflag = undef;
	$autocloseflag = 1, $tag="</p>" if $tag eq "</\20>";
	$tag =~ tr/\t\n\f\r //d; # remove whitespace
	return (lc($tag),2,$autocloseflag);
    }
    if ($tag =~ m{^<([^\s</>]+)\s+}gs) {
	my $tt = lc($1);
	my $autocloseflag = undef;
	$autocloseflag = 1, $tt="p" if $tt eq "\20";
	my $out = "<" . $tt . " ";
	my $ok = $tagatt{$tt};
	ref($ok) eq "HASH" or $ok = {};
	my $atc = 0;
	while ($tag =~ m{\G\s*([^\s\042\047</>=]+)((?>=)|\s*)}gcs) {
	    my ($a,$s) = ($1, $2);
	    if ($s eq "" && substr($tag, pos($tag), 1) =~ /^[\042\047]/) {
		# pretend the "=" sign wasn't overlooked
		$s = "=";
	    }
	    if (substr($s,0,1) ne "=") {
		# it's one of "those" attributes (e.g. compact) or not
		# _SanitizeAtt will fix it up if it is
		$out .= _SanitizeAtt($a, '""', $ok, $seenatt, $tt);
		++$atc;
		next;
	    }
	    if ($tag =~ /\G([\042\047])((?:(?!\1)(?!<).)*)\1\s*/gcs) {
		$out .= _SanitizeAtt($a, $1.$2.$1, $ok, $seenatt, $tt);
		++$atc;
		next;
	    }
	    if ($tag =~ m{\G([\042\047])((?:(?!\1)(?![<>])(?!/>).)*)}gcs) {
		# what to do what to do what to do
		# trim trailing \s+ and magically add the missing quote
		my ($q, $v) = ($1, $2);
		$v =~ s/\s+$//;
		$out .= _SanitizeAtt($a, $q.$v.$q, $ok, $seenatt, $tt);
		++$atc;
		next;
	    }
	    if ($tag =~ m{\G([^\s<>]+)\s*}gcs) {
		# auto quote it
		my $v = $1;
		$v =~ s/\042/&quot;/go;
		$out .= _SanitizeAtt($a, '"'.$v.'"', $ok, $seenatt, $tt);
		++$atc;
		next;
	    }
	    # give it an empty value
	    $out .= _SanitizeAtt($a, '""', $ok, $seenatt, $tt);
	    ++$atc;
        }
	my $sfx = substr($tag, pos($tag));
	$out =~ s/\s+$//;
	my $typ = 1;
	if ($tagmt{$tt}) {
	    $typ = ($sfx =~ m,/>$,) ? 3 : -3;
	    $out .= $opt{empty_element_suffix};
	    return ("&lt;" . substr($tag,1), 0) if !$atc && $taga1p{$tt};
	} else {
	    if ($sfx =~ m,/>$,) {
		return ("&lt;" . substr($tag,1), 0) if !$atc && $taga1p{$tt};
		$typ = 3;
	    } else {
		return ("&lt;" . substr($tag,1), 0) if !$atc && $taga1p{$tt};
	    }
	    $out .= ">";
	    $out .= "</$tt>" if $typ == 3;
	}
	return ($out,$typ,$autocloseflag);
    } elsif ($tag =~ /^<([^\s<\/>]+)/s) {
	my $tt = lc($1);
	return ("&lt;" . substr($tag,1), 0) if $taga1p{$tt};
	if ($tagmt{$tt}) {
	    my $typ = ($tag =~ m,/>$,) ? 3 : -3;
	    return ("<" . $tt . $opt{empty_element_suffix}, $typ);
	} elsif ($tag =~ m,/>$,) {
	    return ("<" . $tt . "></" . $tt . ">", 3);
	} else {
	    return ("<" . $tt . ">", 1) unless $tt eq "\20";
	    return ("<p>", 1, 1);
	}
    }
    return (lc($tag),0);
}


sub _SanitizeAtt {
    my $att = lc($_[0]);
    return "" unless $att =~ /^[_a-z:][_a-z:0-9.-]*$/; # no weirdo char att names
    return "" unless $univatt{$att} || $_[2]->{$att};
    return "" if $_[3]->{$att}; # no repeats
    $_[3]->{$att} = 1;
    $impatt{$att} and return $att."=".'"'.$att.'" ';
    (($_[4] eq "a" && $att eq "href") ||
     ($_[4] eq "img" && $att eq "src")) &&
    $_[1] =~ /^\s*[\047\042]\s*javascript:/io and do {
	$_[1] = '"#"';
	ref($opt{base_prefix}) eq 'CODE' and
	    $_[1] = '"' . escapeXML(&{$opt{base_prefix}}("#")) . '"';
    };
    if ($_[4] eq "a") {
	$att eq "target" and
	    return $_[1] =~ /^([\042\047])\s*_blank\s*\1$/io ? 'target="_blank" ' : "";
	$att eq "rel" and
	    return $_[1] =~ /^([\042\047])\s*nofollow\s*\1$/io ? 'rel="nofollow" ' : "";
    }
    if ($lcattval{$att}) {
	return $att."="._SanitizeAttValue(lc($_[1]))." ";
    } else {
	my $satt = _SanitizeAttValue($_[1]);
	if (ref($opt{urlfunc}) eq 'CODE' &&
	    (($_[4] eq "a" && $att eq "href") ||
	     ($_[4] eq "img" && $att eq "src")) ) {
		my ($lq,$v,$rq);
		$lq = substr($satt, 0, 1);
		$rq = substr($satt, -1, 1);
		$v = unescapeXML(substr($satt, 1, length($satt)-2));
		my ($uhost, $upath, $uq, $uf) = SplitURL($v);
		$v = &{$opt{urlfunc}}($v, \%opt, $_[4], $uhost, $upath, $uq, $uf);
		$satt = $lq . escapeXML($v) . $rq;
	}
	return $att."=".$satt." ";
    }
}


sub _SanitizeAttValue {
    my $v = shift;
    if ($v =~ /^([\042\047])(.*?)\1$/s) {
	return $1.escapeXML($2).$1;
    } else {
	return '"'.escapeXML($v).'"';
    }
}


sub _ProcessURLTag {
    my ($att, $tag, $dofrag) = @_;

    $att = lc($att) . "=";
    if ($tag =~ /^(<[^\s>]+\s+)/g) {
	my $out = $1;
	while ($tag =~ /\G([^\s\042\047<\/>=]+=)([\042\047])((?:(?!\2)(?!<).)*)(\2\s*)/gcs) {
	    my ($p, $q, $v, $s) = ($1, $2, $3, $4);
	    if (lc($p) eq $att && $v ne "") {
		if ($dofrag && $v =~ m"^#") {
		    $v = _FindFragmentMatch($v);
		    my $bpcr;
		    if (ref($bpcr = $opt{base_prefix}) eq 'CODE') {
			$v = "\2\3" . &$bpcr($v);
		    }
		} else {
		    $v = _PrefixURL($v);
		}
		$v = _EncodeAttText($v);
	    }
	    $out .= $p . $q . $v . $s;
	}
	$out .= substr($tag, pos($tag));
	substr($out,0,1) = $g_escape_table{'<'};
	substr($out,-1,1) = $g_escape_table{'>'};
	return $out;
    }

    return $tag;
}


my $oops_entities;
BEGIN { $oops_entities = qr/(?:lt|gt|amp|quot|apos|nbsp)/io; }


# $_[0] => the value to XML escape
# returns the XML escaped value
# Encodes the five required entites (amp,lt,gt,quot,apos)
# while preserving any pre-existing entities which means that
# calling this repeatedly on already-escaped text should return
# it unchanged (i.e. it's idempotent).
#
sub escapeXML {
    my $text = shift;

    # Treat these accidents as though they had the needed ';'
    $text =~ s/&($oops_entities)(?![A-Za-z0-9=;])/&$1;/go;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    # Remaining entities now
    $text =~ s/\042/&quot;/g;
    $text =~ s/\047/&#39;/g; # Some older browsers do not grok &apos;
    $text =~ s/</&lt;/g;
    $text =~ s/>/&gt;/g;

    return $text;
}


# $_[0] => value to be unescaped
# returns unescaped value
# The five required XML entities (amp,lt,gt,quot,apos) plus nbsp
# are decoded as well as decimal (#d+) and hexadecimal (#xh+).
#
# While the escapeXML function tries to be idempotent when presented
# with an already-escaped string, this function is NOT necessarily
# idempotent when presented with an already decoded string unless it's
# been decoded to the point there are no more recognizable entities left.
# In other words given a string such as:
#
#   &amp;amp;amp;amp;
#
# Each call will only decode one layer of escaping and it will take four
# successive calls to finally end up with just "&".
#
sub unescapeXML {
    my $text = shift;

    # Treat these accidents as though they had the needed ';'
    $text =~ s/&($oops_entities)(?![A-Za-z0-9=;])/&$1;/go;

    $text =~ s/&[qQ][uU][oO][tT];/\042/gso;
    $text =~ s/&[aA][pP][oO][sS];/\047/gso;
    $text =~ s/&[gG][tT];/>/gso;
    $text =~ s/&[lL][tT];/</gso;
    $text =~ s/&[nN][bB][sS][pP];/&#160;/gso;
    $text =~ s{&([aA][mM][pP]|\#\d+|\#x[0-9a-fA-F]+);}{
	local $_=$1;
        lc($_) eq 'amp' ? '&' :
	/^#(\d+)$/ ? chr($1) :
	/^#[xX](.*)$/ ? chr(hex($1)) :
	$_
    }gsex;

    return $text;
}


# $_[0] => the input URL to split
# $_[1] => if a true value, call unescapeXML before splitting
# returns array:
#  [0] => scheme, name+password, host, port ("" if not present)
#  [1] => path in url (starts with "/" if absolute otherwise relative)
#  [2] => query string ("" if not present otherwise starts with "?")
#  [3] => fragment ("" if not present otherwise starts with "#")
# The returned value recovers the (possibly unescapeXML'd) input
# string by simply concatenating the returned array elements.
#
sub SplitURL {
	my ($url, $unesc) = @_;
	$unesc and $url = unescapeXML($url);
	my ($sh, $p, $q, $f) = ("", "", "", "");
	if ($url =~ m{^([A-Za-z][A-Za-z0-9.+-]*:)(//.*)$}os) {
		$sh = $1;
		$url = $2;
	}
	if ($url =~ m{^(//[^/?#]*)((?:[/?#].*)?)$}os) {
		$sh .= $1;
		$url = $2;
	}
	($p, $q, $f) = $url =~ m{^([^?#]*)((?:[?][^#]*)?)((?:[#].*)?)$}os;
	return ($sh, $p, $q, $f);
}


my $_replacesub;
BEGIN { $_replacesub = sub {
    my $x = $named_character_entity{$_[1]};
    $x ? '&#'.$x.';' : $_[0];
} }


# $_[0] => the input text to process
# returns text with all known named character entities replaced
# with their equivalent numerical entity
sub ConvertNamedCharacterEntities {
    use bytes;
    my $text = shift;
    defined($text) or return undef;
    $text =~ s/(&([A-Za-z]{3,8}[1-4]{0,2});)/&$_replacesub($1,$2)/goes;
    return $text;
}


my $_usasciisub;
BEGIN { $_usasciisub = sub {
    my $c = $_[0];
    my $o = ord($c);
    return ($o <= 999) ? (($o < 128) ? $c : "&#$o;") : sprintf("&#x%x;", $o);
} }


# $_[0] => the input text to process
# returns text with non-US-ASCII characters replaced
# with their equivalent numerical character entities,
# but only if the input text has already been utf8::decode'd
sub ConvertToASCII {
    my $text = shift;
    defined($text) or return undef;
    $text =~ s/([^\x00-\x7F])/&$_usasciisub($1)/goes;
    return $text;
}


sub _EncodeAmps {
    my $text = shift;

    # Treat these accidents as though they had the needed ';'
    $text =~ s/&($oops_entities)(?![A-Za-z0-9=;])/&$1;/go;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    return $text;
}


sub _EncodeAmpsAndAngles {
# Smart processing for ampersands and angle brackets that need to be encoded.

    my $text = shift;

    # Treat these accidents as though they had the needed ';'
    $text =~ s/&($oops_entities)(?![A-Za-z0-9=;])/&$1;/go;

    # Ampersand-encoding based entirely on Nat Irons's Amputator MT plugin:
    #   http://bumppo.net/projects/amputator/
    $text =~ s/&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)/&amp;/g;

    # Encode naked <'s
    $text =~ s{<(?![\020a-z/?\$!])}{&lt;}gi;
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
    s{\\([`*_~{}\[\]()>#+\-.!`])}{$g_escape_table{$1}}go;

    return $_;
}


sub _DoAutoLinks {
    local $_ = shift;

    s{<((https?|ftps?):[^'\042>\s]+)>(?!\s*</a>)}{_MakeATag($1, "&lt;".$1."&gt;")}gise;

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
    s{(?:^|(?<=\s))((?:https?|ftps?)://(?:[-a-zA-Z0-9./?\&\%=_~!*;:\@+\$,\x23](?:(?<![.,:;])|(?=[^\s])))+)}
     {_MakeATag($1, $1)}soge;
    s{(?<![][])(?<!\] )\[RFC( ?)([0-9]{1,5})\](?![][])(?! \[)}
     {"["._MakeATag("https://tools.ietf.org/html/rfc$2", "RFC$1$2", "RFC $2")."]"}soge;

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
# mailing list: <https://tinyurl.com/yu7ue>
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
    $addr = _MakeATag("$addr", $prefix.$bareaddr.$suffix);

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
#   <https://web.archive.org/web/20041215155739/http://bradchoate.com/weblog/2002/07/27/mtregex>
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


# _DeTab
#
# $1 => input text
# $2 => optional tab width (default is $opt{tab_width})
# $3 => leading spaces to strip off each line first (default is 0 aka none)
# <= result with tabs expanded
sub _DeTab {
    my $text = shift;
    my $ts = shift || $opt{tab_width};
    my $leadsp = shift || 0;
    my $spr = qr/^ {1,$leadsp}/ if $leadsp;
    pos($text) = 0;
    my $end = length($text);
    my $ans = "";
    while (pos($text) < $end) {
	my $line;
	if ($text =~ /\G(.*?\n)/gcs) {
	    $line = $1;
	} else {
	    $line = substr($text, pos($text));
	    pos($text) = $end;
	}
	$line =~ s/$spr// if $leadsp;
	# From the Perl camel book section "Fluent Perl" but modified a bit
	$line =~ s/(.*?)(\t+)/$1 . ' ' x (length($2) * $ts - length($1) % $ts)/ges;
	$ans .= $line;
    }
    return $ans;
}


sub _PrefixURL {
#
# Add URL prefix if needed
#
    my $url = shift;
    $url =~ s/^\s+//;
    $url =~ s/\s+$//;
    $url = "#" unless $url ne "";

    return $url unless
	ref($opt{abs_prefix}) eq 'CODE' ||
	ref($opt{url_prefix}) eq 'CODE' ||
	ref($opt{img_prefix}) eq 'CODE' ;
    return $url if $url =~ m"^\002\003" || $url =~ m"^#" || $url =~ m,^//,;
    $url = &{$opt{abs_prefix}}($url) if $url =~ m,^/, && ref($opt{abs_prefix}) eq 'CODE';
    return $url if $url =~ /^[A-Za-z][A-Za-z0-9+.-]*:/ || $url =~ m,^//, ||
	($opt{keepabs} && $url =~ m,^/,);
    my $cr = $opt{url_prefix};
    $cr = $opt{img_prefix}
	if ref($opt{img_prefix}) eq 'CODE' && $url =~ m"^[^#?]*\.(?:png|gif|jpe?g|svgz?)(?:[#?]|$)"i;
    return $url unless ref($cr) eq 'CODE';
    return "\2\3".&$cr(substr($url, 0, 1) eq '/' ? substr($url, 1) : $url);
}


BEGIN {
    $g_style_sheet = <<'STYLESHEET';

<style type="text/css">
/* <![CDATA[ */

/* Markdown.pl fancy style sheet
** Copyright (C) 2017,2018,2019,2020,2021 Kyle J. McKay.
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
table.%(base)yaml-table {
	border-collapse: collapse;
}
table.%(base)yaml-table * {
	border: thin solid;
}
table.%(base)yaml-table th {
	text-align: center;
}
table.%(base)yaml-table th, table.%(base)yaml-table td {
	padding-left: 0.5ex;
	padding-right: 0.5ex;
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
	vertical-align: text-top;
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
   --sanitize                           sanitize tag attributes
   --no-sanitize                        do not sanitize tag attributes
   --validate-xml                       check if output is valid XML
   --validate-xml-internal              fast basic check if output is valid XML
   --no-validate-xml                    do not check output for valid XML
   --strip-comments                     remove XML-like comments from output
   --strip-comments-lax                 remove XML-like comments from output
   --strip-comments-strict              remove only strictly valid XML comments
   --strip-comments-lax-only            remove only invalid XML-like comments
   --no-strip-comments                  do not remove any XML/XML-like comments
   --tabwidth=num                       expand tabs to num instead of 8
   --auto-number                        automatically number h1-h6 headers
   -k | --keep-abs                      keep abspath URLs despite -r/-i
   -a prefix | --absroot=prefix         append abspath URLs to prefix
   -b prefix | --base=prefix            prepend prefix to fragment-only URLs
   -r prefix | --htmlroot=prefix        append relative non-img URLs to prefix
   -i prefix | --imageroot=prefix       append relative img URLs to prefix
   -w [wikipat] | --wiki[=wikipat]      activate wiki links using wikipat
   --yaml[=(enable|disable|strip|...)]  select YAML front matter processing
   -V | --version                       show version, authors, license
                                        and copyright
   -s | --shortversion                  show just the version number
   --raw | --raw-xml                    input contains only raw xhtml
   --raw-html                           input contains only raw html
   --div[=id]                           wrap body in div with given id
   --stylesheet                         output the fancy style sheet
   --no-stylesheet                      do not output fancy style sheet
   --keep-named-character-entities      do not convert named character entities
   --us-ascii                           convert non-ASCII to character entities
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

This option is I<NOT compatible> with the B<--validate-xml> option
and will produce an immediate error if both are given.


=item B<--deprecated>

Both "<dir>" and "<menu>" are normally taken as literal text and the leading
"<" will be automatically escaped.

If this option is used, they are recognized as valid tags and passed through
without being escaped.

When dealing with program argument descriptions "<dir>" can be particularly
problematic therefore use of this option is not recommended.

Other deprecated tags (such as "<font>" and "<center>" for example) continue
to be recognized and passed through even without using this option.


=item B<--sanitize>

Removes troublesome tag attributes from embedded tags.  Only a very strictly
limited set of tag attributes will be permitted, other attributes will be
silently discarded.  The set of allowed attributes varies by tag.

Splits empty minimized elements that are not one of the HTML allowed empty
elements (C<area> C<basefont> C<br> C<col> C<hr> C<img>) into separate begin
and end tags.  For example, C<< <p/> >> or C<< <p /> >> will be split into
C<< <p></p> >>.

Combines adjacent (whitespace separated only) opening and closing tags for
the same HTML empty element into a single minimized tag.  For example,
C<< <br></br> >> will become C<< <br /> >>.

Tags that require at least one attribute to be present to be meaningful
(e.g. C<a>, C<area>, C<img>, C<map>) but have none will be treated as non-tags
potentially creating unexpected errors.  For example, the sequence
C<< <a>text here</a> >> will be sanitized to C<< &lt;a>text here</a> >> since
an C<a> tag without any attributes is meaningless, but then the trailing
close tag C<< </a> >> will become an error because it has no matching open
C<< <a ...> >> tag.

The point of this check is not to cause undue frustration, but to allow
such constructs to be used as text without the need for escaping since they
are meaningless as tags.  For example, C<< <a><c><e> >> works just fine
as plain text and so does C<< <A><C><E> >> because the
C<< <a> >>/C<< <A> >> will be treated as a non-tag automatically.  In fact,
they can even appear inside links too such as
C<< <a href="#somewhere">Link to <a><c><e> article</a> >>.

Problematic C<&> characters are fixed up such as standalone C<&>s (or those not
part of a valid entity reference) are turned into C<&amp;>.  Within attribute
values, single and double quotes are turned into C<&> entity refs.

This is enabled by default.


=item B<--no-sanitize>

Do not sanitize tag attributes.  This option does not allow any tags that would
not be allowed without this option, but it does completely suppress the
attribute sanitation process.   If this option is specified, no attributes will
be removed from any tag (although C<img> and C<a> tags will still be affected
by B<--imageroot>, B<--htmlroot>, B<--absroot> and/or B<--base> options).
Use of this option is I<NOT RECOMMENDED>.


=item B<--validate-xml>

Perform XML validation on the output before it's output and die if
it fails validation.  This requires the C<XML::Simple> or C<XML::Parser>
module be present (one is only required if this option is given).

Any errors are reported to STDERR and the exit status will be
non-zero on XML validation failure.  Note that all line and column
numbers in the error output refer to the entire output that would
have been produced.  Re-run with B<--no-validate-xml> to see what's
actually present at those line and column positions.

If the B<--stub> option has also been given, then the entire output is
validated as-is.  Without the B<--stub> option, the output will be wrapped
in C<< <div>...</div> >> for validation purposes but that extra "div" added
for validation will not be added to the final output.

This option is I<NOT enabled by default>.

This option is I<NOT compatible> with the B<--html4tags> option and will
produce an immediate error if both are given.


=item B<--validate-xml-internal>

Perform XML validation on the output before it's output and die if
it fails validation.  This uses a simple internal consistency checker
that finds unmatched and mismatched open/close tags.

Non-empty elements that in HTML have optional closing tags (C<colgroup>
C<dd> C<dt> C<li> C<p> C<tbody> C<td> C<tfoot> C<th> C<thead> C<tr>)
will automatically have any omitted end tags inserted during the
`--validate-xml-internal` process.

Any errors are reported to STDERR and the exit status will be
non-zero on XML validation failure.  Note that all line and column
numbers in the error output refer to the entire output that would
have been produced before sanitization without any B<--stub> or
B<--stylesheet> options.  Re-run with B<--no-sanitize> and
B<--no-validate-xml> and I<without> any B<--stub> or B<--stylesheet>
options to see what's actually present at those line and column
positions.

This option validates the output I<prior to> adding any requested
B<--stub> or B<--stylesheet>.  As the built-in stub and stylesheet
have already been validated that speeds things up.  The output is
I<NOT> wrapped (in a C<< <div>...</div> >>) for validation as that's
not required for the internal checker.

This option is I<IS enabled by default> unless B<--no-sanitize> is
active.

This option is I<IS compatible> with the B<--html4tags> option.

This option requires the B<--sanitize> option and will produce an
immediate error if both B<--no-sanitize> and B<--validate-xml-internal>
are given.

Note that B<--validate-xml-internal> is I<MUCH faster> than
B<--validate-xml> and I<does NOT> require any extra XML modules to
be present.


=item B<--no-validate-xml>

Do not perform XML validation on the output.  Markdown.pl itself will
normally generate valid XML sequences (unless B<--html4tags> has been
used).  However, any raw tags in the input (that are on the "approved"
list), could potentially result in invalid XML output (i.e. mismatched
start and end tags, missing start or end tag etc.).

Markdown.pl can check for these issues itself using its own internal
B<--validate-xml-internal> check or, with the B<--validate-xml>
option, it can use C<XML::Simple> or C<XML::Parser> to do so.

Note that B<--validate-xml-internal> is the default option unless
B<--no-sanitize> is used in which case B<--no-validate-xml> is the
default option.


=item B<--strip-comments>/B<--strip-comments-lax>

(N.B. B<--strip-comments> is just a short form of B<--strip-comments-lax>)

Strip XML and XML-like comments from the output.  Any XML or XML-like
comments encountered will be omitted from the output if either of these
options is given.

Unlike the B<--strip-comments-strict> option, these options I<will>
strip any XML-like comments that contain internal double hyphen
(i.e. C<-->) sequences.

This option requires the B<--sanitize> option to be used (which is
the default).

If either of these options is given, it will supersede any previous
B<--strip-comments-strict>, B<--strip-comments-lax-only> or
B<--no-strip-comments> options.


=item B<--strip-comments-strict>

Strip only strictly XML standard compliant comments from the output.

Note that the XML standard section 2.5 specifically prohibits
a C<--> sequence within an XML comment (i.e. C<--> cannot occur after
the comment start tag C<< <!-- >> unless it is immediately followed
by C<< > >> which makes it the comment end tag C<< --> >>).

In other words, S<C<< <!-- --> >>>, S<C<< <!-- - --> >>>, S<C<< <!----> >>>,
and S<C<< <!--- --> >>> are all valid XML comments, but S<C<< <!-----> >>>
and S<C<< <!-- ---> >>> are not!

As part of the "sanitation" process (triggered by the B<--sanitize>
option), any invalid tags have their leading C<< < >> escaped (to
C<< &#lt; >>) thus making them ordinary text and this I<includes>
invalid XML comments.

What this means is that the B<--strip-comments-strict> option I<will not>
remove invalid XML comments (such as S<C<< <!-----> >>>)!

But see the B<--strip-comments-lax> option for a solution.

If this option is given, it will supersede any previous
B<--strip-comments>, B<--strip-comments-lax>, B<--strip-comments-lax-only>
or B<--no-strip-comments> options.


=item B<--strip-comments-lax-only>

This is the default option if no other strip comments options are given
AND the B<--sanitize> option is active (the default).

This is a compromise option.  It works just like the B<--strip-comments-lax>
option, but I<ONLY> on strictly invalid XML-like comments.

In other words, if a strictly valid XML comment is present, it will be retained
in the output.  If a strictly invalid XML comment is present which would have
been stripped by B<--strip-comments-lax> but would have had its leading C<< < >>
escaped automatically by the B<--no-strip-comments> or B<--strip-comments-strict>
modes (because it's not a strictly valid XML comment), then it I<will> be stripped
by this mode.

This option prevents ugly invalid XML comments from slipping through into the
output as escaped plain text while still passing through valid XML comments
without stripping them.

If this option is given, it will supersede any previous
B<--strip-comments>, B<--strip-comments-lax>, B<--strip-comments-lax-only>
or B<--no-strip-comments> options.


=item B<--no-strip-comments>

Do not strip XML or XML-like comments from the output.

This is the default option I<ONLY> when no other strip comments options have
been give I<and> the B<--no-sanitize> option is in effect (which is I<not> the
default).

When B<--no-strip-comments> is active, strictly invalid XML comments such
as those that contain an internal double hyphen (C<-->) sequence will end
up having their leading C<< < >> escaped automatically and end up as plain
text in the output!

If this option is given, it will supersede any previous
B<--strip-comments>, B<--strip-comments-lax>, B<--strip-comments-lax-only>
or B<--no-strip-comments> options.


=item B<--tabwidth>=I<num>

Expand tabs to I<num> character wide tab stop positions instead of the default
8.  Don't use this; physical tabs should always be expanded to 8-character
positions.  This option does I<not> affect the number of spaces needed to
start a new "indent level".  That will always be 4 no matter what value is
used (or implied by default) with this option.  Also note that tabs inside
backticks-delimited code blocks will always be expanded to 8-character tab
stop positions no matter what value is used for this option.

The value must be S<2 <= I<num> <= 32>.


=item B<--auto-number>

Automatically number all h1-h6 headings generated from Markdown markup.
Explicit C<< <h1> >> ... C<< <h6> >> tag content remains unmolested.

If this option is given, any YAML C<header_enum> setting will be ignored.


=item B<-k>, B<--keep-abs>

Normally any absolute path URLs (i.e. URLs without a scheme starting
with "/" but not "//") are subject to modification by any
B<-r>/B<--htmlroot> or B<-i>/B<--imageroot> option.

If the B<-a>/B<--absroot> option is used and it transforms these
absolute path URLs into a full absolute URL (i.e. starts with a
scheme or "//") then any subsequent B<-r>/B<--htmlroot> or
B<-i>/B<--imageroot> processing will be skipped because the URL is
no longer relative.

If the B<--keep-abs> option is given, then (after applying any
B<-a>/B<--absroot> option if present) absolute path URLs will be
kept as-is and will not be processed further by any B<-r>/B<--htmlroot>
or B<-i>/B<--imageroot> option.

Note that if the B<-a>/B<--absroot> option transforms an absolute
path URL into a relative PATH URL it I<will> be subject to subsequent
B<-r>/B<--htmlroot> or B<-i>/B<--imageroot> processing regardless
of the B<-k>/B<--keep-abs> option.


=item B<-a> I<prefix>, B<--absroot>=I<prefix>

Any absolute path URLs (i.e. URLs without a scheme starting with "/" but not
"//") have I<prefix> prepended which prevents them from being acted upon by the
B<--htmlroot> and/or B<--imageroot> options provided the result is a full
absolute URL.  The default is to prepend nothing and leave them as absolute
path URLs which will allow them to be processed by any B<--htmlroot> and/or
B<--imageroot> options.

This option can be helpful when documents are being formatted for display on a
different system and the absolute path URLs need to be "fixed up".


=item B<-b> I<prefix>, B<--base>=I<prefix>

Any fragment-only URLs have I<prefix> prepended.  The default is to prepend
nothing and leave them as bare fragment URLs.  Use of this option may be
necessary when embedding the output of Markdown.pl into a document that makes
use of the C<< <base> >> tag in order for intra-document fragment URL links to
work properly in such a document.


=item B<-r> I<prefix>, B<--htmlroot>=I<prefix>

Any non-absolute URLs have I<prefix> prepended.


=item B<-i> I<prefix>, B<--imageroot>=I<prefix>

Any non-absolute URLs have I<prefix> prepended (overriding the B<-r> prefix
if any) but only if they end in an image suffix.


=item B<-w> [I<wikipat>], B<--wiki>[=I<wikipat>]

Activate wiki links.  Any link enclosed in double brackets (e.g. "[[link]]") is
considered a wiki link.  By default only absolute URL and fragment links are
allowed in the "wiki link style" format.  Any other double-bracketed strings
are left unmolested.

If this option is given, all other wiki links are enabled as well.  Any
non-absolute URL or fragment links will be transformed into a link using
I<wikipat> where the default I<wikipat> if none is given is C<%{s(:md)}.html>.

If the given I<wikipat> does not contain a C<%{...}> placeholder sequence
then it will automatically have C<%{s(:md)}.html> suffixed to it.

The C<...> part of the C<%{...}> sequence specifies zero or more
case-insensitive single-letter options with the following effects:

=over

=item B<b>

Retain blanks (aka spaces) in the output.  They will become C<%20>
in the final URL.  Because spaces are always trimmed before processing
wiki links, runs of multiple spaces will be collapsed into a single
space and any leading or trailing spaces will be removed.

=item B<d>

Convert spaces to dashes (ASCII 0x2D) instead of underscore (ASCII
0x5F).  Note that if this option is given then runs of multiple
dashes will be converted to a single dash I<instead> but runs of
multiple underscores will be left untouched.

=item B<f>

Flatten the resulting name by replacing forward slashes (ASCII 0x2F)
as well.  They will be converted to underscores unless the C<d>
option is given (in which case they will be converted to dashes).
This conversion takes place before applying the runs-of-multiple
reduction.  This option is incompatible with the B<%> option.

=item B<%>

Flatten the resulting name by replacing runs of one or more forward
slashes (ASCII 0x2F) with C<%2F>.  Note that when encoded into a
URL the C<%2F> actually becomes C<%252F>.  This option is incompatible
with the B<f> option.

=item B<l>

Convert link target (excluding any query string and/or fragment) to lowercase.
Takes precedence over any C<u> option, but specifically excludes C<%>-escapes
which are always UPPERCASE hexadecimal.

=item B<r>

Leave raw UTF-8 characters in the result.  Normally anything not allowed
directly in a URL ends up URL-encoded.  With this option, raw valid UTF-8
sequences will be left untouched.  Use with care.

=item B<s> or B<s(>I<< <ext> >>[B<,>I<< <ext> >>]...B<)>

After (temporarily) removing any query string and/or fragment, strip any final
"dot" suffix so long as it occurs after the last slash (if any slash was
present before applying the C<f> option).  The "dot" (ASCII 0x2E) and all
following characters (if any) are removed.  If the optional C<< (<ext>,...) >>
part is present then only strip the extension if it consists of a "dot"
followed by one of the case-insensitive I<< <ext> >> values.  As a special
case, using the value C<:md> for one of the I<< <ext> >> values causes that
value to be expanded to all known markdown extensions.

When processing wiki image links, this option is ignored.

=item B<u>

Convert link target (excluding any query string and/or fragment) to UPPERCASE.

=item B<v>

Leave runs-of-multiple characters alone (aka "verbatim").  Does not affect
any of the other options except by eliminating the runs-of-multple reduction
step.  Also does I<not> inhibit the initial whitespace trimming.

Does not affect the runs-of-multiple "/" replacement performed by the B<%>
option.

=back

The URL target of the wiki link is created by first trimming whitespace
(starting and ending whitespace is removed and all other runs of consecutive
whitespace are replaced with a single space) from the wiki link target,
removing (temporarily) any query string and/or fragment, if no options are
present, spaces are converted to underscores (C<_>) and runs of multiple
consecutive underscores are replaced with a single underscore (ASCII 0x5F).
Finally, the I<wikipat> string gets its first placeholder (the C<%{...}>
sequence) replaced with this computed value and the original query string
and/or fragment is re-appended (if any were originally present) and
URL-encoding is applied as needed to produce the actual final target URL.

Note that when processing wiki image links, no extension stripping ever takes
place (i.e. the "s" option is ignored) and anything after the placeholder (the
C<%{...}> sequence) in the pattern is omitted from the result.

See above option descriptions for possible available modifications.

One of the commonly used hosting platforms does something substantially similar
to using C<%{dfv}> as the placeholder.

One of the commonly used wiki platforms does something similar to using C<%{%}>
as the placeholder.


=item B<--yaml>[=I<yamlmode>]

Select YAML front matter processing.  The optional I<yamlmode> value
must be one of the following:

=over

=item B<enable>

Recognize any YAML front matter and apply any options specified
therein.  If any unrecognized options are present, the options will
also be shown in the formatted output.

This is the default I<yamlmode> if omitted.

=item B<disable>

No YAML front matter processing at all takes place.  If YAML front
matter is present, it will be treated as regular non-YAML markup
text to be processed.

=item B<strip>

If YAML front matter is present, it will be stripped and completely
ignored before beginning to process the rest of the input.

In this mode, any options in the YAML front matter that would have
otherwise been recognized will I<not have any effect!>

=item B<show>

If YAML front matter is present and contains anything other than
comments, the non-comments parts will be shown in the formatted
output.

In this mode, any options in the YAML front matter that would have
otherwise been recognized will I<not have any effect!>

This is a show-only mode.

=item B<reveal>

This mode works just like the B<enable> mode except that if the
YAML front matter contains anything other than comments, then
I<all> of the non-comments parts will be shown in the formatted
output.

In this mode, any recognized options in the YAML front matter I<are>
processed the same way they would be in the B<enable> mode except
that any option to suppress the B<reveal> mode is ignored.

=item B<conceal>

This mode works just like the B<enable> mode except that no options
are ever shown in the formatted output regardless of whether or not
there are any unrecognized options present.

In this mode, any recognized options in the YAML front matter I<are>
processed the same way they would be in the B<enable> mode except
that any option to suppress the B<conceal> mode is ignored.

=item B<unknown>

This mode works just like the B<show> mode if any unrecognized
YAML front matter options are present.  Otherwise it works like
the B<strip> mode.

In this mode, any options in the YAML front matter that would have
otherwise been recognized will I<not have any effect!>

=back

If B<--raw>, B<--raw-xml> or B<--raw-html> has been specified then
the default if no B<--yaml> option has been given is B<--yaml=disable>.

Otherwise the default if no B<--yaml> option has been given is
B<--yaml=enable>.

Note that only a limited subset of YAML is recognized.  Specifically
only comments, and top-level single-line S<C<key: value>> items
where key must be plain (i.e. non-quoted), start with a letter or
underscore and contain only letters, underscores, hyphens (C<->),
periods (C<.>) and digits.  Keys are case-insensitive (i.e. converted
to lowercase).  As with YAML, at least one whitespace is required
between the ":" and the value (unless it's the empty value).

Values may be either plain or double-quoted (single-quoted is not
recognized).  The double-quoted style may use C-style character
escape codes but may not extend past the end of the line.

For YAML front matter to be recognized, the very first line of the
document must be exactly three hyphens (C<--->).  The YAML terminates
when a line of three hyphens (C<--->) or a line of three periods
(C<...>) or the end of the file is encountered.  Of course the YAML
mode must also be something I<other> than B<--yaml=disable>.


=item B<-V>, B<--version>

Display Markdown's version number and copyright information.


=item B<-s>, B<--shortversion>

Display the short-form version number.


=item B<--raw>, B<--raw-xml>

Input contains only raw XHTML.  All options other than B<--html4tags>,
B<--deprecated>, B<--sanitize> (on by default), B<--strip-comments>,
B<--div>, B<--keep-named-character-entities>, B<--validate-xml> and
B<--validate-xml-internal> (and their B<--no-...> variants) are
ignored.

With this option, arbitrary XHTML input can be passed through
the sanitizer and/or validator.  If sanitation is requested (the
default), input must only contain the contents of the "<body>"
section (i.e. no "<head>" or "<html>").  Output I<will> be converted
to UTF-8 regardless of the input encoding.  All line endings will
be normalized to C<\n> and input encodings other than UTF-8 or
ISO-8859-1 or US-ASCII will end up mangled.

Remember that any B<--stub> and/or B<--stylesheet> options are
I<completely ignored> when B<--raw> is given.


=item B<--raw-html>

Input contains only raw HTML.  All options other than
B<--html4tags>, B<--deprecated>, B<--sanitize> (on by default),
B<--strip-comments>, and B<--validate-xml-internal>
(and their B<--no-...> variants) are ignored.

Requires the (possibly implicit) B<--validate-xml-internal> option.

Works just like B<--raw-xml> except that HTML auto closing and
optional closing tag semantics are activated during the validation
causing missing closing tags to be inserted where required by the
standard.  Non-raw mode always enables these semantics.

This will transform HTML into valid XHTML or fail with an error message.

Unfortunately, it will also fail to accept some documents that
the plain B<--raw-xml> option will.

For example, this document:

 <dt><li>a</li></dt>

Will be rejected because upon encountering the C<< <li> >> open
tag a closing C<< </dt> >> will automatically be inserted resulting
in this document:

 <dt></dt><li>a</li></dt>

Which, of course, no longer validates.  Since C<li> blocks cannot
directly be nested within C<dt> blocks (according to the standard),
the input document is not strictly correct.

Remember that any B<--stub> and/or B<--stylesheet> options are
I<completely ignored> when B<--raw-html> is given.


=item B<--div>[=I<divname>]

Wrap the output contents in a C<div> tag.  If I<divname> is given the
tag will have that C<id> attribute value.  If the B<--stub> option and/or
the B<--stylesheet> option are active, they are applied I<after> wrapping
the output contents in the C<div>.  Note that if a YAML table ends up
being generated, it I<will> be included I<inside> the C<div> wrapper.

In contrast to the B<--stylesheet> and B<--stub> options, this option
I<is> allowed with the B<--raw-xml> and B<--raw-html> options.


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


=item B<--keep-named-character-entities>

Do not convert named character entities to their equivalent numerical character
entity.  Normally any occurrence of a named character entity such as
C<&hellip;> would be converted to its equivalent character entity such as
C<&#x2026;>.  If this option is given, that conversion is suppressed.

The only always-valid named entities as far as XML is concerned are the five
entities C<&amp;>, C<&lt;>, C<&gt;>, C<&quot;> and C<&apos;>.  Even that last
one (C<&apos;>) may not be universally supported in XHTML user agents (and it
is converted to C<&#39;> for that reason unless this option is given).

Regardless of this option, C<&amp;>, C<&lt;>, C<&gt;> and C<&quot;> are always
left alone since they are universally supported.

Use of this option is I<NOT RECOMMENDED>.


=item B<--us-ascii>/B<--ascii>

(N.B. B<--ascii> is just a short form of B<--us-ascii>)

Convert any non-US-ASCII characters to their equivalent numerical character
entity.  Any characters with a code point value greater than or equal to
128 will be converted.  Note that the output is still technically UTF-8 since
the US-ASCII code points coincide with the same code points of UTF-8.

Using this option will make the output strictly 7-bit and therefore it should
survive just about any transport mechanism at the expense of an increase in
size that depends on how many non-US-ASCII characters are present.


=item B<--stub>

Wrap the output in a full document stub (i.e. has C<html>, C<head> and C<body>
tags).  The style sheet I<will> be included in the C<head> section unless the
B<--no-stylesheet> option is also used.

The C<< <title> >> value for a document produced with the B<--stub> option
comes from the first markdown markup C<h1> that's generated unless YAML
processing has been enabled (the default) and a C<title> YAML value has
been set in which case that always takes precedence.


=item B<-h>, B<--help>

Display Markdown's help.  With B<--help> full help is shown, with B<-h> only
the usage and options are shown.


=back


=head1 HTML CONTENT

Markdown format documents are intended to be human readable without the use
of XML-like markup.

Nevertheless, html content can be included verbatim provided that the tags
used are limited to those of the HTML 4 specification and only those tags
that represent body content -- scripting tags and attributes are not allowed.

The final version of the HTML specification (including a DTD) can be found
here:

=over

=over

=item L<https://www.w3.org/TR/1999/REC-html401-19991224/>

=back

=back

Note that attempts to use any of the new tags from the "HTML Living Standard"
will simply result in them being escaped into literal text.

Stick to markdown-format text or HTML 4 tags to avoid unexpected output.


=head1 PERL MODULE

Markdown can be used as a Perl module and can be "use"d like so:

 use Markdown qw(...);

Or like so:

 BEGIN {require "Markdown.pl" && Markdown->import(qw(...))}

where the C<...> part is the list of desired imports.

The Markdown module does not export any functions by default.

The C<Markdown.pm> file is a symbolic link to C<Markdown.pl>.

=head2 Markdown module functions

Any of these functions may be imported, but none of them
are imported by default.

=over


=item * $result = Markdown::Markdown($string[, options...])

Converts Markdown-format C<$string> to UTF-8 encoded XHTML and
returns it.

The C<options...> may be either a single HASH ref or one or more
pairs of C<< key => value >>.

See the comments for the C<_SanitizeOpts> function for a list of
possible option keys.


=item * $result = Markdown::ProcessRaw($string[, options...])

Converts raw XHTML in C<$string> to XHTML and returns it.

The C<options...> may be either a single HASH ref or one or more
pairs of C<< key => value >>.

See the comments for the C<_SanitizeOpts> function for a list of
possible option keys.

This function provides the ability to apply the internal XML
validation and sanitation functionality to arbitrary XHTML without
performing any of the Markdown format interpretation.


=item * $stylesheet = Markdown::GenerateStyleSheet([$prefix])

Returns an XHTML style sheet that supports the fancy Markdown styles
such as checkboxes and right parenthesis lists.

All of the style class names have C<$prefix> prepended.

If C<$prefix> is omitted or C<undef> then the default S<"_markdown-">
prefix will be used which is the same default prefix that the
C<Markdown> function uses.

The returned string value consists of a C<< <style type="text/css"> >>
tag, the contents of the style sheet and ends with a C<< </style> >> tag.


=item * Markdown::SetWikiOpts($hashref, $wikioption)

The value of C<$wikioption> should be the value of the C<wikipat> value
from the B<--wiki> option.  Use the empty string S<""> to enable wiki
links using the defaults and use C<undef> to disable wiki links.

The C<wikipat> and C<wikiopt> keys in C<$hashref> will both be
affected by this call and they should be passed in to the Markdown
function as options to enable processing of wiki links.

The simplest way to do this is simply to pass a HASH ref as the
second argument to the Markdown function after having used this
function on it to properly set the C<wikipat> and C<wikiopt>
keys and values.


=back


=head2 Example

This rudimentary example approximates running
S<C<Markdown.pl --stub --wiki>>
on the input (files if given, standard input if not).

 use Markdown qw(Markdown SetWikiOpts GenerateStyleSheet escapeXML);

 my $string;
 {local $/; $string = <>;}
 my %opts = ( h1 => "default title" );
 SetWikiOpts(\%opts, ""); # enable default --wiki processing
 my $xhtml = Markdown($string, \%opts);
 print "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",
   "<head>\n<title>".escapeXML($opts{h1})."</title>\n",
   GenerateStyleSheet(),"</head>\n",
   "<body style=\"text-align:center\">\n",
   "<div style=\"".
   "display:inline-block;text-align:left;max-width:42pc\">\n",
   $xhtml, "</div></body></html>\n";


=head1 VERSION HISTORY

Z<> See the F<README> file for detailed release notes for this version.

=over

=item Z<> 1.1.15 - 15 Aug 2021

=item Z<> 1.1.14 - 14 Jun 2021

=item Z<> 1.1.13 - 13 Jun 2021

=item Z<> 1.1.12 - 17 Mar 2021

=item Z<> 1.1.11 - 05 Mar 2021

=item Z<> 1.1.10 - 08 Jul 2020

=item Z<> 1.1.9 - 15 Dec 2019

=item Z<> 1.1.8 - 22 Nov 2019

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

=item L<https://daringfireball.net>

=item L<https://daringfireball.net/projects/markdown/>

=item E<160>

=back

=over

=item PHP port and other contributions by Michel Fortin

=item L<https://michelf.ca>

=item E<160>

=back

=over

=item Additional enhancements and tweaks by Kyle J. McKay

=item mackyle<at>gmail.com

=item L<https://repo.or.cz/markdown.git>

=back

=head1 COPYRIGHT AND LICENSE

=over

=item Copyright (C) 2003-2004 John Gruber

=item Copyright (C) 2015-2021 Kyle J. McKay

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
