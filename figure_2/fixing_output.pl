#!usr/bin/perl -w

#fixing my mess

use strict;
use Getopt::Long();

my $usage = "\nUsage: $0 --in <input file> --out <Output file> --help <help on usage of script>\n\n";

my ($in, $out, $help);

Getopt::Long::GetOptions('in=s' => \$in,
                         'out=s' => \$out,
                         'h|help' => \$help
                         );

if (defined($help)) {
    die $usage;
}
if (!defined($in)) {
    die $usage;
}
if (-e $out) {
    die "\nOutput file $out already exists\n\n";
}

open (my $in_fh, '<', $in) || die "\nCan't open $in\n\n";
open (my $out_fh, '>', $out) || die "\nCan't open $out\n\n";

while (my $line = <$in_fh>) {
	chomp $line;

	my($id, $gos) = split("\t", $line);
	#if perl is able to split "$gos" by ",", then print ID and the first go, then print id and the next go, until the line is done
	if (grep $gos, ",") {

		my ($go1, $go2, $go3, $go4, $go5, $go6) = split(",", $gos);
		print $out_fh "$id\t$go1\n$id\t$go2\n$id\t$go3\n$id\t$go4\n$id\t$go5\n$id\t$go6\n";
	
	} else { print $out_fh "$id\t$gos\n"
		
		}

}

close $in_fh;
close $out_fh;

exit;