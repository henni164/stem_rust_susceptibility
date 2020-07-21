#! /usr/bin/perl -w

# goterm_adding_to_network_file.pl

# This script will take enrichment output information and reduce it to what is necessary for plotting in R

# Cory Hirsch / Eva Henningsen
# June 12, 2019

use strict;
use Getopt::Long();

my $usage = "\nUsage: $0 --in <input enrichment file> --out <Output file> --help <help on usage of script>\n\n";

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

my $header = <$in_fh>;

print $out_fh "Description\tAdj_pvalue\tGene_Ratio\tDomain\n";

while (my $line = <$in_fh>) {
	chomp $line;
	my ($goterm, $ontology, $ID, $description, $generatio, $bgratio, $pvalue, $p_adjust, $qvalue, $geneID, $count) = split("\t", $line);

	print $out_fh "$description\t$p_adjust\t$generatio\t$ontology\n";

}

close $in_fh;
close $out_fh;

exit;