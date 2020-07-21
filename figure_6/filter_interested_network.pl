#! /usr/bin/perl -w

# filter_interested_network.pl

# This script will take a network file (node1, node2, weights, goterms, interested genes) and an gene of interest list and print out interested gene nodes and all connected nodes to connections 

# Cory Hirsch / Eva Henningsen
# June 12, 2019

use strict;
use Getopt::Long();

my $usage = "\nUsage: $0 --in <input network file> --interest <file contatining genes of interest>  --out <Output file> --help <help on usage of script>\n\n";

my ($in, $interest, $out, $help);

Getopt::Long::GetOptions('in=s' => \$in,
                         'interest=s' => \$interest,
                         'out=s' => \$out,
                         'h|help' => \$help
                         );

if (defined($help)) {
    die $usage;
}
if (!defined($in) || !defined($interest) ) {
    die $usage;
}
if (-e $out) {
    die "\nOutput file $out already exists\n\n";
}

open (my $interest_fh, "<", $interest) || die "Can't open $interest\n\n";
#open (my $out_fh, ">", $out) || die "Can't open $out\n\n";

while (my $line = <$interest_fh>) {
	chomp $line;

	open (my $in_fh, "<", $in) || die "Can't open $in\n\n";
	
	my $out2 = $out . '_' . $line . '.txt';
	open (my $out2_fh, ">", $out2) || die "Can't open $out\n\n";
	
	my $header = <$in_fh>;
	chomp $header;
	print $out2_fh "$header\n";

	my %match;
	while (my $line2 = <$in_fh>) {
		chomp $line2;

		my ($gene_A, $gene_B, $score, $goterm, $interesting_gene) = split ("\t", $line2);
		if ($gene_A eq $line || $gene_B eq $line) {
			$match{$gene_A} = 1;
			$match{$gene_B} = 1;
		}
	}
	close;

	#my $out2 = $out . '_' . $line . '.txt';
	open (my $in_fh2, "<", $in) || die "Can't open $in\n\n";
	#open (my $out2_fh, ">", $out2) || die "Can't open $out\n\n";
	while (my $line3 = <$in_fh2>) {
		chomp $line3;
		my ($gene_A, $gene_B, $score, $goterm, $interesting_gene) = split ("\t", $line3);
		if (exists $match{$gene_A} && exists $match{$gene_B} ) {
			print $out2_fh "$line3\n";
		}
	}
	close $in_fh2;
	close $out2_fh;
}

exit;







