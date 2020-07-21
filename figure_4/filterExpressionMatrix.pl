#!/usr/bin/perl -w

# This script will filter a matrix based on a minimum gene expression value
# Originally written for file: Fig3_genes_wheat.csv
# File setup: rows: gene expression information ; columns: FPKM values across samples
# Want to only keep genes in list that have an expression greater than 'X' in at least 1 sample

# filterExpressionMatrix.pl

# Jan 31, 2019
# Cory Hirsch

use strict;
use Getopt::Long;

# sample run: filterExpressionMatrix.pl  --in_file <file want to filter with FPKM values>  --fpkm_filter <minmum fpkm filter> --output <output file> --help <help on usage of script>
my $usage = "\nUsage: $0 --in_file <>  --fpkm_filter <minmum fpkm filter> --output <output file> --help <help on usage of script> --help <help on usage of script>\n\n";

my ($in_file, $fpkm_filter, $output, $help);

Getopt::Long::GetOptions('in_file=s' => \$in_file,
                         'fpkm_filter=i' => \$fpkm_filter,
                         'output=s' => \$output,
                         'h|help' => \$help
                         );

if (defined($help)) {
    die $usage;
}

# Open input file to retain only desired samples with large enough FPKM values
open (my $in_file_fh, '<', $in_file) or die "Can't open $in_file\n\n";

# Open output file to print to
open (my $output_fh, '>', $output) or die "Can't open $output\n\n";

# Retain header information in output file
my $header = <$in_file_fh>;
chomp $header;
print $output_fh "$header\n";

# Search each line of file and only retain row if it contains 
# at least one column with a value greater than 'fpkm' filter
while (my $line = <$in_file_fh>) {
    chomp $line;

    # Put all data into an array
    my @row = split (",", $line);

    # Loop through array and retain information if contains a value greater than set filter
    foreach my $value (@row) {
        # Determine if value is a number to evaluate
        if ( $value =~ /^[0-9,.E]+$/ ) {
            # Determine if value is greater than or equal to filter value
            if ($value >= $fpkm_filter) {
                my $passed = join (",", @row);
                print $output_fh "$passed\n";
                last;
            }
        }
    }
}

close $in_file_fh;
close $output_fh;

exit;
