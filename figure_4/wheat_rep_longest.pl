#! /usr/bin/perl -w

# wheat_rep_longest.pl

# This script will take a wheat peptide fasta file and determine and retain only the longest peptide for each locus in the output file

# Cory Hirsch
# August 16 2017

use strict;
use Getopt::Long();
use Bio::SeqIO;

my $usage = "\nUsage: $0 --pep_in <peptide seq file> --rep_out <longest representative peptide output file> --help <help on usage of script>\n\n";

my ($pep_in, $rep_out, $help);

Getopt::Long::GetOptions('pep_in=s' => \$pep_in,
             'rep_out=s' => \$rep_out,
             'h|help' => \$help
            );

if (defined($help)) {
    die $usage;
}

# Use Bioperl module to call in sequences
my $inseq = Bio::SeqIO->new(
                -file   => "<$pep_in",
                -format => 'Fasta',
               );

my %longest;
my $short_id;
# Call in each sequence in the file 
while (my $seq = $inseq->next_seq) {
    # Get the sequence identifier, the sequence, the length of the sequence, and the description
    my $id = $seq->display_id;
    my $sequence = $seq->seq;
    my $len = $seq->length();
    my $desc = $seq->desc();
    print "$id\t$desc\n";

    # get rid of transcript id from sequence identifier
    if ($id =~ /^Traes/) {
        $id =~ /(Traes\w+)\.\d+/;
        $short_id = $1;
        print "$short_id\n";
    }
    
    # Retain information for longest transcript in the hash
    # gene -> chr, start, stop, length, sequence
    if (exists $longest{$short_id}) {
        my $hash_length = $longest{$short_id}{'LENGTH'};
        if ($hash_length < $len) {
            $longest{$short_id}{'LENGTH'} = $len;
            $longest{$short_id}{'SEQ'} = $sequence;
            $longest{$short_id}{'GENE'} = $id;
        }
    }
    if (! (exists $longest{$short_id}) ) {
        $longest{$short_id}{'LENGTH'} = $len;
        $longest{$short_id}{'SEQ'} = $sequence;
        $longest{$short_id}{'GENE'} = $id;
    }
}

# Open Bioperl for longetst transcript output file
my $seqout = Bio::SeqIO->new(
                 -file   => ">$rep_out",
                 -format => 'Fasta',
                );

foreach my $key (keys %longest) {
    my $seq = Bio::Seq->new(
                            -seq => $longest{$key}{'SEQ'},
                            -id  => $longest{$key}{'GENE'},
                            );
    $seqout->write_seq($seq);
}

exit;