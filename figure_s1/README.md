# figure_s1

## brachy_interesting_fpkm.txt
An expression matrix for all of the gene IDs associated with susceptibility gene orthogroups in B. distachyon.

## wheat_interesting_fpkm.txt
An expression matrix for all of the gene IDs associated with susceptibility gene orthogroups in T. aestivum

## figs1_metadata.txt
An association file between the abbreviated names of susceptibility candidates from other organisms and the orthogroups they are in.

## S_gene_full_orthogroups.txt
An association file between orthogroup number and the gene IDs in candidate susceptibility gene orthogroups

## Fig_S1_testin_all_genes.R
All 4 files are read in and metadata is added to expression information.

0.001 is added to all expression values to facilitate log2fc calculations (can't divide by 0)

metadata is extracted from column names

Mean, quotient, and log2fc are calculated

An expression plot with all of the candidate susceptibility orthogroups is created and saved