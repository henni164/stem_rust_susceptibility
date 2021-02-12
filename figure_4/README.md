# figure_4
## brachy_interesting_fpkm.txt
The FPKM values for all of the genes in candidate susceptibility gene orthogroups for B. distachyon
## wheat_interesting_fpkm.txt
The FPKM values for all of the genes in candidate susceptibility gene orthogroups for T. aestivum
## S_gene_full_orthogroups
An association file containing orthogroup numbers and the genes in the orthogroup
## fig4_metadata.txt
An association file between the abbreviated names of susceptibility genes identified in other organisms and an orthogroup number

## Fig4_updated.R

This script reads in the expression matrices and adds 0.001 to make log2fc calculation possible later (can't divide by 0)

orthogroup and other metadata information is added to the main dataframes

Data is expanded to get genotype, dat, treatment, and rep information into columns

Then wheat + brachypodium data is combined

Mean, quotient, and log2fc are calculated with grouping on geneid, S_gene, genotype, and day

The 8 candidates are subsetted out and plotted individually, before being joined together and plotted at the end.