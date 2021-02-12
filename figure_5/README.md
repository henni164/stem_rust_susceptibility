# figure_5
## wheat_goslims_space.txt
An association file between T. aestivum gene IDs and GO terms in space-delimited format
## brachy_goslims_space.txt
An association file between B. distachyon gene IDs and GO terms in space-delimited format
## clusters_and_sgenes.txt
An association file between genotype, cluster number, and abbreviated susceptibility gene name
## /[genotype]_networks
These 3 folders contain association files with gene IDs and GO terms. Each file is named a number, which is the cluster number that the file represents. All of the genes in the cluster that have a GO term associated with it are in the file.

## Figure_5.R
The wheat/brachy_goslims_space.txt files are used to create the wheat/brachy GO universes.

Then, the files in the [genotype]_files folders are read into a list and used in the TOPgo enrichment test

Tests have to be repeated for the different GO ontologies (MF, CC, BP)

Finally, the results are combined and a plot is generated.

Scales are omitted in this plot to simplify visualization.