# figure 6
## brachy_counts_fpkm.txt
FPKM values for all brachy genes
## wheat_counts_fpkm.txt
FPKM values for all wheat genes
## network_clusters.SR_brachy_2.csv
Association file between Bd21-3 gene IDs and cluster numbers for all clusters
## network_cluster.SR_Sr9b_2.csv
Association file between Sr9b gene IDs and cluster numbers for all clusters
## network_cluster.SR_W2691_2.csv
Association file between W2691 gene IDs and cluster numbers for all clusters
## /[genotype]
These 3 folders contain files named "coexpr_scores_all_genes.SR_[genotype]_2.cluster_[#].txt"

These files are network files for whichever genotype and cluster are indicated in the file name

Format:

gene_a	gene_b	score

[geneID1]	[geneID2]	[z-score]

## Figure6_expression_plots.R

This R script plots the networks and expression graphs together.

### Network plots (top section)

The files in the /[genotype] folders are read into lists

Then the files are formatted as networks and a list of plots is made. 

The "important genes" layer is re-plotted in order for it to show up on top.

### Gene expression plots (bottom section)

The network_clusters.Sr_[genotype]_2.csv files are read in, as well as brachy_counts_fpkm.txt and wheat_counts_fpkm.txt

These are joined together to get important metadata, and then a variable is split to extract genotype, day, treatment, and rep information

0.001 is added to the expression values

various calculations were made, some for data exploration.

A list of cluster numbers is made and these are used to create a list of data subsets for each genotype.

Finally, the expression plots are put into a list and the "important genes" layer is plotted on top

At the very end, the networks and expression plots that were selected for visualization are combined, and labels are drawn on top of the large plot.

