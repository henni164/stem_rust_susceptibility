# GO Enrichment test for DE genes
# Used to make figure 2 of MPGI mansucript

# Install packages
#source("https://bioconductor.org/biocLite.R")
#biocLite("AnnotationForge")
# biocLite("clusterProfiler")

# Load packages
library(AnnotationForge)

# Set working directory
setwd("/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_input")

## For Brachypodium
# Read in list of all genes ids: genes with GO terms and not GO terms
bd_genes = read.csv("BD.geneList.csv", sep="\t", header=TRUE)

# Need common column between dataframes when making the DB so add an extra "dummy" column
bd_genes["LOCUS"] <- bd_genes$GID

####
# Steps for making the db 
# Read in list of all genes with GO annotation
bd_go = read.csv("BD.gene.slim.go", sep="\t", header=TRUE)

# Need to have an evidence column for GO dataframe
bd_go["EVIDENCE"] <- "IEA"
bd_genes$GID <- as.character(bd_genes$GID)
bd_go$GID <- as.character(bd_go$GID)

# Make OrgDB
makeOrgPackage(gene_info=bd_genes,
               go=bd_go,
               version="0.1",
               maintainer="Cory Hirsch <cdhirsch@umn.edu>",
               author="Cory Hirsch <cdhirsch@umn.edu>",
               outputDir = ".",
               tax_id="12062",
               genus="Brachypodium",
               species="distachyon",
               goTable="go" )

install.packages("./org.Bdistachyon.eg.db", repos=NULL, type="source")
####

## For Wheat
# Read in list of all genes ids: genes with GO terms and not GO terms
wheat_genes = read.csv("wheat.geneList", sep="\t", header=TRUE)

# Need common column between dataframes when making the DB so add an extra "dummy" column
wheat_genes["LOCUS"] <- wheat_genes$GID

####
# Steps for making the db 
# Read in list of all genes with GO annotation
wheat_go = read.csv("wheat_slim.go", sep="\t", header=TRUE)

# Need to have an evidence column for GO dataframe
wheat_go["EVIDENCE"] <- "IEA"
wheat_genes$GID <- as.character(wheat_genes$GID)
wheat_go$GID <- as.character(wheat_go$GID)

# Make OrgDB
makeOrgPackage(gene_info=wheat_genes,
               go=wheat_go,
               version="0.1",
               maintainer="Cory Hirsch <cdhirsch@umn.edu>",
               author="Cory Hirsch <cdhirsch@umn.edu>",
               outputDir = ".",
               tax_id="4565",
               genus="Triticum",
               species="aest",
               goTable="go"
               )

install.packages("./org.Taest.eg.db", repos=NULL, type="source")
####

####
# Test for GO enrichment for DE gene set in genotypes and days
# GO over-representation test
# Options for "ont" are molecular function (MF), biological process (BP), and cellular component (CC)
# Options for pAdjustMethod are "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"

# Load in OrgDB packages
library(org.Bdistachyon.eg.db)
library(org.Taest.eg.db)
library(clusterProfiler)

# Read in up/down DE gene lists
## Brachypodium 6 comparisons
BDIBD213.1G0110600 = read.table("Bd21_BDIBD213.1G0110600_expanded.txt", sep="\t", header=TRUE)
BDIBD213.1G0110600 = unique(BDIBD213.1G0110600$GID)

BDIBD213.1G1026800 = read.table("Bd21_BDIBD213.1G1026800_expanded.txt", sep="\t", header=TRUE)
BDIBD213.1G1026800 = unique(BDIBD213.1G1026800$GID)

BDIBD213.2G0590100 = read.table("Bd21_BDIBD213.2G0590100_expanded.txt", sep="\t", header=TRUE)
BDIBD213.2G0590100 = unique(BDIBD213.2G0590100$GID)

BDIBD213.2G0641600 = read.table("Bd21_BDIBD213.2G0641600_expanded.txt", sep="\t", header=TRUE)
BDIBD213.2G0641600 = unique(BDIBD213.2G0641600$GID)

BDIBD213.3G0030500 = read.table("Bd21_BDIBD213.3G0030500_expanded.txt", sep="\t", header=TRUE)
BDIBD213.3G0030500 = unique(BDIBD213.3G0030500$GID)

BDIBD213.4G0315600 = read.table("Bd21_BDIBD213.4G0315600_expanded.txt", sep="\t", header=TRUE)
BDIBD213.4G0315600 = unique(BDIBD213.4G0315600$GID)

## W2691
W2691_TRAESCS2A01G231900 = read.table("W2691_TRAESCS2A01G231900_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS2A01G231900 = unique(W2691_TRAESCS2A01G231900$GID)

W2691_TRAESCS2B01G253500 = read.table("W2691_TRAESCS2B01G253500_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS2B01G253500 = unique(W2691_TRAESCS2B01G253500$GID)

W2691_TRAESCS2D01G236800 = read.table("W2691_TRAESCS2D01G236800_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS2D01G236800 = unique(W2691_TRAESCS2D01G236800$GID)

W2691_TRAESCS3D01G246000 = read.table("W2691_TRAESCS3D01G246000_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS3D01G246000 = unique(W2691_TRAESCS3D01G246000$GID)

W2691_TRAESCS4B01G106300 = read.table("W2691_TRAESCS4B01G106300_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS4B01G106300 = unique(W2691_TRAESCS4B01G106300$GID)

W2691_TRAESCS4B01G346900 = read.table("W2691_TRAESCS4B01G346900_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS4B01G346900 = unique(W2691_TRAESCS4B01G346900$GID)

W2691_TRAESCS4D01G341800 = read.table("W2691_TRAESCS4D01G341800_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS4D01G341800 = unique(W2691_TRAESCS4D01G341800$GID)

W2691_TRAESCS5A01G515600 = read.table("W2691_TRAESCS5A01G515600_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS5A01G515600 = unique(W2691_TRAESCS5A01G515600$GID)

W2691_TRAESCS5D01G404600 = read.table("W2691_TRAESCS5D01G404600_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS5D01G404600 = unique(W2691_TRAESCS5D01G404600$GID)

W2691_TRAESCS6A01G083200 = read.table("W2691_TRAESCS6A01G083200_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS6A01G083200 = unique(W2691_TRAESCS6A01G083200$GID)

W2691_TRAESCS6B01G113900 = read.table("W2691_TRAESCS6B01G113900_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS6B01G113900 = unique(W2691_TRAESCS6B01G113900$GID)

W2691_TRAESCS6D01G077000 = read.table("W2691_TRAESCS6D01G077000_expanded.txt", sep="\t", header=TRUE)
W2691_TRAESCS6D01G077000 = unique(W2691_TRAESCS6D01G077000$GID)

## Sr9b
Sr9b_TRAESCS2A01G231900 = read.table("Sr9b_TRAESCS2A01G231900_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS2A01G231900 = unique(Sr9b_TRAESCS2A01G231900$GID)

Sr9b_TRAESCS2B01G253500 = read.table("Sr9b_TRAESCS2B01G253500_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS2B01G253500 = unique(Sr9b_TRAESCS2B01G253500$GID)

Sr9b_TRAESCS2D01G236800 = read.table("Sr9b_TRAESCS2D01G236800_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS2D01G236800 = unique(Sr9b_TRAESCS2D01G236800$GID)

Sr9b_TRAESCS3D01G246000 = read.table("Sr9b_TRAESCS3D01G246000_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS3D01G246000 = unique(Sr9b_TRAESCS3D01G246000$GID)

Sr9b_TRAESCS4B01G106300 = read.table("Sr9b_TRAESCS4B01G106300_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS4B01G106300 = unique(Sr9b_TRAESCS4B01G106300$GID)

Sr9b_TRAESCS4B01G346900 = read.table("Sr9b_TRAESCS4B01G346900_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS4B01G346900 = unique(Sr9b_TRAESCS4B01G346900$GID)

Sr9b_TRAESCS4D01G341800 = read.table("Sr9b_TRAESCS4D01G341800_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS4D01G341800 = unique(Sr9b_TRAESCS4D01G341800$GID)

Sr9b_TRAESCS5A01G515600 = read.table("Sr9b_TRAESCS5A01G515600_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS5A01G515600 = unique(Sr9b_TRAESCS5A01G515600$GID)

Sr9b_TRAESCS5D01G404600 = read.table("Sr9b_TRAESCS5D01G404600_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS5D01G404600 = unique(Sr9b_TRAESCS5D01G404600$GID)

Sr9b_TRAESCS6A01G083200 = read.table("Sr9b_TRAESCS6A01G083200_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS6A01G083200 = unique(Sr9b_TRAESCS6A01G083200$GID)

Sr9b_TRAESCS6B01G113900 = read.table("Sr9b_TRAESCS6B01G113900_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS6B01G113900 = unique(Sr9b_TRAESCS6B01G113900$GID)

Sr9b_TRAESCS6D01G077000 = read.table("Sr9b_TRAESCS6D01G077000_expanded.txt", sep="\t", header=TRUE)
Sr9b_TRAESCS6D01G077000 = unique(Sr9b_TRAESCS6D01G077000$GID)

#Bd21-3
BDIBD213.1G0110600_enrichment <- enrichGO(gene = BDIBD213.1G0110600,
              universe = bd_genes$LOCUS,
              OrgDb = org.Bdistachyon.eg.db,
              keyType = "LOCUS", 
              ont = "ALL",
              pAdjustMethod = "holm",
              pvalueCutoff = 0.05,
              readable = FALSE)
write.table(BDIBD213.1G0110600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.1G0110600_goslim_enrichment.txt", sep = "\t")

BDIBD213.1G1026800_enrichment <- enrichGO(gene = BDIBD213.1G1026800,
                universe = bd_genes$LOCUS,
                OrgDb = org.Bdistachyon.eg.db,
                keyType = "LOCUS",
                ont = "ALL",
                pAdjustMethod = "holm",
                pvalueCutoff = 0.05,
                readable = FALSE)
write.table(BDIBD213.1G1026800_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.1G1026800_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS2A01G231900_enrichment <- enrichGO(gene = Sr9b_TRAESCS2A01G231900,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS2A01G231900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS2A01G231900_goslim_enrichment.txt", sep = "\t")


BDIBD213.2G0590100_enrichment <- enrichGO(gene = BDIBD213.2G0590100,
              universe = bd_genes$LOCUS,
              OrgDb = org.Bdistachyon.eg.db,
              keyType = "LOCUS", 
              ont = "ALL",
              pAdjustMethod = "holm",
              pvalueCutoff = 0.05,
              #qvalueCutoff = 0.05,
              readable = FALSE)

write.table(BDIBD213.2G0590100_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.2G0590100_goslim_enrichment.txt", sep = "\t")


           
BDIBD213.2G0641600_enrichment <- enrichGO(gene = BDIBD213.2G0641600,
              universe = bd_genes$LOCUS,
              OrgDb = org.Bdistachyon.eg.db,
              keyType = "LOCUS", 
              ont = "ALL",
              pAdjustMethod = "holm",
              pvalueCutoff = 0.05,
              #qvalueCutoff = 0.05,
              readable = FALSE)
       
write.table(BDIBD213.2G0641600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.2G0641600_goslim_enrichment.txt", sep = "\t")

BDIBD213.3G0030500_enrichment <- enrichGO(gene = BDIBD213.3G0030500,
                                          universe = bd_genes$LOCUS,
                                          OrgDb = org.Bdistachyon.eg.db,
                                          keyType = "LOCUS", 
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          #qvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(BDIBD213.3G0030500_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.3G0030500_goslim_enrichment.txt", sep = "\t")

BDIBD213.4G0315600_enrichment <- enrichGO(gene = BDIBD213.4G0315600,
                                          universe = bd_genes$LOCUS,
                                          OrgDb = org.Bdistachyon.eg.db,
                                          keyType = "LOCUS", 
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          #qvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(BDIBD213.4G0315600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/BDIBD213.4G0315600_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS2B01G253500_enrichment <- enrichGO(gene = Sr9b_TRAESCS2B01G253500,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS2B01G253500_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS2B01G253500_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS2D01G236800_enrichment <- enrichGO(gene = Sr9b_TRAESCS2D01G236800,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS2D01G236800_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS2D01G236800_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS3D01G246000_enrichment <- enrichGO(gene = Sr9b_TRAESCS3D01G246000,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS3D01G246000_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS3D01G246000_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS4B01G106300_enrichment <- enrichGO(gene = Sr9b_TRAESCS4B01G106300,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS4B01G106300_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS4B01G106300_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS4B01G346900_enrichment <- enrichGO(gene = Sr9b_TRAESCS4B01G346900,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS4B01G346900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS4B01G346900_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS4D01G341800_enrichment <- enrichGO(gene = Sr9b_TRAESCS4D01G341800,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS4D01G341800_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS4D01G341800_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS5A01G515600_enrichment <- enrichGO(gene = Sr9b_TRAESCS5A01G515600,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS5A01G515600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS5A01G515600_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS5D01G404600_enrichment <- enrichGO(gene = Sr9b_TRAESCS5D01G404600,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS5D01G404600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS5D01G404600_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS6A01G083200_enrichment <- enrichGO(gene = Sr9b_TRAESCS6A01G083200,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS6A01G083200_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS6A01G083200_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS6B01G113900_enrichment <- enrichGO(gene = Sr9b_TRAESCS6B01G113900,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS6B01G113900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS6B01G113900_goslim_enrichment.txt", sep = "\t")

Sr9b_TRAESCS6D01G077000_enrichment <- enrichGO(gene = Sr9b_TRAESCS6D01G077000,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(Sr9b_TRAESCS6D01G077000_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Sr9b_TRAESCS6D01G077000_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS2A01G231900_enrichment <- enrichGO(gene = W2691_TRAESCS2A01G231900,
                                          universe = wheat_genes$LOCUS,
                                          OrgDb = org.Taest.eg.db,
                                          keyType = "LOCUS",
                                          ont = "ALL",
                                          pAdjustMethod = "holm",
                                          pvalueCutoff = 0.05,
                                          readable = FALSE)

write.table(W2691_TRAESCS2A01G231900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS2A01G231900_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS2B01G253500_enrichment <- enrichGO(gene = W2691_TRAESCS2B01G253500,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS2B01G253500_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS2B01G253500_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS2D01G236800_enrichment <- enrichGO(gene = W2691_TRAESCS2D01G236800,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS2D01G236800_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS2D01G236800_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS3D01G246000_enrichment <- enrichGO(gene = W2691_TRAESCS3D01G246000,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS3D01G246000_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS3D01G246000_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS4B01G106300_enrichment <- enrichGO(gene = W2691_TRAESCS4B01G106300,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS4B01G106300_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS4B01G106300_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS4B01G346900_enrichment <- enrichGO(gene = W2691_TRAESCS4B01G346900,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS4B01G346900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS4B01G346900_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS4D01G341800_enrichment <- enrichGO(gene = W2691_TRAESCS4D01G341800,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS4D01G341800_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS4D01G341800_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS5A01G515600_enrichment <- enrichGO(gene = W2691_TRAESCS5A01G515600,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS5A01G515600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS5A01G515600_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS5D01G404600_enrichment <- enrichGO(gene = W2691_TRAESCS5D01G404600,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS5D01G404600_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS5D01G404600_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS6A01G083200_enrichment <- enrichGO(gene = W2691_TRAESCS6A01G083200,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS6A01G083200_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS6A01G083200_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS6B01G113900_enrichment <- enrichGO(gene = W2691_TRAESCS6B01G113900,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS6B01G113900_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS6B01G113900_goslim_enrichment.txt", sep = "\t")

W2691_TRAESCS6D01G077000_enrichment <- enrichGO(gene = W2691_TRAESCS6D01G077000,
                                                universe = wheat_genes$LOCUS,
                                                OrgDb = org.Taest.eg.db,
                                                keyType = "LOCUS",
                                                ont = "ALL",
                                                pAdjustMethod = "holm",
                                                pvalueCutoff = 0.05,
                                                readable = FALSE)

write.table(W2691_TRAESCS6D01G077000_enrichment, file = "/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/W2691_TRAESCS6D01G077000_goslim_enrichment.txt", sep = "\t")
