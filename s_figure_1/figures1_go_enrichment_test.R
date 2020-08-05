# GO Enrichment test for DE genes
# Used to make figure 2 of MPGI mansucript

# Install packages
source("https://bioconductor.org/biocLite.R")
biocLite("AnnotationForge")
# biocLite("clusterProfiler")

# Load packages
library(AnnotationForge)

# Set working directory
setwd("/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/ARCHIVE_FULL_GO")

## For Brachypodium
# Read in list of all genes ids: genes with GO terms and not GO terms
bd_genes = read.csv("BD.geneList.csv", sep="\t", header=TRUE)

# Need common column between dataframes when making the DB so add an extra "dummy" column
bd_genes["LOCUS"] <- bd_genes$GID

####
# Steps for making the db 
# Read in list of all genes with GO annotation
bd_go = read.csv("BD.gene.go", sep="\t", header=TRUE)

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
               species="distachyon2",
               goTable="go" )

install.packages("./org.Bdistachyon2.eg.db", repos=NULL, type="source")
####

## For Wheat
# Read in list of all genes ids: genes with GO terms and not GO terms
wheat_genes = read.csv("wheat.geneList", sep="\t", header=TRUE)

# Need common column between dataframes when making the DB so add an extra "dummy" column
wheat_genes["LOCUS"] <- wheat_genes$GID

####
# Steps for making the db 
# Read in list of all genes with GO annotation
wheat_go = read.csv("wheat-gene.go", sep="\t", header=TRUE)

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
library(org.Bdistachyon2.eg.db)
library(org.Taest.eg.db)
library(clusterProfiler)

# Read in up/down DE gene lists
## Brachypodium 6 comparisons
b2dpiUP = read.csv("filtered_DE_bd21-3_2dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
b2dpiUP = unique(b2dpiUP$GID)
b4dpiUP = read.csv("filtered_DE_bd21-3_4dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
b4dpiUP = unique(b4dpiUP$GID)
b6dpiUP = read.csv("filtered_DE_bd21-3_6dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
b6dpiUP = unique(b6dpiUP$GID)

b2dpiDOWN = read.csv("filtered_DE_bd21-3_2dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
b2dpiDOWN = unique(b2dpiDOWN$GID)
b4dpiDOWN = read.csv("filtered_DE_bd21-3_4dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
b4dpiDOWN = unique(b4dpiDOWN$GID)
b6dpiDOWN = read.csv("filtered_DE_bd21-3_6dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
b6dpiDOWN = unique(b6dpiDOWN$GID)

## W2691 6 comparisons
w2dpiUP = read.csv("filtered_DE_w2691_2dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
w2dpiUP = unique(w2dpiUP$GID)
w4dpiUP = read.csv("filtered_DE_w2691_4dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
w4dpiUP = unique(w4dpiUP$GID)
w6dpiUP = read.csv("filtered_DE_w2691_6dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
w6dpiUP = unique(w6dpiUP$GID)

w2dpiDOWN = read.csv("filtered_DE_w2691_2dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
w2dpiDOWN = unique(w2dpiDOWN$GID)
w4dpiDOWN = read.csv("filtered_DE_w2691_4dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
w4dpiDOWN = unique(w4dpiDOWN$GID)
w6dpiDOWN = read.csv("filtered_DE_w2691_6dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
w6dpiDOWN = unique(w6dpiDOWN$GID)

## Sr9b 6 comparisons
s2dpiUP = read.csv("filtered_DE_sr9b_2dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
s2dpiUP = unique(s2dpiUP$GID)
s4dpiUP = read.csv("filtered_DE_sr9b_4dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
s4dpiUP = unique(s4dpiUP$GID)
s6dpiUP = read.csv("filtered_DE_sr9b_6dpi_0.05_1_up_names_GO.csv", sep="\t", header=TRUE)
s6dpiUP = unique(s6dpiUP$GID)

s2dpiDOWN = read.csv("filtered_DE_sr9b_2dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
s2dpiDOWN = unique(s2dpiDOWN$GID)
s4dpiDOWN = read.csv("filtered_DE_sr9b_4dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
s4dpiDOWN = unique(s4dpiDOWN$GID)
s6dpiDOWN = read.csv("filtered_DE_sr9b_6dpi_0.05_1_down_names_GO.csv", sep="\t", header=TRUE)
s6dpiDOWN = unique(s6dpiDOWN$GID)


# Conduct enrichments test for each comparison for MF, BP, CC (3 x 6 * 3 = 54 enrichment tests)
dpi <- c("2", "4", "6")
exp <- c("UP", "DOWN")
ont <- c("MF", "BP", "CC")

## Brachypodium
# Set up variables
b2dpiUP_enrichedMF <- "b2dpiUP_enrichedMF"
b2dpiUP_enrichedBP <- "b2dpiUP_enrichedBP"
b2dpiUP_enrichedCC <- "b2dpiUP_enrichedCC"
b4dpiUP_enrichedMF <- "b4dpiUP_enrichedMF"
b4dpiUP_enrichedBP <- "b4dpiUP_enrichedBP"
b4dpiUP_enrichedCC <- "b4dpiUP_enrichedCC"
b6dpiUP_enrichedMF <- "b6dpiUP_enrichedMF"
b6dpiUP_enrichedBP <- "b6dpiUP_enrichedBP"
b6dpiUP_enrichedCC <- "b6dpiUP_enrichedCC"

b2dpiDOWN_enrichedMF <- "b2dpiDOWN_enrichedMF"
b2dpiDOWN_enrichedBP <- "b2dpiDOWN_enrichedBP"
b2dpiDOWN_enrichedCC <- "b2dpiDOWN_enrichedCC"
b4dpiDOWN_enrichedMF <- "b4dpiDOWN_enrichedMF"
b4dpiDOWN_enrichedBP <- "b4dpiDOWN_enrichedBP"
b4dpiDOWN_enrichedCC <- "b4dpiDOWN_enrichedCC"
b6dpiDOWN_enrichedMF <- "b6dpiDOWN_enrichedMF"
b6dpiDOWN_enrichedBP <- "b6dpiDOWN_enrichedBP"
b6dpiDOWN_enrichedCC <- "b6dpiDOWN_enrichedCC"

# Loop GO enrichment tests for Bd21-3
for (i in dpi) {
    print(i)
        for(j in exp) {
            print(j)
            for(k in ont) {
                print(k)
                n <- paste("b", i, "dpi", j, "_enriched", k, sep="")
                print(n)
                n <- get(n)
                print(n)
                g <- paste("b", i, "dpi", j, sep="")
                g <- get(g)
                print(g)
                m <- enrichGO(gene = g,
                              universe = bd_genes$LOCUS,
                              OrgDb = org.Bdistachyon2.eg.db,
                              keyType = "LOCUS", 
                              ont = k,
                              pAdjustMethod = "holm",
                              pvalueCutoff = 0.05,
                              readable = FALSE
                              )
                tmp <- simplify(m, cutoff=0.7, by="p.adjust")
                write(print(n), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE)
                write.table(as.data.frame(m), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE)
            }
        }
}


## W2691
# Set up variables
w2dpiUP_enrichedMF <- "w2dpiUP_enrichedMF"
w2dpiUP_enrichedBP <- "w2dpiUP_enrichedBP"
w2dpiUP_enrichedCC <- "w2dpiUP_enrichedCC"
w4dpiUP_enrichedMF <- "w4dpiUP_enrichedMF"
w4dpiUP_enrichedBP <- "w4dpiUP_enrichedBP"
w4dpiUP_enrichedCC <- "w4dpiUP_enrichedCC"
w6dpiUP_enrichedMF <- "w6dpiUP_enrichedMF"
w6dpiUP_enrichedBP <- "w6dpiUP_enrichedBP"
w6dpiUP_enrichedCC <- "w6dpiUP_enrichedCC"

w2dpiDOWN_enrichedMF <- "w2dpiDOWN_enrichedMF"
w2dpiDOWN_enrichedBP <- "w2dpiDOWN_enrichedBP"
w2dpiDOWN_enrichedCC <- "w2dpiDOWN_enrichedCC"
w4dpiDOWN_enrichedMF <- "w4dpiDOWN_enrichedMF"
w4dpiDOWN_enrichedBP <- "w4dpiDOWN_enrichedBP"
w4dpiDOWN_enrichedCC <- "w4dpiDOWN_enrichedCC"
w6dpiDOWN_enrichedMF <- "w6dpiDOWN_enrichedMF"
w6dpiDOWN_enrichedBP <- "w6dpiDOWN_enrichedBP"
w6dpiDOWN_enrichedCC <- "w6dpiDOWN_enrichedCC"

# Loop GO enrichment tests for W2691
for (i in dpi) {
    print(i)
    for(j in exp) {
        print(j)
        for(k in ont) {
            print(k)
            n <- paste("w", i, "dpi", j, "_enriched", k, sep="")
            print(n)
            n <- get(n)
            print(n)
            g <- paste("w", i, "dpi", j, sep="")
            g <- get(g)
            print(g)
            m <- enrichGO(gene = g,
                          universe = wheat_genes$LOCUS,
                          OrgDb = org.Taest.eg.db,
                          keyType = "LOCUS", 
                          ont = k,
                          pAdjustMethod = "holm",
                          pvalueCutoff = 0.05,
                          #qvalueCutoff = 0.05,
                          readable = FALSE
                          )
            
            write(print(n), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE)
            write.table(as.data.frame(m), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE) 
            
        }
    }
}


## Sr9b
# Set up variables
s2dpiUP_enrichedMF <- "s2dpiUP_enrichedMF"
s2dpiUP_enrichedBP <- "s2dpiUP_enrichedBP"
s2dpiUP_enrichedCC <- "s2dpiUP_enrichedCC"
s4dpiUP_enrichedMF <- "s4dpiUP_enrichedMF"
s4dpiUP_enrichedBP <- "s4dpiUP_enrichedBP"
s4dpiUP_enrichedCC <- "s4dpiUP_enrichedCC"
s6dpiUP_enrichedMF <- "s6dpiUP_enrichedMF"
s6dpiUP_enrichedBP <- "s6dpiUP_enrichedBP"
s6dpiUP_enrichedCC <- "s6dpiUP_enrichedCC"

s2dpiDOWN_enrichedMF <- "s2dpiDOWN_enrichedMF"
s2dpiDOWN_enrichedBP <- "s2dpiDOWN_enrichedBP"
s2dpiDOWN_enrichedCC <- "s2dpiDOWN_enrichedCC"
s4dpiDOWN_enrichedMF <- "s4dpiDOWN_enrichedMF"
s4dpiDOWN_enrichedBP <- "s4dpiDOWN_enrichedBP"
s4dpiDOWN_enrichedCC <- "s4dpiDOWN_enrichedCC"
s6dpiDOWN_enrichedMF <- "s6dpiDOWN_enrichedMF"
s6dpiDOWN_enrichedBP <- "s6dpiDOWN_enrichedBP"
s6dpiDOWN_enrichedCC <- "s6dpiDOWN_enrichedCC"

# Loop GO enrichment tests for Sr9b
for (i in dpi) {
    print(i)
    for(j in exp) {
        print(j)
        for(k in ont) {
            print(k)
            n <- paste("s", i, "dpi", j, "_enriched", k, sep="")
            print(n)
            n <- get(n)
            print(n)
            g <- paste("s", i, "dpi", j, sep="")
            g <- get(g)
            print(g)
            m <- enrichGO(gene = g,
                          universe = wheat_genes$LOCUS,
                          OrgDb = org.Taest.eg.db,
                          keyType = "LOCUS", 
                          ont = k,
                          pAdjustMethod = "holm",
                          pvalueCutoff = 0.05,
                          #qvalueCutoff = 0.05,
                          readable = FALSE
                          )
            
            write(print(n), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE)
            write.table(as.data.frame(m), file="/Users/evahenningsen/Documents/GitHub/cory/MPGI/figure2/GOenrichment_output.txt", sep="\t", append=TRUE) 
            
        }
    }
}


# GO enrichment test for Biological Processes (BP) terms
# b6dpiUP_enrichedBP <- enrichGO(gene = b6dpiUP, 
#                                universe = bd_genes$LOCUS,
#                                OrgDb = org.Bdistachyon.eg.db,
#                                keyType = "LOCUS", 
#                                ont = "BP",
#                                pAdjustMethod = "holm",
#                                #pvalueCutoff = 0.05,
#                                qvalueCutoff = 0.05,
#                                readable = FALSE)

#tmp <- simplify(b2dpiUP_enrichedMF, cutoff=0.7, by="p.adjust")

# https://shiring.github.io/genome/2017/01/05/homologous_genes_part3_post
# 
# ggplot(plot_df, aes(x = Species, y = Description, fill = GR)) + 
#     geom_tile(width = 1, height = 1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") +
#     facet_grid(Category ~ ., scales = "free_y") +
#     scale_x_discrete(position = "top") +
#     labs(
#         title = paste("Top", cutoff, "enriched GO terms"),
#         x = "",
#         y = ""
#     )
# 
# 
# 
# Genotype <- c("W2691", "Sr9b", "Bd21-3", "W2691", "Sr9b", "Bd21-3")
# dpi <- c(2, 2, 2, 4, 4, 4)
# GO <- c("test go annotation", "test go annotation", "test go annotation", "test go annotation", "test go annotation", "HELP")
# pvalue <- c(0.04, 0.02, 0.01, 0.04, .02, .01)
# gene_ratio <- c(.2,.1,.3, .15, .3, 0.3)
# category <- c("MF", "MF", "CC", "MF", "MF", "CC")
# enrichment <- c("UP", "UP", "DOWN", "DOWN", "UP", "UP")
# 
# p <- as.data.frame(cbind(Genotype, dpi, GO, pvalue, gene_ratio, category, enrichment))
# p$pvalue <- as.numeric(as.character(p$pvalue))
# p$gene_ratio <- as.numeric(as.character(p$gene_ratio))
# #p$dpi <- as.numeric(as.character(p$dpi))
# 
# ggplot(p, aes(x = dpi, y = GO, fill = gene_ratio)) +
#     geom_tile(width = .5, height = .5) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") +
#     facet_grid(enrichment ~ category ~ Genotype, scales = "free_y") +
#     theme(
#         axis.text = element_text(size = 10),
#         axis.title = element_text(size = 10),
#         panel.grid.major = element_line(color = "black"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         strip.text = element_text(face = "bold", size = 10, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         legend.box.background = element_rect(fill = "transparent")
#         )

# genotype, dpi, go annotation, go term, pvalue, gene ratio
 
 
 







