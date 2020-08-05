
#if (!requireNamespace("BiocManager", quietly = TRUE))
 #  install.packages("BiocManager")
#BiocManager::install("ggtree")

library(ape)
library(ggtree)
library(ggplot2)
setwd("~/Documents/MPGI_paper/Phylogeny_results")

#ADH
ADH_phylo <- read.newick(file = "ADH_updated.txt")

color_vec <- c(rep("black", 8), "red", rep("black", 14))
d = data.frame(node=1:23, color=color_vec, 23, replace=T)
ADH_plot <- ggtree(ADH_phylo) %<+% d + aes(color=I(color)) + geom_treescale(x = 0.17, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 0.25)

ggsave("ADH_phylogenetic_tree.pdf", plot = ADH_plot, device = "pdf", width = 7.5, height = 1.7, units = "in")

#BI-1
BI1_phylo <- read.newick(file = "BI1_phylogeny.txt")

color_vec1 <- c(rep("black", 0), "red", rep("black", 7))
d1 = data.frame(node=1:8, color=color_vec1, 8, replace=T)
BI1_plot <- ggtree(BI1_phylo) %<+% d1 + aes(color=I(color)) + geom_treescale(x = 0.084, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 0.125)

ggsave("BI1_phylogenetic_tree.pdf", plot = BI1_plot, device = "pdf", width = 7.5, height = 1.25, units = "in")

#DMR6
DMR6_phylo <- read.newick(file = "DMR6_updated_phylogeny.txt")

color_vec2 <- c(rep("black", 4), "red", "red", rep("black", 6))
d2 = data.frame(node=1:12, color=color_vec2, 12, replace=T)
DMR6_plot <- ggtree(DMR6_phylo) %<+% d2 + aes(color=I(color)) + geom_treescale(x = 0.47, y = -.8, fontsize = 3) + geom_tiplab(size = 3) + xlim(-.001, 0.7)

ggsave("DMR6_phylogenetic_tree.pdf", plot = DMR6_plot, device = "pdf", width = 7.5, height = 1.3, units = "in")

#DND1 does not need one

#PMR4
PMR4_phylo <- read.newick(file = "PMR4_phylogeny.txt")

color_vec3 <- c(rep("black", 0), "red", "red", rep("black", 12))
d3 = data.frame(node=1:14, color=color_vec3, 14, replace=T)
PMR4_plot <- ggtree(PMR4_phylo) %<+% d3 + aes(color=I(color)) + geom_treescale(x = 0.6, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 0.9)

ggsave("PMR4_phylogenetic_tree.pdf", plot = PMR4_plot, device = "pdf", width = 7.5, height = 1.3, units = "in")

#VAD1
VAD1_phylo <- read.newick(file = "VAD1_phylogeny.txt")

color_vec4 <- c(rep("black", 0), "red", rep("black", 7))
d4 = data.frame(node=1:8, color=color_vec4, 8, replace=T)
VAD1_plot <- ggtree(VAD1_phylo) %<+% d4 + aes(color=I(color)) + geom_treescale(x = 1.067, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 1.6)

ggsave("VAD1_phylogenetic_tree.pdf", plot = VAD1_plot, device = "pdf", width = 7.5, height = 1.3, units = "in")
