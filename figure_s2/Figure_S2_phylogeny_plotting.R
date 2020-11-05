#library(BiocManager)
#BiocManager::install("ggtree")

library(ape)
library(ggtree)
library(ggplot2)
library(treeio)
library(ggpubr)
setwd("/Users/evahenningsen/Documents/MPGI_paper/Figure_4")

#AGD2
AGD2_phylo <- read.newick(file = "AGD2_newick.txt")

color_vec0 <- c(rep("black", 13))
d0 = data.frame(node=1:13, color=color_vec0, 13, replace=T)
AGD2_plot <- ggtree(AGD2_phylo) %<+% d0 + aes(color=I(color)) + geom_treescale(x = 0.243, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 0.33) + ggtitle("AGD2") + theme(title = element_text(size = 8))

#ggsave("AGD2_phylogenetic_tree.pdf", plot = AGD2_plot, device = "pdf", width = 7.5, height = 1.2, units = "in")

#BI-1
BI1_phylo <- read.newick(file = "BI1_newick.txt")

color_vec1 <- c(rep("black", 8))
d1 = data.frame(node=1:8, color=color_vec1, 8, replace=T)
BI1_plot <- ggtree(BI1_phylo) %<+% d1 + aes(color=I(color)) + geom_treescale(x = 0.072, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 0.1) + ggtitle("BI-1") + theme(title = element_text(size = 8))

#ggsave("BI1_phylogenetic_tree.pdf", plot = BI1_plot, device = "pdf", width = 7.5, height = 1.2, units = "in")

#DMR6
DMR6_phylo <- read.newick(file = "DMR6_newick.txt")

color_vec2 <- c(rep("black", 6))
d2 = data.frame(node=1:6, color=color_vec2, 6, replace=T)
DMR6_plot <- ggtree(DMR6_phylo) %<+% d2 + aes(color=I(color)) + geom_treescale(x = 0.47, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-.001, 0.65) + ggtitle("DMR6") + theme(title = element_text(size = 8))

#ggsave("DMR6_phylogenetic_tree.pdf", plot = DMR6_plot, device = "pdf", width = 7.5, height = 1.1, units = "in")

#DND1 does not need one

#FAH1
FAH1_phylo <- read.newick(file = "FAH1_newick.txt")

color_vec3 <- c(rep("black", 10))
d3 = data.frame(node=1:10, color=color_vec3, 10, replace=T)
FAH1_plot <- ggtree(FAH1_phylo) %<+% d3 + aes(color=I(color)) + geom_treescale(x = 0.345, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-.001, 0.48) + ggtitle("FAH1") + theme(title = element_text(size = 8))

#ggsave("FAH1_phylogenetic_tree.pdf", plot = FAH1_plot, device = "pdf", width = 7.5, height = 1.1, units = "in")
#IBR3
IBR3_phylo <- read.newick(file = "IBR3_newick.txt")

color_vec33 <- c(rep("black", 10))
d33 = data.frame(node=1:10, color=color_vec33, 10, replace=T)
IBR3_plot <- ggtree(IBR3_phylo) %<+% d33 + aes(color=I(color)) + geom_treescale(x = 0.375, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-.001, 0.52) + ggtitle("IBR3") + theme(title = element_text(size = 8))

#ggsave("IBR3_phylogenetic_tree.pdf", plot = IBR3_plot, device = "pdf", width = 7.5, height = 1.1, units = "in")
#VAD1
VAD1_phylo <- read.newick(file = "VAD1_newick.txt")

color_vec4 <- c(rep("black", 8))
d4 = data.frame(node=1:8, color=color_vec4, 8, replace=T)
VAD1_plot <- ggtree(VAD1_phylo) %<+% d4 + aes(color=I(color)) + geom_treescale(x = 1.17, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 1.6) + ggtitle("VAD1") + theme(title = element_text(size = 8))

#ggsave("VAD1_phylogenetic_tree.pdf", plot = VAD1_plot, device = "pdf", width = 7.5, height = 1.2, units = "in")

#WRKY25

WRKY25_phylo <- read.newick(file = "WRKY25_newick.txt")

color_vec5 <- c(rep("black", 10))
d5 = data.frame(node=1:10, color=color_vec5, 10, replace=T)
WRKY25_plot <- ggtree(WRKY25_phylo) %<+% d5 + aes(color=I(color)) + geom_treescale(x = 0.88, y = 0, fontsize = 3) + geom_tiplab(size = 3) + xlim(-0.001, 1.22) + ggtitle("WRKY25") + theme(title = element_text(size = 8))

#ggsave("WRKY25_phylogenetic_tree.pdf", plot = WRKY25_plot, device = "pdf", width = 7.5, height = 1.2, units = "in")


combined_plot <- ggarrange(AGD2_plot, BI1_plot, DMR6_plot, FAH1_plot, IBR3_plot, VAD1_plot, WRKY25_plot, nrow = 7, ncol = 1, heights = c(1.1, 1, 1,1.1,1,1.1,1.1))

ggsave(filename = "figure_s2_combined.tiff", combined_plot, device = "tiff", width = 7.5, height = 8, units = "in", scale = 1)

