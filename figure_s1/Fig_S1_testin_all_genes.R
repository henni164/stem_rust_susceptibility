# Figure S1 plotting code
## Eva Henningsen
## Started 10/30/18

#load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(reshape2)
library(tidyr)

## Data exploration for figure 3
# Directory for expression matrix files
setwd("/Users/evahenningsen/Documents/MPGI_files/Figure_3")

#Import filtered wheat and brachy expression matrices
# Decide between filtered or unfiltered data
d_wheat_1 <- read.csv("wheat_interesting_fpkm.txt", header = TRUE, sep = "\t")
d_bd21_1 <- read.csv("brachy_interesting_fpkm.txt", header = TRUE, sep = "\t")
orthogroup_dat <- read.delim("S_gene_full_orthogroups.txt", header = FALSE, sep = "\t")
colnames(orthogroup_dat) <- c("OG", "geneid")
# Want to be able to do calculations on 0 values. Will add 0.001 to each value
d_wheat_scaled <- d_wheat_1[,2:37] + 0.001
d_wheat <- cbind(d_wheat_1[,1], d_wheat_scaled)

d_bd21_scaled <- d_bd21_1[,2:19] + 0.001
d_bd21 <- cbind(d_bd21_1[,1], d_bd21_scaled)

metadat_dat <- read.delim("fig3_metadata.txt", header = TRUE, sep = "\t")
colnames(metadat_dat) <- c("S_gene", "OG")


colnames(d_bd21)[1] <- "geneid"
colnames(d_wheat)[1] <- "geneid"
wheat_orth <- left_join(d_wheat, orthogroup_dat, by = "geneid")
brachy_orth <- left_join(d_bd21, orthogroup_dat, by = "geneid")

wheat_ready_almost <- left_join(wheat_orth, metadat_dat, by = "OG")
brachy_ready_almost <- left_join(brachy_orth, metadat_dat, by = "OG")

wheat_ready <- wheat_ready_almost
brachy_ready <- brachy_ready_almost

brachy_long <- melt(brachy_ready, id.vars = c("geneid", "S_gene", "OG"))
brachy_w_met <- separate(data = brachy_long, col = variable, into = c("Genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)

wheat_long <- melt(wheat_ready, id.vars = c("geneid", "S_gene", "OG"))
wheat_w_met <- separate(data = wheat_long, col = variable, into = c("Genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)

all_data <- rbind(brachy_w_met, wheat_w_met)


all_data_rep_means <- all_data %>%
  group_by(geneid, S_gene, Genotype, day, treatment, OG) %>%
  summarise(Mean = mean(value)) %>%
  as.data.frame()

all_data_quotient <- all_data_rep_means %>%
  group_by(geneid, S_gene, Genotype, day, OG) %>%
  summarise(Quotient = (Mean[treatment == "treated"]/Mean[treatment == "mock"])) %>%
  as.data.frame()

all_data_log2fc <- all_data_quotient %>%
  group_by(geneid, S_gene, Genotype, day, OG) %>%
  summarise(Log2FC = log2(Quotient)) %>%
  as.data.frame()


all_data_log2fc$Genotype <- as.factor(all_data_log2fc$Genotype)
levels(all_data_log2fc$Genotype)
all_data_log2fc$Genotype <- factor(all_data_log2fc$Genotype, levels = c("W2691", "Sr9b", "Bd21"))
all_data_log2fc$S_gene <- as.factor(all_data_log2fc$S_gene)
all_data_log2fc$OG <- as.factor(all_data_log2fc$OG)

big_plot <- ggplot(all_data_log2fc, aes(x= day,
                                        y = Log2FC,
                                        group = interaction(Genotype, geneid),
                                        color = Genotype)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(size = 0) +
  facet_wrap(~ S_gene + OG) + 
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_x_discrete(labels = c("2","4","6")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain"),
        strip.text = element_text(size = 8, face = "italic"))

#Printing whole plot
#ggsave(filename = "all_S_no_fpkm_filter.tiff", plot = big_plot, device = "tiff", height = 10, width = 7.5, unit = "in")
mylegend <- get_legend(big_plot + theme(legend.position = "left", text = element_text(color = "black", size = 10), legend.text = element_text(color = "black", size = 10), legend.direction = "vertical"))
legend.plot <- as_ggplot(mylegend)
#ggsave(filename = "figure_legend_s1.pdf", plot = legend.plot, device = "pdf", height = 20, width = 100, unit = "mm")

together_plot <- ggdraw(big_plot) + draw_plot(mylegend, x = 0.7, y = -0.12, width = 0.4, height = 0.4)

ggsave(filename = "figure_s1_together.tiff", plot = together_plot, device = "tiff", height = 10, width = 7.08, unit = "in")

