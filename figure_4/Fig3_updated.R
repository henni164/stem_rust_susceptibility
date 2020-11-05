# Figure 3 plotting code
## Eva and Cory
## Started 10/30/18

#load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(reshape2)
library(tidyr)
library(egg)

## Data exploration for figure 3
# Directory for expression matrix files
setwd("/Users/evahenningsen/Documents/MPGI_paper/Figure_3")

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

cols <- c(1:37, 39)
colsb <- c(1:19, 21)
wheat_ready <- wheat_ready_almost[,cols]
brachy_ready <- brachy_ready_almost[,colsb]

brachy_long <- melt(brachy_ready, id.vars = c("geneid", "S_gene"))
brachy_w_met <- separate(data = brachy_long, col = variable, into = c("genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)

wheat_long <- melt(wheat_ready, id.vars = c("geneid", "S_gene"))
wheat_w_met <- separate(data = wheat_long, col = variable, into = c("genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)

all_data <- rbind(brachy_w_met, wheat_w_met)

all_data_rep_means <- all_data %>%
  group_by(geneid, S_gene, genotype, day, treatment) %>%
  summarise(Mean = mean(value)) %>%
  as.data.frame()

#### Table 2
# AGD2_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "AGD2"))
# BI1_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "BI-1"))
# DMR6_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "DMR6"))
# DND1_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "DND1"))
# FAH1_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "FAH1"))
# IBR3_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "IBR3"))
# VAD1_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "VAD1"))
# WRKY25_sub <- as.data.frame(subset(all_data_rep_means, S_gene == "WRKY25"))
# 
# table2_mean <- rbind(AGD2_sub, BI1_sub, DMR6_sub, DND1_sub, FAH1_sub, IBR3_sub, VAD1_sub, WRKY25_sub)
# 
# table2_mean <- unite(table2_mean, day_trt, day, treatment, remove = TRUE)
# 
# table2_mean <- spread(table2_mean, key = day_trt, value = Mean)
# table2_og<- left_join(table2_mean, orthogroup_dat)
# 
# cluster_dat <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Figure_6/clusters_and_sgenes.txt", sep = "\t", header = TRUE)
# colnames(cluster_dat) <- c("genotype", "cluster", "S_gene")
# 
# table2_done <- left_join(table2_og, cluster_dat)
# 
# write.table(table2_done, file = "/Users/evahenningsen/Documents/MPGI_paper/Table_2.txt", sep = "\t", col.names = TRUE, row.names = FALSE)
##########

all_data_quotient <- all_data_rep_means %>%
  group_by(geneid, S_gene, genotype, day) %>%
  summarise(Quotient = (Mean[treatment == "treated"]/Mean[treatment == "mock"])) %>%
  as.data.frame()

all_data_log2fc <- all_data_quotient %>%
  group_by(geneid, S_gene, genotype, day) %>%
  summarise(Log2FC = log2(Quotient)) %>%
  as.data.frame()


all_data_log2fc$genotype <- as.factor(all_data_log2fc$genotype)
levels(all_data_log2fc$genotype)
all_data_log2fc$genotype <- factor(all_data_log2fc$genotype, levels = c("W2691", "Sr9b", "Bd21"))
all_data_log2fc$S_gene <- as.factor(all_data_log2fc$S_gene)

AGD2_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "AGD2"))
BI1_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "BI-1"))
DMR6_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "DMR6"))
DND1_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "DND1"))
FAH1_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "FAH1"))
IBR3_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "IBR3"))
VAD1_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "VAD1"))
WRKY25_sub <- as.data.frame(subset(all_data_log2fc, S_gene == "WRKY25"))


AGD2_plot <- ggplot(AGD2_sub, aes(x= day,
                    y = Log2FC,
                    group = interaction(genotype, geneid),
                    color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = NULL,
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"), name = "Genotype") +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2), labels = c("0", "", "1", "", "2"), limits = c(-.25, 2.5)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain"),
        legend.key = element_blank()) +
  ggtitle(expression(italic("AGD2")))

BI1_plot <- ggplot(BI1_sub, aes(x= day,
                                y = Log2FC,
                                group = interaction(genotype, geneid),
                                color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_y_continuous(breaks = c(-1, 0, 1,2,3,4), labels = c("","0","","2","","4"), limits = c(-1, 4)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle(expression(italic("BI-1")))

DMR6_plot <- ggplot(DMR6_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_y_continuous(breaks = c(0,2.5,5,7.5,10), labels = c("0", "" ,"5", "", "10"), limits = c(-2, 10)) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle(expression(italic("DMR6")))

DND1_plot <- ggplot(DND1_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"), name = "Genotype") +
  scale_y_continuous(breaks = c(-1,0, 1, 2), labels = c("", "0", "1", "2"), limits = c(-1, 2.5)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain"),
        legend.key = element_blank()) +
  ggtitle(expression(italic("DND1")))

FAH1_plot <- ggplot(FAH1_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = "dpi",
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"), name = "Genotype") +
  scale_y_continuous(breaks = c(-2,0,2,4), labels = c("-2", "0", "", "4"), limits = c(-3, 6)) +
  scale_x_discrete(labels = c("2","4","6")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain"),
        legend.key = element_blank()) +
  ggtitle(expression(italic("FAH1")))

IBR3_plot <- ggplot(IBR3_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = "dpi",
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2), labels = c("0", "", "1", "", "2"), limits = c(-.5,2.25)) + 
  scale_x_discrete(labels = c("2","4","6")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle(expression(italic("IBR3")))

VAD1_plot <- ggplot(VAD1_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = "dpi",
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5), labels = c("0", "", "1", "","2" ,""), limits = c(-.5, 3)) + 
  scale_x_discrete(labels = c("2","4","6")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle(expression(italic("VAD1")))

WRKY25_plot <- ggplot(WRKY25_sub, aes(x= day,
                                  y = Log2FC,
                                  group = interaction(genotype, geneid),
                                  color = genotype)) +
  geom_line() +
  geom_point(size = 0) +
  labs(x = "dpi",
       y = NULL) +
  scale_color_manual(values = c("#F8B500", "#FC3C3C", "#00ADB5"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("0", "", "2", "", "4"), limits = c(0,4)) + 
  scale_x_discrete(labels = c("2","4","6")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle(expression(italic("WRKY25")))


mylegend <- get_legend(AGD2_plot + theme(legend.position = "left", text = element_text(color = "black", size = 10), legend.text = element_text(color = "black", size = 10), legend.direction = "horizontal"))
grid_plot2 <- egg::ggarrange(AGD2_plot, BI1_plot, DMR6_plot, DND1_plot, FAH1_plot, IBR3_plot, VAD1_plot, WRKY25_plot, nrow = 2, ncol = 4, heights = c(1, 1), widths = c(1, 1, 1, 1))
final_plot <- grid.arrange(grid_plot2, mylegend, nrow = 2, ncol = 1, heights = c(1,0.25))
ggsave(filename = "final_combined_plot.tiff", plot = final_plot, device = "tiff", height = 100, width = 120, unit = "mm")

ggsave(filename = "altbigplot_nolegend.pdf", plot = grid_plot2, device = "pdf", height = 80, width = 120, unit = "mm")
ggsave(filename = "figure_legend.pdf", plot = legend.plot, device = "pdf", height = 20, width = 100, unit = "mm")
