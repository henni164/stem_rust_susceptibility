# Figure 3 plotting code
## Eva and Cory
## Started 10/30/18

#load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(cowplot)

## Data exploration for figure 3
# Directory for expression matrix files
setwd("~/Documents/GitHub/cory/MPGI/figure3")

#Import filtered wheat and brachy expression matrices
d_wheat_1 <- read.csv("updated_fig3_wheat_filtered6.csv", header = TRUE, sep = ",", row.names = 1)
d_bd21_1 <- read.csv("Fig3_genes_brachy_filtered6.csv", header = TRUE, sep = ",", row.names = 1)

# Want to be able to do calculations on 0 values. Will add 0.001 to each value
d_wheat_scaled <- d_wheat_1[,3:38] + 0.001
d_wheat <- cbind(d_wheat_1[,1:2], d_wheat_scaled)

d_bd21_scaled <- d_bd21_1[,3:20] + 0.001
d_bd21 <- cbind(d_bd21_1[,1:2], d_bd21_scaled)

# Means for 3 columns will average 3 reps for each genotype, treatment, day
# for wheat
w2691_mean_mock_day2 <- as.matrix(rowMeans(d_wheat[,3:5]))
w2691_mean_mock_day4 <- as.matrix(rowMeans(d_wheat[,6:8]))
w2691_mean_mock_day6 <- as.matrix(rowMeans(d_wheat[,9:11]))
sr9b_mean_mock_day2 <- as.matrix(rowMeans(d_wheat[,12:14]))
sr9b_mean_mock_day4 <- as.matrix(rowMeans(d_wheat[,15:17]))
sr9b_mean_mock_day6 <- as.matrix(rowMeans(d_wheat[,18:20]))
w2691_mean_inoc_day2 <- as.matrix(rowMeans(d_wheat[,21:23]))
w2691_mean_inoc_day4 <- as.matrix(rowMeans(d_wheat[,24:26]))
w2691_mean_inoc_day6 <- as.matrix(rowMeans(d_wheat[,27:29]))
sr9b_mean_inoc_day2 <- as.matrix(rowMeans(d_wheat[,30:32]))
sr9b_mean_inoc_day4 <- as.matrix(rowMeans(d_wheat[,33:35]))
sr9b_mean_inoc_day6 <- as.matrix(rowMeans(d_wheat[,36:38]))

# for brachy
bd_mean_mock_day2 <- as.matrix(rowMeans(d_bd21[,3:5]))
bd_mean_mock_day4 <- as.matrix(rowMeans(d_bd21[,6:8]))
bd_mean_mock_day6 <- as.matrix(rowMeans(d_bd21[,9:11]))
bd_mean_inoc_day2 <- as.matrix(rowMeans(d_bd21[,12:14]))
bd_mean_inoc_day4 <- as.matrix(rowMeans(d_bd21[,15:17]))
bd_mean_inoc_day6 <- as.matrix(rowMeans(d_bd21[,18:20]))

# add metadata as column to each row of means
# For Wheat
w2691 <- as.matrix(rep("W2691", nrow(w2691_mean_mock_day2)))
sr9b <- as.matrix(rep("W2691 + Sr9b", nrow(w2691_mean_mock_day2)))
w_mock <- as.matrix(rep("Mock", nrow(w2691_mean_mock_day2)))
w_inoc <- as.matrix(rep("Inoculated", nrow(w2691_mean_mock_day2)))
w_day2 <- as.matrix(rep(2, nrow(w2691_mean_mock_day2)))
w_day4 <- as.matrix(rep(4, nrow(w2691_mean_mock_day2)))
w_day6 <- as.matrix(rep(6, nrow(w2691_mean_mock_day2)))
w_geneid <- as.matrix(d_wheat[,1:2])

# For brachypodium
bd21 <- as.matrix(rep("Bd21-3", nrow(bd_mean_mock_day2)))
bd_mock <- as.matrix(rep("Mock", nrow(bd_mean_mock_day2)))
bd_inoc <- as.matrix(rep("Inoculated", nrow(bd_mean_mock_day2)))
bd_day2 <- as.matrix(rep(2, nrow(bd_mean_mock_day2)))
bd_day4 <- as.matrix(rep(4, nrow(bd_mean_mock_day2)))
bd_day6 <- as.matrix(rep(6, nrow(bd_mean_mock_day2)))
bd_geneid <- as.matrix(d_bd21[,1:2])

# Make full matrix for the different genotypes, days, and treatments
# For Wheat
full_w2691_mean_mock_day2 <- cbind(w2691_mean_mock_day2, w2691, w_mock, w_day2, w_geneid)
full_w2691_mean_mock_day4 <- cbind(w2691_mean_mock_day4, w2691, w_mock, w_day4, w_geneid)
full_w2691_mean_mock_day6 <- cbind(w2691_mean_mock_day6, w2691, w_mock, w_day6, w_geneid)
full_w2691_mean_inoc_day2 <- cbind(w2691_mean_inoc_day2, w2691, w_inoc, w_day2, w_geneid)
full_w2691_mean_inoc_day4 <- cbind(w2691_mean_inoc_day4, w2691, w_inoc, w_day4, w_geneid)
full_w2691_mean_inoc_day6 <- cbind(w2691_mean_inoc_day6, w2691, w_inoc, w_day6, w_geneid)

full_sr9b_mean_mock_day2 <- cbind(sr9b_mean_mock_day2, sr9b, w_mock, w_day2, w_geneid)
full_sr9b_mean_mock_day4 <- cbind(sr9b_mean_mock_day4, sr9b, w_mock, w_day4, w_geneid)
full_sr9b_mean_mock_day6 <- cbind(sr9b_mean_mock_day6, sr9b, w_mock, w_day6, w_geneid)
full_sr9b_mean_inoc_day2 <- cbind(sr9b_mean_inoc_day2, sr9b, w_inoc, w_day2, w_geneid)
full_sr9b_mean_inoc_day4 <- cbind(sr9b_mean_inoc_day4, sr9b, w_inoc, w_day4, w_geneid)
full_sr9b_mean_inoc_day6 <- cbind(sr9b_mean_inoc_day6, sr9b, w_inoc, w_day6, w_geneid)

# For brachypodium
full_bd_mean_mock_day2 <- cbind(bd_mean_mock_day2, bd21, bd_mock, bd_day2, bd_geneid)
full_bd_mean_mock_day4 <- cbind(bd_mean_mock_day4, bd21, bd_mock, bd_day4, bd_geneid)
full_bd_mean_mock_day6 <- cbind(bd_mean_mock_day6, bd21, bd_mock, bd_day6, bd_geneid)
full_bd_mean_inoc_day2 <- cbind(bd_mean_inoc_day2, bd21, bd_inoc, bd_day2, bd_geneid)
full_bd_mean_inoc_day4 <- cbind(bd_mean_inoc_day4, bd21, bd_inoc, bd_day4, bd_geneid)
full_bd_mean_inoc_day6 <- cbind(bd_mean_inoc_day6, bd21, bd_inoc, bd_day6, bd_geneid)

# Name the columns of the "first" matrix
colnames(full_w2691_mean_mock_day2) <- c("Mean", "Genotype", "Treatment", "Day", "GeneID", "GeneName")

# Make large matrix containing all data

all_data <- rbind(full_w2691_mean_mock_day2, full_w2691_mean_mock_day4, full_w2691_mean_mock_day6, full_w2691_mean_inoc_day2, full_w2691_mean_inoc_day4, full_w2691_mean_inoc_day6, full_sr9b_mean_mock_day2, full_sr9b_mean_mock_day4, full_sr9b_mean_mock_day6, full_sr9b_mean_inoc_day2, full_sr9b_mean_inoc_day4, full_sr9b_mean_inoc_day6, full_bd_mean_mock_day2, full_bd_mean_mock_day4, full_bd_mean_mock_day6, full_bd_mean_inoc_day2, full_bd_mean_inoc_day4, full_bd_mean_inoc_day6)

all_data <- as.data.frame(all_data)
attach(all_data)
all_data[,'Mean'] <- as.numeric(as.character(all_data[,'Mean']))
all_data[,'Day'] <- as.numeric(as.character(all_data[,'Day']))

#Dplyr can do calculations in subsets.
#Below is our "all_data" spreadsheet which has been grouped by GeneID, Day, and Genotype.
#It is then "summarised" within these groupings in the way of Mean(when treatment == inoculated) divided by mean (when treatment == mock)

all_data_quotient <- all_data %>%
  group_by(GeneID, GeneName, Genotype, Day) %>%
  summarise(Quotient = (Mean[Treatment == "Inoculated"]/ Mean[Treatment == "Mock"])) %>%
  as.data.frame()
attach(all_data_quotient)

## Calculating the log2FC on the new data.frame and attaching it to create a final data.frame

logfc <- log2(Quotient)
all_data_log2fc <- cbind(all_data_quotient, logfc)
colnames(all_data_log2fc) <- c("GeneID", "GeneName", "Genotype", "Day", "Quotient", "Log2Fc")
as.data.frame(all_data_log2fc)
all_data_log2fc$GeneID <- as.character(all_data_log2fc$GeneID)
all_data_log2fc$GeneName <- as.character(all_data_log2fc$GeneName)
all_data_log2fc$Genotype <- as.character(all_data_log2fc$Genotype)
attach(all_data_log2fc)

## subset GeneName 
VAD1_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "VAD1"))
attach(VAD1_sub)
PMR4_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "PMR4"))
attach(PMR4_sub)
DND1_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "DND1"))
attach(DND1_sub)
DMR6_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "DMR6"))
attach(DMR6_sub)
BI1_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "BI-1"))
attach(BI1_sub)
ADH_sub <- as.data.frame(subset(all_data_log2fc, GeneName == "ADH"))
attach(ADH_sub)



#ADH
ADH_plot <- ggplot(ADH_sub, aes(x= Day,
                    y = Log2Fc,
                    group = interaction(Genotype, GeneID),
                    color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = NULL,
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_color_manual(values = c("#00ADB5", "#F8B500", "#FC3C3C"), breaks = c("W2691", "W2691 + Sr9b", "Bd21-3"), labels = c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")) +
  scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2), labels = c(" ", 0, " ", 1, " ", 2)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        legend.position = "none",
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle("ADH")

#BI1
BI1_plot <- ggplot(BI1_sub, aes(x= Day,
                    y = Log2Fc,
                    group = interaction(Genotype, GeneID),
                    color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(guide = FALSE, values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
  scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), labels = c(" ", 0, " ", " ", " ", 2, " ", " ", " ", 4)) +
  theme(panel.background = element_blank(),
                axis.line = element_line(color = "black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
                plot.title = element_text(size = 10, face = "plain")) +
  ggtitle("BI-1")

#DMR6
DMR6_plot <- ggplot(DMR6_sub, aes(x= Day,
                                  y = Log2Fc,
                                  group = interaction(Genotype, GeneID),
                                  color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual(guide = FALSE, values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
  scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7,8), labels = c(" ", " ", 0, " ", " ", 3, " ", " ", 6, "  ", " ")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle("DMR6")

#DND1
DND1_plot <- ggplot(DND1_sub, aes(x= Day,
                                  y = Log2Fc,
                                  group = interaction(Genotype, GeneID),
                                  color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = "dpi",
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_color_manual(guide = FALSE, values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
  scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2), labels = c(" ", 0, " ", 1, " ", 2)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle("DND1")

#VAD1
VAD1_plot <- ggplot(VAD1_sub, aes(x= Day,
                                  y = Log2Fc,
                                  group = interaction(Genotype, GeneID),
                                  color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = "dpi",
       y = NULL, size = 10) +
  scale_color_manual(guide = FALSE, values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
  scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5), labels = c(" ", 0, " ", 1, " ", 2, " ")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        plot.title=element_text(size = 10, face = "plain")) +
  ggtitle("VAD1")

#PMR4
PMR4_plot <- ggplot(PMR4_sub, aes(x= Day,
                                  y = Log2Fc,
                                  group = interaction(Genotype, GeneID),
                                  color = Genotype)) +
  geom_line() +
  geom_point(size = 0.75) +
  labs(x = "dpi",
       y = NULL, size = 10) +
    scale_color_manual(guide = FALSE, values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
    scale_x_continuous(breaks = c(2,4,6)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2), labels = c(" ", 0, " ", 1, " ", 2)) +
  ggtitle("PMR4") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        plot.title=element_text(size = 10, face = "plain"))

#Prining individual plots

#ggsave(filename = "ADHplot", plot = ADH_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")
#ggsave(filename = "BI1plot", plot = BI1_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")
#ggsave(filename = "DMR6plot", plot = DMR6_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")
#ggsave(filename = "DND1plot", plot = DND1_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")
#ggsave(filename = "PMR4plot", plot = PMR4_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")
#ggsave(filename = "VAD1plot", plot = VAD1_plot, device = "pdf", height = 2.5, width = 2.5, units = "in")

#Printing whole plot

mylegend <- get_legend(ADH_plot + theme(legend.position = "bottom", text = element_text(color = "black", size = 10), legend.text = element_text(color = "black", size = 10)))
legend.plot <- as_ggplot(mylegend)
grid_plot2 <- ggarrange(ADH_plot, BI1_plot, DMR6_plot, DND1_plot, PMR4_plot, VAD1_plot, nrow = 2, ncol = 3, heights = c(1, 1.1), widths = c(1.1, 1, 1))
grid_plot2_1 <- ggarrange(grid_plot2, legend.plot, ncol = 1, nrow = 2, heights = c(1, 0.2), widths = c(1.1, 0.4))
ggsave(filename = "altbigplot_nolegend.pdf", plot = grid_plot2, device = "pdf", height = 80, width = 85, unit = "mm")
#ggsave(filename = "altbigplot.pdf", plot = grid_plot2_1, device = "pdf", height = 100, width = 85, unit = "mm")
ggsave(filename = "figure_legend.pdf", plot = legend.plot, device = "pdf", height = 20, width = 100, unit = "mm")
