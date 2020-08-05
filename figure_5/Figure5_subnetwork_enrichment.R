# Figure 4 network enrichment exploration
## Eva

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(DescTools)
library(grid)
library(gtable)
library(ggpubr)

# Change working directory to appropriate location
setwd("~/Documents/GitHub/cory/MPGI/figure4/Genelist_creation/Go_enrichment_tests/Enrichment_R_output/Parsed_output")

# DMR6
BDIBD213.1G1026800 <- read.delim("BDIBD213.1G1026800_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(BDIBD213.1G1026800)
# Turn Gene Ratio column to fraction
BDIBD213.1G1026800$Gene_Ratio <- as.character(BDIBD213.1G1026800$Gene_Ratio)
BDIBD213.1G1026800$Gene_Ratio <- sapply(BDIBD213.1G1026800$Gene_Ratio, function(x) eval(parse(text=x)))
BDIBD213.1G1026800_names <- as.matrix(rep("BDIBD213.1G1026800", nrow(BDIBD213.1G1026800)))
BDIBD213.1G1026800_plusnames <- cbind(BDIBD213.1G1026800_names, BDIBD213.1G1026800)
colnames(BDIBD213.1G1026800_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

Sr9b_TRAESCS4B01G346900 <- read.delim("Sr9b_TRAESCS4B01G346900_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS4B01G346900)
Sr9b_TRAESCS4B01G346900$Gene_Ratio <- as.character(Sr9b_TRAESCS4B01G346900$Gene_Ratio)
Sr9b_TRAESCS4B01G346900$Gene_Ratio <- sapply(Sr9b_TRAESCS4B01G346900$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS4B01G346900_names <- as.matrix(rep("Sr9b_TRAESCS4B01G346900", nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900_plusnames <- cbind(Sr9b_TRAESCS4B01G346900_names, Sr9b_TRAESCS4B01G346900)
colnames(Sr9b_TRAESCS4B01G346900_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

Sr9b_TRAESCS4D01G341800 <- read.delim("Sr9b_TRAESCS4D01G341800_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS4D01G341800)
Sr9b_TRAESCS4D01G341800$Gene_Ratio <- as.character(Sr9b_TRAESCS4D01G341800$Gene_Ratio)
Sr9b_TRAESCS4D01G341800$Gene_Ratio <- sapply(Sr9b_TRAESCS4D01G341800$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS4D01G341800_names <- as.matrix(rep("Sr9b_TRAESCS4D01G341800", nrow(Sr9b_TRAESCS4D01G341800)))
Sr9b_TRAESCS4D01G341800_plusnames <- cbind(Sr9b_TRAESCS4D01G341800_names, Sr9b_TRAESCS4D01G341800)
colnames(Sr9b_TRAESCS4D01G341800_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

Sr9b_TRAESCS5A01G515600 <- read.delim("Sr9b_TRAESCS5A01G515600_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS5A01G515600)
Sr9b_TRAESCS5A01G515600$Gene_Ratio <- as.character(Sr9b_TRAESCS5A01G515600$Gene_Ratio)
Sr9b_TRAESCS5A01G515600$Gene_Ratio <- sapply(Sr9b_TRAESCS5A01G515600$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS5A01G515600_names <- as.matrix(rep("Sr9b_TRAESCS5A01G515600", nrow(Sr9b_TRAESCS5A01G515600)))
Sr9b_TRAESCS5A01G515600_plusnames <- cbind(Sr9b_TRAESCS5A01G515600_names, Sr9b_TRAESCS5A01G515600)
colnames(Sr9b_TRAESCS5A01G515600_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

W2691_TRAESCS4B01G346900 <- read.delim("W2691_TRAESCS4B01G346900_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS4B01G346900)
W2691_TRAESCS4B01G346900$Gene_Ratio <- as.character(W2691_TRAESCS4B01G346900$Gene_Ratio)
W2691_TRAESCS4B01G346900$Gene_Ratio <- sapply(W2691_TRAESCS4B01G346900$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS4B01G346900_names <- as.matrix(rep("W2691_TRAESCS4B01G346900", nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900_plusnames <- cbind(W2691_TRAESCS4B01G346900_names, W2691_TRAESCS4B01G346900)
colnames(W2691_TRAESCS4B01G346900_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

W2691_TRAESCS4D01G341800 <- read.delim("W2691_TRAESCS4D01G341800_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS4D01G341800)
W2691_TRAESCS4D01G341800$Gene_Ratio <- as.character(W2691_TRAESCS4D01G341800$Gene_Ratio)
W2691_TRAESCS4D01G341800$Gene_Ratio <- sapply(W2691_TRAESCS4D01G341800$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS4D01G341800_names <- as.matrix(rep("W2691_TRAESCS4D01G341800", nrow(W2691_TRAESCS4D01G341800)))
W2691_TRAESCS4D01G341800_plusnames <- cbind(W2691_TRAESCS4D01G341800_names, W2691_TRAESCS4D01G341800)
colnames(W2691_TRAESCS4D01G341800_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

W2691_TRAESCS5A01G515600 <- read.delim("W2691_TRAESCS5A01G515600_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS5A01G515600)
W2691_TRAESCS5A01G515600$Gene_Ratio <- as.character(W2691_TRAESCS5A01G515600$Gene_Ratio)
W2691_TRAESCS5A01G515600$Gene_Ratio <- sapply(W2691_TRAESCS5A01G515600$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS5A01G515600_names <- as.matrix(rep("W2691_TRAESCS5A01G515600", nrow(W2691_TRAESCS5A01G515600)))
W2691_TRAESCS5A01G515600_plusnames <- cbind(W2691_TRAESCS5A01G515600_names, W2691_TRAESCS5A01G515600)
colnames(W2691_TRAESCS5A01G515600_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

DMR6_all <- rbind(BDIBD213.1G1026800_plusnames, Sr9b_TRAESCS4B01G346900_plusnames, Sr9b_TRAESCS4D01G341800_plusnames, Sr9b_TRAESCS5A01G515600_plusnames, 
                  W2691_TRAESCS4B01G346900_plusnames, W2691_TRAESCS4D01G341800_plusnames, W2691_TRAESCS5A01G515600_plusnames)
DMR6_names <- as.matrix(rep("DMR6", nrow(DMR6_all)))
Genotype_DMR6 <- c("Bd-21", "Bd-21", rep("W2691 + Sr9b", 20), rep("W2691",6))
DMR6_combo <- cbind(DMR6_names, Genotype_DMR6, DMR6_all)
colnames(DMR6_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(DMR6_combo)

##Plotting DMR6
# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
              )
)

# ggplot(DMR6_all, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("DMR6")

# PMR4
BDIBD213.2G0590100 <- read.delim("BDIBD213.2G0590100_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(BDIBD213.2G0590100)
# Turn Gene Ratio column to fraction
BDIBD213.2G0590100$Gene_Ratio <- as.character(BDIBD213.2G0590100$Gene_Ratio)
BDIBD213.2G0590100$Gene_Ratio <- sapply(BDIBD213.2G0590100$Gene_Ratio, function(x) eval(parse(text=x)))
BDIBD213.2G0590100_names <- as.matrix(rep("BDIBD213.2G0590100", nrow(BDIBD213.2G0590100)))
BDIBD213.2G0590100_plusnames <- cbind(BDIBD213.2G0590100_names, BDIBD213.2G0590100)
colnames(BDIBD213.2G0590100_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


BDIBD213.2G0641600 <- read.delim("BDIBD213.2G0641600_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(BDIBD213.2G0641600)
BDIBD213.2G0641600$Gene_Ratio <- as.character(BDIBD213.2G0641600$Gene_Ratio)
BDIBD213.2G0641600$Gene_Ratio <- sapply(BDIBD213.2G0641600$Gene_Ratio, function(x) eval(parse(text=x)))
BDIBD213.2G0641600_names <- as.matrix(rep("BDIBD213.2G0641600", nrow(BDIBD213.2G0641600)))
BDIBD213.2G0641600_plusnames <- cbind(BDIBD213.2G0641600_names, BDIBD213.2G0641600)
colnames(BDIBD213.2G0641600_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

Sr9b_TRAESCS3D01G246000 <- read.delim("Sr9b_TRAESCS3D01G246000_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS3D01G246000)
Sr9b_TRAESCS3D01G246000$Gene_Ratio <- as.character(Sr9b_TRAESCS3D01G246000$Gene_Ratio)
Sr9b_TRAESCS3D01G246000$Gene_Ratio <- sapply(Sr9b_TRAESCS3D01G246000$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS3D01G246000_names <- as.matrix(rep("Sr9b_TRAESCS3D01G246000", nrow(Sr9b_TRAESCS3D01G246000)))
Sr9b_TRAESCS3D01G246000_plusnames <- cbind(Sr9b_TRAESCS3D01G246000_names, Sr9b_TRAESCS3D01G246000)
colnames(Sr9b_TRAESCS3D01G246000_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

PMR4_all <- rbind(BDIBD213.2G0641600_plusnames, BDIBD213.2G0590100_plusnames, Sr9b_TRAESCS3D01G246000_plusnames)
PMR4_names <- as.matrix(rep("PMR4", nrow(PMR4_all)))
Genotype_PMR4 <- c(rep("Bd-21", 13), "W2691 + Sr9b")
PMR4_combo <- cbind(PMR4_names, Genotype_PMR4, PMR4_all)
colnames(PMR4_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(PMR4_combo)

##Plotting PMR4

# ggplot(PMR4_all, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("PMR4")

# VAD1
# Read in the data
Sr9b_TRAESCS2A01G231900 <- read.delim("Sr9b_TRAESCS2A01G231900_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS2A01G231900)
# Turn Gene Ratio column to fraction
Sr9b_TRAESCS2A01G231900$Gene_Ratio <- as.character(Sr9b_TRAESCS2A01G231900$Gene_Ratio)
Sr9b_TRAESCS2A01G231900$Gene_Ratio <- sapply(Sr9b_TRAESCS2A01G231900$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS2A01G231900_names <- as.matrix(rep("Sr9b_TRAESCS2A01G231900", nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900_plusnames <- cbind(Sr9b_TRAESCS2A01G231900_names, Sr9b_TRAESCS2A01G231900)
colnames(Sr9b_TRAESCS2A01G231900_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


Sr9b_TRAESCS2B01G253500 <- read.delim("Sr9b_TRAESCS2B01G253500_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS2B01G253500)
Sr9b_TRAESCS2B01G253500$Gene_Ratio <- as.character(Sr9b_TRAESCS2B01G253500$Gene_Ratio)
Sr9b_TRAESCS2B01G253500$Gene_Ratio <- sapply(Sr9b_TRAESCS2B01G253500$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS2B01G253500_names <- as.matrix(rep("Sr9b_TRAESCS2B01G253500", nrow(Sr9b_TRAESCS2B01G253500)))
Sr9b_TRAESCS2B01G253500_plusnames <- cbind(Sr9b_TRAESCS2B01G253500_names, Sr9b_TRAESCS2B01G253500)
colnames(Sr9b_TRAESCS2B01G253500_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


Sr9b_TRAESCS2D01G236800 <- read.delim("Sr9b_TRAESCS2D01G236800_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS2D01G236800)
Sr9b_TRAESCS2D01G236800$Gene_Ratio <- as.character(Sr9b_TRAESCS2D01G236800$Gene_Ratio)
Sr9b_TRAESCS2D01G236800$Gene_Ratio <- sapply(Sr9b_TRAESCS2D01G236800$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS2D01G236800_names <- as.matrix(rep("Sr9b_TRAESCS2D01G236800", nrow(Sr9b_TRAESCS2D01G236800)))
Sr9b_TRAESCS2D01G236800_plusnames <- cbind(Sr9b_TRAESCS2D01G236800_names, Sr9b_TRAESCS2D01G236800)
colnames(Sr9b_TRAESCS2D01G236800_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


W2691_TRAESCS2A01G231900 <- read.delim("W2691_TRAESCS2A01G231900_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS2A01G231900)
W2691_TRAESCS2A01G231900$Gene_Ratio <- as.character(W2691_TRAESCS2A01G231900$Gene_Ratio)
W2691_TRAESCS2A01G231900$Gene_Ratio <- sapply(W2691_TRAESCS2A01G231900$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS2A01G231900_names <- as.matrix(rep("W2691_TRAESCS2A01G231900", nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900_plusnames <- cbind(W2691_TRAESCS2A01G231900_names, W2691_TRAESCS2A01G231900)
colnames(W2691_TRAESCS2A01G231900_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


W2691_TRAESCS2B01G253500 <- read.delim("W2691_TRAESCS2B01G253500_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS2B01G253500)
W2691_TRAESCS2B01G253500$Gene_Ratio <- as.character(W2691_TRAESCS2B01G253500$Gene_Ratio)
W2691_TRAESCS2B01G253500$Gene_Ratio <- sapply(W2691_TRAESCS2B01G253500$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS2B01G253500_names <- as.matrix(rep("W2691_TRAESCS2B01G253500", nrow(W2691_TRAESCS2B01G253500)))
W2691_TRAESCS2B01G253500_plusnames <- cbind(W2691_TRAESCS2B01G253500_names, W2691_TRAESCS2B01G253500)
colnames(W2691_TRAESCS2B01G253500_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")


W2691_TRAESCS2D01G236800 <- read.delim("W2691_TRAESCS2D01G236800_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS2D01G236800)
W2691_TRAESCS2D01G236800$Gene_Ratio <- as.character(W2691_TRAESCS2D01G236800$Gene_Ratio)
W2691_TRAESCS2D01G236800$Gene_Ratio <- sapply(W2691_TRAESCS2D01G236800$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS2D01G236800_names <- as.matrix(rep("W2691_TRAESCS2D01G236800", nrow(W2691_TRAESCS2D01G236800)))
W2691_TRAESCS2D01G236800_plusnames <- cbind(W2691_TRAESCS2D01G236800_names, W2691_TRAESCS2D01G236800)
colnames(W2691_TRAESCS2D01G236800_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

VAD1_all <- rbind(Sr9b_TRAESCS2A01G231900_plusnames, Sr9b_TRAESCS2B01G253500_plusnames, Sr9b_TRAESCS2D01G236800_plusnames,
                  W2691_TRAESCS2A01G231900_plusnames, W2691_TRAESCS2B01G253500_plusnames, W2691_TRAESCS2D01G236800_plusnames)
VAD1_names <- as.matrix(rep("VAD1", nrow(VAD1_all)))
Genotype_VAD1 <- c(rep("W2691 + Sr9b", 10), rep("W2691",11))
VAD1_combo <- cbind(VAD1_names, Genotype_VAD1, VAD1_all)
colnames(VAD1_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(VAD1_combo)

##Plotting VAD1

# ggplot(VAD1_all, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("VAD1")


## ADH
Sr9b_TRAESCS4B01G106300 <- read.delim("Sr9b_TRAESCS4B01G106300_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS4B01G106300)
# Turn Gene Ratio column to fraction
Sr9b_TRAESCS4B01G106300$Gene_Ratio <- as.character(Sr9b_TRAESCS4B01G106300$Gene_Ratio)
Sr9b_TRAESCS4B01G106300$Gene_Ratio <- sapply(Sr9b_TRAESCS4B01G106300$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS4B01G106300_names <- as.matrix(rep("Sr9b_TRAESCS4B01G106300", nrow(Sr9b_TRAESCS4B01G106300)))
Sr9b_TRAESCS4B01G106300_plusnames <- cbind(Sr9b_TRAESCS4B01G106300_names, Sr9b_TRAESCS4B01G106300)
colnames(Sr9b_TRAESCS4B01G106300_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
ADH_all <- Sr9b_TRAESCS4B01G106300_plusnames
ADH_names <- as.matrix(rep("ADH", nrow(ADH_all)))
Genotype_ADH <- c("W2691 + Sr9b")
ADH_combo <- cbind(ADH_names, Genotype_ADH, ADH_all)
colnames(ADH_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(ADH_combo)
##Plotting ADH

# ggplot(Sr9b_TRAESCS4B01G106300_plusnames, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("ADH")


## BI-1
Sr9b_TRAESCS6D01G077000 <- read.delim("Sr9b_TRAESCS6D01G077000_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(Sr9b_TRAESCS6D01G077000)
Sr9b_TRAESCS6D01G077000$Gene_Ratio <- as.character(Sr9b_TRAESCS6D01G077000$Gene_Ratio)
Sr9b_TRAESCS6D01G077000$Gene_Ratio <- sapply(Sr9b_TRAESCS6D01G077000$Gene_Ratio, function(x) eval(parse(text=x)))
Sr9b_TRAESCS6D01G077000_names <- as.matrix(rep("Sr9b_TRAESCS6D01G077000", nrow(Sr9b_TRAESCS6D01G077000)))
Sr9b_TRAESCS6D01G077000_plusnames <- cbind(Sr9b_TRAESCS6D01G077000_names, Sr9b_TRAESCS6D01G077000)
colnames(Sr9b_TRAESCS6D01G077000_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

W2691_TRAESCS6A01G083200 <- read.delim("W2691_TRAESCS6A01G083200_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS6A01G083200)
W2691_TRAESCS6A01G083200$Gene_Ratio <- as.character(W2691_TRAESCS6A01G083200$Gene_Ratio)
W2691_TRAESCS6A01G083200$Gene_Ratio <- sapply(W2691_TRAESCS6A01G083200$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS6A01G083200_names <- as.matrix(rep("W2691_TRAESCS6A01G083200", nrow(W2691_TRAESCS6A01G083200)))
W2691_TRAESCS6A01G083200_plusnames <- cbind(W2691_TRAESCS6A01G083200_names, W2691_TRAESCS6A01G083200)
colnames(W2691_TRAESCS6A01G083200_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

W2691_TRAESCS6B01G113900 <- read.delim("W2691_TRAESCS6B01G113900_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS6B01G113900)
W2691_TRAESCS6B01G113900$Gene_Ratio <- as.character(W2691_TRAESCS6B01G113900$Gene_Ratio)
W2691_TRAESCS6B01G113900$Gene_Ratio <- sapply(W2691_TRAESCS6B01G113900$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS6B01G113900_names <- as.matrix(rep("W2691_TRAESCS6B01G113900", nrow(W2691_TRAESCS6B01G113900)))
W2691_TRAESCS6B01G113900_plusnames <- cbind(W2691_TRAESCS6B01G113900_names, W2691_TRAESCS6B01G113900)
colnames(W2691_TRAESCS6B01G113900_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")

BI1_all <- rbind(Sr9b_TRAESCS6D01G077000_plusnames, W2691_TRAESCS6A01G083200_plusnames, W2691_TRAESCS6B01G113900_plusnames)
BI1_names <- as.matrix(rep("BI1", nrow(BI1_all)))
Genotype_BI1 <- c("W2691 + Sr9b", rep("W2691", 4))
BI1_combo <- cbind(BI1_names, Genotype_BI1, BI1_all)
colnames(BI1_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(BI1_combo)

##Plotting BI-1
# ggplot(BI1_all, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("BI-1")


# DND1
W2691_TRAESCS5D01G404600 <- read.delim("W2691_TRAESCS5D01G404600_goslim_enrichment_parsed.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
attach(W2691_TRAESCS5D01G404600)
# Turn Gene Ratio column to fraction
W2691_TRAESCS5D01G404600$Gene_Ratio <- as.character(W2691_TRAESCS5D01G404600$Gene_Ratio)
W2691_TRAESCS5D01G404600$Gene_Ratio <- sapply(W2691_TRAESCS5D01G404600$Gene_Ratio, function(x) eval(parse(text=x)))
W2691_TRAESCS5D01G404600_names <- as.matrix(rep("W2691_TRAESCS5D01G404600", nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600_plusnames <- cbind(W2691_TRAESCS5D01G404600_names, W2691_TRAESCS5D01G404600)
colnames(W2691_TRAESCS5D01G404600_plusnames) <- c("Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
DND1_all <- W2691_TRAESCS5D01G404600_plusnames
DND1_names <- as.matrix(rep("DND1", nrow(DND1_all)))
Genotype_DND1 <- c(rep("W2691", 3))
DND1_combo <- cbind(DND1_names, Genotype_DND1, DND1_all)
colnames(DND1_combo) <- c("Shortname", "Genotype", "Gene", "Description", "Adj_pvalue", "Gene_Ratio", "Domain")
attach(DND1_combo)
##Plotting DND1

# ggplot(W2691_TRAESCS5D01G404600_plusnames, aes(x = 1, y=Description, fill=Gene_Ratio)) +
#     geom_tile(inherit.aes = TRUE, width=1, height=1) +
#     scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
#     facet_grid(Domain ~ Gene, scales = "free_y", space = "free_y", labeller=label_parsed) +
#     theme(
#         axis.text.x = element_text(size = 8, color="black"),
#         axis.title = element_text(size = 8),
#         axis.text.y = element_text(size=6, color="black"),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         panel.grid.major = element_line(color = "gray"),
#         panel.grid.minor = element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         strip.background = element_rect(fill = "transparent", color = "white", size = 1),
#         #strip.text = element_text(face = "bold", size = 10, color = "black"),
#         strip.text = element_text(size = 8, color = "black"),
#         legend.position = "bottom",
#         legend.justification = "top",
#         legend.box = "horizontal",
#         #legend.box.background = element_rect(colour = "black"),
#         #legend.background = element_blank(),
#         panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.background = element_rect(fill = "transparent"),
#         #legend.box.background = element_rect(fill = "transparent"),
#         legend.box.background=element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=8),
#         strip.text.x=element_text(size=8)) +
#     ggtitle("DND1")

## Making giant figure

ALL_GENES <- rbind(ADH_combo, BI1_combo, DMR6_combo, DND1_combo, PMR4_combo, VAD1_combo)
attach(ALL_GENES)
ALL_GENES2 <- ALL_GENES[order(ALL_GENES$Description, decreasing = TRUE),]
ALL_GENES2$Description <- as.factor(ALL_GENES2$Description)
ALL_GENES2 <- as.data.frame(ALL_GENES2)
levels(ALL_GENES2$Genotype) = c("W2691"=expression(paste("W2691")), "W2691 + Sr9b"=expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"=expression(paste("Bd21-3")))
levels(ALL_GENES2$Shortname) <- c("ADH" = expression(paste(bold("ADH"))), "BI1" = expression(paste(bold("BI1"))), "DMR6" = expression(paste(bold("DMR6"))), "DND1" = expression(paste(bold("DND1"))), "PMR4" = expression(paste(bold("PMR4"))), "VAD1" = expression(paste(bold("VAD1"))))
levels(ALL_GENES2$Description)[4] <- gsub("cellular component organization or biogenesis", "cellular component \n organization \n or biogenesis", levels(ALL_GENES2$Description)[4])
levels(ALL_GENES2$Description)[25] <- gsub("response to endogenous stimulus", "response to \n endogenous stimulus", levels(ALL_GENES2$Description)[25])
levels(ALL_GENES2$Description)[8] <- gsub("DNA-binding transcription factor activity", "DNA-binding \n transcription \n factor activity", levels(ALL_GENES2$Description)[8])
levels(ALL_GENES2$Description)[3] <- gsub("cellular component organization", "cellular component \n organization", levels(ALL_GENES2$Description)[3])
levels(ALL_GENES2$Description)[24] <- gsub("response to abiotic stimulus", "response to \n abiotic stimulus", levels(ALL_GENES2$Description)[24])
levels(ALL_GENES2$Description)[12] <- gsub("intracellular organelle lumen", "intracellular \n organelle lumen", levels(ALL_GENES2$Description)[12])
levels(ALL_GENES2$Description)[15] <- gsub("membrane-enclosed lumen", "membrane-enclosed \n lumen", levels(ALL_GENES2$Description)[15])
levels(ALL_GENES2$Description)[13] <- gsub("intracellular organelle part", "intracellular \n organelle part", levels(ALL_GENES2$Description)[13])
x_axis <- " "

ALL_plot <- ggplot(ALL_GENES2, aes(x = x_axis, y=reorder(Description, desc(Description)), fill=Gene_Ratio)) +
    geom_tile(inherit.aes = TRUE, width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") +
    facet_grid(rows = vars(Domain), cols = vars(Shortname, Genotype), scales = "free", space = "free", labeller=label_parsed) +
    labs(x = NULL, y = NULL) +
    theme(
        axis.text.x = element_text(size = 8, color="black", angle = 90),
        axis.title = element_text(size = 8),
        axis.text.y = element_text(size=8, color="black"),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        #strip.text = element_text(face = "bold", size = 10, color = "black"),
        #strip.text = element_text(size = 8, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        #legend.box.background = element_rect(fill = "transparent"),
        legend.box.background=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        strip.text.x=element_text(size=8),
        strip.text.y = element_text(size = 8))
    
ALL_plot_more = ggplot_gtable(ggplot_build(ALL_plot))
#gtable_show_layout(ALL_plot_more)
ALL_plot_more$widths[4] = 0.95*ALL_plot_more$widths[4]
ALL_plot_more$widths[9] = 1.85*ALL_plot_more$widths[9]
ALL_plot_more$widths[13] = 1.85*ALL_plot_more$widths[13]
ALL_plot_more$widths[17] = 1.85*ALL_plot_more$widths[17]
ALL_plot_more$widths[25] = 1.85*ALL_plot_more$widths[25]
ALL_plot_more$widths[15] = 1.2*ALL_plot_more$widths[15]
ALL_plot_more$widths[21] = 1.2*ALL_plot_more$widths[21]
ALL_plot_more$widths[27] = 0.1*ALL_plot_more$widths[27]
ALL_plot_more$heights[8] = 1.35*ALL_plot_more$heights[8]
ALL_plot_more$heights[10] = 1.1*ALL_plot_more$heights[10]
ALL_plot_more$heights[12] = 1.1*ALL_plot_more$heights[12]
ALL_plot_more$heights[16] = 0.5*ALL_plot_more$heights[16]
as_ggplot(ALL_plot_more)

ggsave("Figure4_enrichment.pdf", plot = ALL_plot_more, device = "pdf", width = 7.5, height = 7.5, units = "in")
