# Figure 2 - GOSLIM edition
## Cory

# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Change working directory to appropriate location
setwd("~/Documents/GitHub/cory/MPGI/figure2/GO_SLIM/")

# Read in the data
data <- read.delim("GOenrichment_output_parsed.txt", header = TRUE, sep = "\t")

# Turn Gene Ratio column to fraction
data$Gene_Ratio <- as.character(data$Gene_Ratio)
data$Gene_Ratio <- sapply(data$Gene_Ratio, function(x) eval(parse(text=x)))

# Turn dpi into factor
data$dpi <- as.factor(data$dpi)

# Turn Gene Ratio column to fraction
#data5_short$Gene_Ratio <- as.numeric(format(round(data5_short$Gene_Ratio, 5), nsmall=5))

## Plotting UP/DOWN genes together
# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
              )
)

# To get italic labels in plot
data$Genotype <- factor(data$Genotype, levels=c("W2691", "W2691-Sr9b", "Bd21-3"))
levels(data$Genotype) = c("W2691"=expression(paste("W2691")), "W2691-Sr9b"=expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"=expression(paste("Bd21-3")))
levels(data$Description)[15] <- gsub("hydrolase activity, acting on acid anhydrides, in phosphorus-containing anhydrides", "hydrolase activity, acting on acid anhydrides, \n in phosphorus-containing anhydrides", levels(data$Description)[15])
# levels(data$Description)[14] <- gsub("hydrolase activity, acting on acid anhydrides", "hydrolase activity, \n acting on acid anhydrides", levels(data$Description)[14])
# levels(data$Description)[8] <- gsub("DNA-binding transcription factor activity", "DNA-binding transcription \n factor activity", levels(data$Description)[8])
# levels(data$Description)[26] <- gsub("translation factor activity, RNA binding", "translation factor activity, \n RNA binding", levels(data$Description)[26])
data$Enrichment_direction <- as.numeric(data$Enrichment_direction)
data$Enrichment_direction <- factor(data$Enrichment_direction, levels = c("2", "1"))

figure2_plot <- ggplot(data, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Enrichment_direction ~ Domain ~ Genotype, scales = "free", space = "free", labeller=label_parsed) +
    theme(
        axis.text.x = element_text(size = 8, color="black"),
        axis.title = element_text(size = 8),
        axis.text.y = element_text(size=8, color="black"),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        #strip.text = element_text(face = "bold", size = 10, color = "black"),
        strip.text = element_text(size = 8, color = "black"),
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
        strip.text.x=element_text(size=8))

figure2_plot_more = ggplot_gtable(ggplot_build(figure2_plot))
#gtable_show_layout(figure2_plot_more)
figure2_plot_more$widths[4] = 1*figure2_plot_more$widths[4]
figure2_plot_more$widths[5] = 0.8*figure2_plot_more$widths[5]
figure2_plot_more$widths[7] = 0.8*figure2_plot_more$widths[7]
figure2_plot_more$widths[9] = 0.8*figure2_plot_more$widths[9]
figure2_plot_more$heights[8] = 0.9*figure2_plot_more$heights[8]
figure2_plot_more$heights[10] = 0.9*figure2_plot_more$heights[10]
figure2_plot_more$heights[12] = 1.4*figure2_plot_more$heights[12]
figure2_plot_more$heights[14] = 1.2*figure2_plot_more$heights[14]
figure2_plot_more$heights[18] = 1.2*figure2_plot_more$heights[18]
figure2_plot_more$heights[22] = 0.5*figure2_plot_more$heights[22]
as_ggplot(figure2_plot_more)

ggsave(filename = "Fig_2_GOSLIM.pdf", plot = figure2_plot_more, device = "pdf", width= 7.5, height=5, units = "in")
