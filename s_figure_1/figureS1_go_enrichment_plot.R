# Figure 2
## Cory

# Load necessary libraries
library(ggplot2)

# Change working directory to appropriate location
setwd("~/Documents/GitHub/cory/MPGI/figure2/")

# Read in the data
data <- read.delim("GOenrichment_output_parsed.txt", header = TRUE, sep = "\t")

# Turn Gene Ratio column to fraction
data$Gene_Ratio <- as.character(data$Gene_Ratio)
data$Gene_Ratio <- sapply(data$Gene_Ratio, function(x) eval(parse(text=x)))

# Turn dpi into factor
data$dpi <- as.factor(data$dpi)

## For UP regulated genes
UP <- subset(data, Enrichment_direction=="UP")

ggplot(UP, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=0.5, height=0.5) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y") +
    theme(
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
    )

# For printing
pdf("Fig_2_UP.pdf", width=7.5, height=10, pointsize=10)
ggplot(UP, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=0.5, height=0.5) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y") +
    theme(
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
    )
dev.off()



## If limiting to Top10 GO for each comparison
# Read in the data
data10 <- read.delim("GOenrichment_output_parsed_top10.txt", header = TRUE, sep = "\t")

# Turn Gene Ratio column to fraction
data10$Gene_Ratio <- as.character(data10$Gene_Ratio)
data10$Gene_Ratio <- sapply(data10$Gene_Ratio, function(x) eval(parse(text=x)))
data10$Gene_Ratio <- as.numeric(format(round(data10$Gene_Ratio, 5), nsmall=5))

# Turn dpi into factor
data10$dpi <- as.factor(data10$dpi)

## For UP regulated genes
UP10 <- subset(data10, Enrichment_direction=="UP")

# For printing
pdf("Fig_2_top10UP.pdf", width=7.5, height=10, pointsize=10)
ggplot(UP10, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y") +
    theme(
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
    )
dev.off()

## For DOWN regulated genes
DOWN10 <- subset(data10, Enrichment_direction=="DOWN")

# For printing
pdf("Fig_2_top10DOWN.pdf", width=7.5, height=10, pointsize=10)
ggplot(DOWN10, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y") +
    theme(
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
    )
dev.off()


## If limiting to Top5 GO for each comparison
# Read in the data
data5 <- read.delim("GOenrichment_output_parsed_top5.txt", header = TRUE, sep = "\t")

# Turn Gene Ratio column to fraction
data5$Gene_Ratio <- as.character(data5$Gene_Ratio)
data5$Gene_Ratio <- sapply(data5$Gene_Ratio, function(x) eval(parse(text=x)))
data5$Gene_Ratio <- as.numeric(format(round(data5$Gene_Ratio, 5), nsmall=5))

# Turn dpi into factor
data5$dpi <- as.factor(data5$dpi)

## Plotting UP/DOWN genes together
# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
              )
)

# For printing - Full page
pdf("Fig_2_top5.pdf", width=7.5, height=10.5, pointsize=10)
ggplot(data5, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Enrichment_direction ~ Domain ~ Genotype, scales = "free_y", space = "free_y") +
    theme(
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "bottom",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        #legend.box.background = element_rect(fill = "transparent"),
        legend.box.background=element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        strip.text.x=element_text(size=10)
    )
dev.off()

# For printing - Reduced term names - 1/2 page
# Read in the data
data5_short <- read.delim("GOenrichment_output_parsed_top5_short.txt", header = TRUE, sep = "\t")

# Turn Gene Ratio column to fraction
data5_short$Gene_Ratio <- as.character(data5_short$Gene_Ratio)
data5_short$Gene_Ratio <- sapply(data5_short$Gene_Ratio, function(x) eval(parse(text=x)))
data5_short$Gene_Ratio <- as.numeric(format(round(data5_short$Gene_Ratio, 5), nsmall=5))

# Turn dpi into factor
data5_short$dpi <- as.factor(data5_short$dpi)

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
data5_short$Genotype <- factor(data5_short$Genotype, levels=c("W2691", "W2691-Sr9b", "Bd21-3"))
levels(data5_short$Genotype) = c("W2691"=expression(paste("W2691")), "W2691-Sr9b"=expression(paste("W2691+", italic("Sr9b"))), "Bd21-3"=expression(paste("Bd21-3")))

pdf("Fig_2_top5_short.pdf", width=7, height=9.7, pointsize=10)
ggplot(data5_short, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Enrichment_direction ~ Domain ~ Genotype, scales = "free_y", space = "free_y", labeller=label_parsed) +
    theme(
        axis.text.x = element_text(size = 8, color="black"),
        axis.title = element_text(size = 8),
        axis.text.y = element_text(size=6, color="black"),
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
        strip.text.x=element_text(size=8)
    )
dev.off()


## To plot UP/DOWN separately
## For UP regulated genes
UP5 <- subset(data5, Enrichment_direction=="UP")

# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
              )
)

# For printing
pdf("Fig_2_top5UP.pdf", width=7.5, height=5.5, pointsize=10)
ggplot(UP5, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y", space = "free_y") +
    theme(
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "right",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10),
        strip.text.x=element_text(size=10)
    )
dev.off()

## For DOWN regulated genes
DOWN5 <- subset(data5, Enrichment_direction=="DOWN")

# For printing
pdf("Fig_2_top5DOWN.pdf", width=7.5, height=5.5, pointsize=10)
ggplot(DOWN5, aes(x=dpi, y=Description, fill=Gene_Ratio)) +
    geom_tile(width=1, height=1) +
    scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Gene Ratio") + 
    facet_grid(Domain ~ Genotype, scales = "free_y", space = "free_y") +
    theme(
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "transparent", color = "white", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "right",
        legend.justification = "top",
        legend.box = "horizontal",
        #legend.box.background = element_rect(colour = "black"),
        #legend.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=10),
        strip.text.x=element_text(size=10)
    )
dev.off()



























