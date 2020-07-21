# Figure 1E
## Melania, Marisa, Cory

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotrix)

# Change working directory to appropriate location
setwd("~/Documents/GitHub/cory/MPGI/figure1/")

# Read in data
data_1e <- read.delim("fig_1e_data_raw.csv", header = TRUE, sep = ",", colClasses=c(rep("factor", 2), c(rep("numeric", 2))))

# Change DPI field to be numeric
data_1e$Dpi <- as.numeric(as.character(data_1e$Dpi))

# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
                    )
          )

# Set up plot for Figure 1E
my_labels <- c(expression(paste("W2691+", italic("Sr9b"))))
p_1e <- ggplot(data_1e, aes(x = Dpi, 
                            y = pg,
                            color = Genotype,
                            group = Genotype) )+
    geom_line(size=0.5) +
    geom_point(size=1) +
    geom_errorbar(aes(ymin = pg-SE,
                      ymax = pg+SE), 
                  width=.2,
                  alpha=.75,
                  color="black" ) +
    scale_color_manual(values = c("chartreuse3", "chocolate1", "blue1")) +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank(),
          legend.position = "none",
          axis.text = element_text(color="black"),
          plot.margin=unit(c(.4,0,0.1,0.1), "cm") ) +
    guides(fill=guide_legend(title=NULL)) +
    xlab("dpi") +
    ylab("Relative\nAbundance (pg)") +
    scale_y_continuous(breaks=c(0,5,10)) +
    scale_x_continuous(breaks=c(2,4,6), limits=c(1.75,8.2)) +
    annotate("text", label="W2691", x=6.71, y=9.6, color="chocolate1") +
    annotate("text", label="W2691+italic(Sr9b)", parse=TRUE, x=7.3, y=6.3, color="blue1") +
    annotate("text", label="Bd21-3", x=6.75, y=2, color="chartreuse3")

    
# Print plot for manuscript for Figure 1E
pdf("Fig_1E.pdf", width=3.53, height=1.25, pointsize=8)
p_1e
dev.off()



### Plot showing all the data points and not error bars
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(grid)

# Change working directory to appropriate location
setwd("~/Documents/GitHub/cory/MPGI/figure1/")

# Read in data
d_1e <- read.delim("fig_1e_datapoints.csv", header = TRUE, sep = ",", colClasses=c(rep("factor", 2), c(rep("numeric", 2))))

# Set theme to be classic
theme_set(theme_classic() + 
              theme(legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title = element_text(size=10),
                    strip.text.x = element_text(size = 10)
              )
)

# Set up plot for Figure 1E
my_labels <- c(expression(paste("W2691+", italic("Sr9b"))))
mean_data_1e <- d_1e %>% group_by(Genotype, dpi) %>% summarise_all(funs(mean))

# Set up plot for Figure 1E
p_1e_b <- ggplot(d_1e, aes(x = dpi, 
                           y = Relative.Abundance,
                           color = Genotype,
                           group = Genotype) ) +
    geom_jitter(size=1.5, width = 0.15) +
    geom_line(data=mean_data_1e, aes(x = dpi, 
                                     y = Relative.Abundance,
                                     color = Genotype,
                                     group = Genotype) )  +
    scale_color_manual(values = c("#00ADB5", "#F8B500", "#FC3C3C")) +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank(),
          legend.position = "none",
          axis.text = element_text(color="black"),
          plot.margin=unit(c(.4,2.5,0.1,0.1), "cm") ) +
    guides(fill=guide_legend(title=NULL)) +
    xlab("dpi") +
    ylab("Relative\nAbundance (pg)") +
    scale_y_continuous(breaks=c(0,5,10) ) +
    scale_x_continuous(breaks=c(2,4,6) )


# Print plot for manuscript for Figure 1E
pdf("Fig_1E_b.pdf", width=3.55, height=2, pointsize=10)
p_1e_b
grid.text("W2691", x=unit(0.74, "npc"), y=unit(0.88, "npc"), just="left", gp= gpar(col="#F8B500") )
grid.text("W2691+", hjust=0, x=unit(0.74, "npc"), y=unit(0.67, "npc"), just="left", gp= gpar(col="#FC3C3C") )
grid.text("Sr9b", x=unit(0.885,"npc"), y=unit(0.67, "npc"), just="left", gp=gpar(fontface="italic", col="#FC3C3C"), hjust=0 )
grid.text("Bd21-3", x=unit(0.74, "npc"), y=unit(0.35, "npc"), just="left", gp=gpar(col="#00ADB5") )

dev.off()





