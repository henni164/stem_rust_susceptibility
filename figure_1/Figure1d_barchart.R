# Figure 1D
## Melania, Marisa, Cory

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotrix)
library(scales)

# Change working directory to appropriate location
#setwd("~/Co-expression network analysis")
setwd("~/Documents/GitHub/cory/MPGI/figure1/")

# Read in data
data_1d <- read.delim("fig_1d_data_raw.csv", header = TRUE, sep = ",", colClasses=c(rep("factor", 3), "numeric"))

# Calculate the mean of reps, standard deviation, and standard error
mean_data_1d <- data_1d %>% group_by(Genotype, Dpi, Infection_Structure) %>% summarise_all(funs(mean,sd,std.error))

# Change the levels of 'Infection_Structure' and 'Genotype' for plotting in the right order
mean_data_1d$Infection_Structure <- factor(mean_data_1d$Infection_Structure, levels = c("GS", "AP", "C", "SC"))
mean_data_1d$Genotype <- factor(mean_data_1d$Genotype, levels = c("W2691", "W2691+Sr9b", "Bd21-3"))

# Relabel the days post inoculation
levels(mean_data_1d$Dpi)<-c("2 dpi", "4 dpi", "6 dpi")

# Set theme to be classic
theme_set(theme_classic() + 
            theme(legend.text = element_text(size=10),
                  axis.text = element_text(size=10),
                  axis.title = element_text(size=10),
                  strip.text.x = element_text(size=10),
                  text = element_text(size=10)
                  #axis.title.x = element_text(size=10),
                  #axis.text.y = element_text(size=10),
                  #axis.title.x = element_text(size=10),
                  #axis.title.y = element_text(size=10)
                  )
          )


# Set up plot for Figure 1D
my_labels <- c("W2691", expression(paste("W2691+", italic("Sr9b"))), "Bd21-3")

p_1d <- ggplot(mean_data_1d, aes(x = Genotype,
                             y = mean,
                             fill = Infection_Structure) ) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-std.error,
                    ymax = mean+std.error,
                    width=.5) ) +
  scale_fill_manual(values= c("#d2b4de", "#a569bd", "#7d3c98", "#5b2c6f")) +
  facet_grid(Infection_Structure ~ Dpi, scales="free_y") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_text(angle=45,
                                   hjust=1),
        legend.position = "top",
        legend.margin=margin(0,0,-15,0),
        legend.box.margin=margin(0,0,0,0),
        legend.title = element_blank(),
        legend.key.size=unit(0.3, "cm"),
        axis.text = element_text(color="black"),
        plot.margin=unit(c(0,0,-.45,.05), "cm") ) +
  ylab("% of Interaction Sites") +
   # scale_y_continuous(breaks=c(0, 25, 50, 75, 100)) +
    scale_y_continuous(breaks = pretty_breaks(n=2))+
    scale_x_discrete(labels = my_labels)

# Print plot for manuscript for Figure 1D
pdf("Fig_1D.pdf", width=3.53, height=3)
p_1d
dev.off()


