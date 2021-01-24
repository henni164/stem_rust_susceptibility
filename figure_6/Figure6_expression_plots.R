# Figure 3 plotting code
## Eva
## Started 10/01/2020
library(network)
library(ggplot2)
library(GGally)
library(sna)
library(ggnetwork)
library(dplyr)
library(stringr)
library(stringi)
library(cowplot)


options(scipen = 999)
set.seed(204)
setwd("/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_6")

## Brachy plot loop

Bd21_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_6/Bd21", pattern = ".txt", full.names = TRUE, recursive = FALSE)
Bd21_files <- as.vector(Bd21_files)

Bd21_dat <- lapply(Bd21_files, FUN = read.delim, sep = "\t", header = TRUE, colClasses = c("character", "character","numeric"))

Bd21_networks <- lapply(Bd21_dat, FUN = function(x){
  Bd21_net <- network(x, directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
  Bd21_names_vec <- ggnetwork(Bd21_net)$vertex.names
  Bd21_names_vec <- unique(Bd21_names_vec)
  Bd21_net %v% "importance" = ifelse(Bd21_names_vec %in% c("10357000", "2102231900","2202253500","2302236800",
                                                           "20000500","40022900","1102070400","1202088900","1302072900",
                                                           "10091300","40011900","5102019200","530202460","5302424200",
                                                           "10234900","2102109700","2202127700","2302109900","10280300",
                                                           "10628300","4102116000","4302189600","7302452900","10110600",
                                                           "5302404600","11026800","4202346900","4302341800","30030500",
                                                           "6102083200","6202113900","6302077000"), "yes", "no")
  
  #ggnetwork(Bd21_net, layout = "fruchtermanreingold", weights = "score")
  Bd21_plot <- ggplot(Bd21_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_color_manual(values = c("#000000","#F00000")) +
    scale_alpha_manual(values = c(0.7, 1)) +
    scale_size_manual(values = c(0.8,1)) +
    geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
    geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
    theme_blank() +
    theme(legend.position = "none")
  
  Bd21_plot_data <- as.data.frame(Bd21_plot$data)
  Bd21_important_dat <- subset(Bd21_plot_data, importance == "yes")
  
  Bd21_final_plot <- Bd21_plot + geom_edges(data = Bd21_important_dat, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = Bd21_important_dat, color = "#F00000", size = 1, inherit.aes = TRUE)
  
})

# for (i in 1:length(Bd21_dat)) {
#   ggsave(filename = paste("Bd213_",i,".tiff",sep=""), plot = Bd21_networks[[i]], device = "tiff", width = 2, height = 2, units = "in") 
# }


## W2691 plot loop

W2691_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_6/W2691", pattern = ".txt", full.names = TRUE, recursive = FALSE)
W2691_files <- as.vector(W2691_files)

W2691_dat <- lapply(W2691_files, FUN = read.delim, sep = "\t", header = TRUE, colClasses = c("character", "character","numeric"))

W2691_networks <- lapply(W2691_dat, FUN = function(x){
  W2691_net <- network(x, directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
  W2691_names_vec <- ggnetwork(W2691_net)$vertex.names
  W2691_names_vec <- unique(W2691_names_vec)
  W2691_net %v% "importance" = ifelse(W2691_names_vec %in% c("10357000", "2102231900","2202253500","2302236800",
                                                             "20000500","40022900","1102070400","1202088900","1302072900",
                                                             "10091300","40011900","5102019200","530202460","5302424200",
                                                             "10234900","2102109700","2202127700","2302109900","10280300",
                                                             "10628300","4102116000","4302189600","7302452900","10110600",
                                                             "5302404600","11026800","4202346900","4302341800","30030500",
                                                             "6102083200","6202113900","6302077000"), "yes", "no")
  
  #ggnetwork(W2691_net, layout = "fruchtermanreingold", weights = "score")
  W2691_plot <- ggplot(W2691_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_color_manual(values = c("#000000","#F00000")) +
    scale_alpha_manual(values = c(0.7, 1)) +
    scale_size_manual(values = c(0.8,1)) +
    geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
    geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
    theme_blank() +
    theme(legend.position = "none")
  
  W2691_plot_data <- as.data.frame(W2691_plot$data)
  W2691_important_dat <- subset(W2691_plot_data, importance == "yes")
  
  W2691_final_plot <- W2691_plot + geom_edges(data = W2691_important_dat, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = W2691_important_dat, color = "#F00000", size = 1, inherit.aes = TRUE)
  
})

# for (i in 1:length(W2691_dat)) {
#   ggsave(filename = paste("W2691_",i,".tiff",sep=""), plot = W2691_networks[[i]], device = "tiff", width = 2, height = 2, units = "in") 
# }

## Sr9b plot loop

Sr9b_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_6/Sr9b", pattern = ".txt", full.names = TRUE, recursive = FALSE)
Sr9b_files <- as.vector(Sr9b_files)

Sr9b_dat <- lapply(Sr9b_files, FUN = read.delim, sep = "\t", header = TRUE, colClasses = c("character", "character","numeric"))

Sr9b_networks <- lapply(Sr9b_dat, FUN = function(x){
  Sr9b_net <- network(x, directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
  Sr9b_names_vec <- ggnetwork(Sr9b_net)$vertex.names
  Sr9b_names_vec <- unique(Sr9b_names_vec)
  Sr9b_net %v% "importance" = ifelse(Sr9b_names_vec %in% c("10357000","20000500","40022900","10091300","40011900","10234900","10280300","10628300",
                                                           "10110600","11026800","30030500","4102116000","4302189600","7302452900","6102083200",
                                                           "6202113900","6302077000","5102019200","5302024600","5302424200","1102070400","1202088900",
                                                           "1302072900","2102109700","2202127700","2302109900","2102231900","2202253500","2302236800",
                                                           "4202346900","4302341800","5302404600"), "yes","no")
  
  #ggnetwork(Sr9b_net, layout = "fruchtermanreingold", weights = "score")
  Sr9b_plot <- ggplot(Sr9b_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_color_manual(values = c("#000000","#F00000")) +
    scale_alpha_manual(values = c(0.7, 1)) +
    scale_size_manual(values = c(0.8,1)) +
    geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
    geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
    theme_blank() +
    theme(legend.position = "none")
  
  Sr9b_plot_data <- as.data.frame(Sr9b_plot$data)
  Sr9b_important_dat <- subset(Sr9b_plot_data, importance == "yes")
  
  Sr9b_final_plot <- Sr9b_plot + geom_edges(data = Sr9b_important_dat, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = Sr9b_important_dat, color = "#F00000", size = 1, inherit.aes = TRUE)
  
})
# 
# for (i in 1:length(Sr9b_dat)) {
#   ggsave(filename = paste("Sr9b_",i,".tiff",sep=""), plot = Sr9b_networks[[i]], device = "tiff", width = 2, height = 2, units = "in") 
# }

# fixing the first W2691 network
W2691_net_5 <- network(W2691_dat[[5]], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_names_vec <- ggnetwork(W2691_net_5)$vertex.names
W2691_names_vec <- unique(W2691_names_vec)
W2691_net_5 %v% "importance" = ifelse(W2691_names_vec %in% c("5302404600"), "yes", "no")

#ggnetwork(W2691_net, layout = "fruchtermanreingold", weights = "score")
W2691_plot_5 <- ggplot(W2691_net_5, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000","#F00000")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_size_manual(values = c(0.8,1)) +
  geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
  geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
  theme_blank() +
  theme(legend.position = "none")

W2691_plot_data_5 <- as.data.frame(W2691_plot_5$data)
W2691_important_dat_5 <- subset(W2691_plot_data_5, importance == "yes")

W2691_plot5_fix <- W2691_plot_5 + geom_edges(data = W2691_important_dat_5, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = W2691_important_dat_5, color = "#F00000", size = 1, inherit.aes = TRUE)

## for the identical networks

W2691_net_1_1 <- network(W2691_dat[[1]], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_names_vec_1_1 <- ggnetwork(W2691_net_1_1)$vertex.names
W2691_names_vec_1_1 <- unique(W2691_names_vec_1_1)
W2691_net_1_1 %v% "importance" = ifelse(W2691_names_vec_1_1 %in% c("2202253500"), "yes", "no")

#ggnetwork(W2691_net, layout = "fruchtermanreingold", weights = "score")
W2691_plot_1_1 <- ggplot(W2691_net_1_1, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000","#F00000")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_size_manual(values = c(0.8,1)) +
  geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
  geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
  theme_blank() +
  theme(legend.position = "none")

W2691_plot_data_1_1 <- as.data.frame(W2691_plot_1_1$data)
W2691_important_dat_1_1 <- subset(W2691_plot_data_1_1, importance == "yes")

W2691_plot_f_1_1 <- W2691_plot_1_1 + geom_edges(data = W2691_important_dat_1_1, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = W2691_important_dat_1_1, color = "#F00000", size = 1, inherit.aes = TRUE)

## other gene
W2691_net_1_2 <- network(W2691_dat[[1]], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_names_vec_1_2 <- ggnetwork(W2691_net_1_2)$vertex.names
W2691_names_vec_1_2 <- unique(W2691_names_vec_1_2)
W2691_net_1_2 %v% "importance" = ifelse(W2691_names_vec_1_2 %in% c("4202346900", "4302341800"), "yes", "no")

#ggnetwork(W2691_net, layout = "fruchtermanreingold", weights = "score")
W2691_plot_1_2 <- ggplot(W2691_net_1_2, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000","#F00000")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_size_manual(values = c(0.8,1)) +
  geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
  geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
  theme_blank() +
  theme(legend.position = "none")

W2691_plot_data_1_2 <- as.data.frame(W2691_plot_1_2$data)
W2691_important_dat_1_2 <- subset(W2691_plot_data_1_2, importance == "yes")

W2691_plot_f_1_2 <- W2691_plot_1_2 + geom_edges(data = W2691_important_dat_1_2, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = W2691_important_dat_1_2, color = "#F00000", size = 1, inherit.aes = TRUE)

# For Sr9b
## first 
Sr9b_net_1_1 <- network(Sr9b_dat[[1]], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
Sr9b_names_vec_1_1 <- ggnetwork(Sr9b_net_1_1)$vertex.names
Sr9b_names_vec_1_1 <- unique(Sr9b_names_vec_1_1)
Sr9b_net_1_1 %v% "importance" = ifelse(Sr9b_names_vec_1_1 %in% c("2202253500"), "yes","no")

#ggnetwork(Sr9b_net, layout = "fruchtermanreingold", weights = "score")
Sr9b_plot_1_1 <- ggplot(Sr9b_net_1_1, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000","#F00000")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_size_manual(values = c(0.8,1)) +
  geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
  geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
  theme_blank() +
  theme(legend.position = "none")

Sr9b_plot_data_1_1 <- as.data.frame(Sr9b_plot_1_1$data)
Sr9b_important_dat_1_1 <- subset(Sr9b_plot_data_1_1, importance == "yes")

Sr9b_final_plot_1_1 <- Sr9b_plot_1_1 + geom_edges(data = Sr9b_important_dat_1_1, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = Sr9b_important_dat_1_1, color = "#F00000", size = 1, inherit.aes = TRUE)

## second
Sr9b_net_1_2 <- network(Sr9b_dat[[1]], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
Sr9b_names_vec_1_2 <- ggnetwork(Sr9b_net_1_2)$vertex.names
Sr9b_names_vec_1_2 <- unique(Sr9b_names_vec_1_2)
Sr9b_net_1_2 %v% "importance" = ifelse(Sr9b_names_vec_1_2 %in% c("4202346900", "4302341800"), "yes","no")

#ggnetwork(Sr9b_net, layout = "fruchtermanreingold", weights = "score")
Sr9b_plot_1_2 <- ggplot(Sr9b_net_1_2, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000","#F00000")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_size_manual(values = c(0.8,1)) +
  geom_edges(color = "grey25", alpha = 0.1, size = 0.4) +
  geom_nodes(aes(color = importance, alpha = importance, size = importance)) +
  theme_blank() +
  theme(legend.position = "none")

Sr9b_plot_data_1_2 <- as.data.frame(Sr9b_plot_1_2$data)
Sr9b_important_dat_1_2 <- subset(Sr9b_plot_data_1_2, importance == "yes")

Sr9b_final_plot_1_2 <- Sr9b_plot_1_2 + geom_edges(data = Sr9b_important_dat_1_2, color = "grey25", alpha = 0.1, size = 0.4, inherit.aes = TRUE) + geom_nodes(data = Sr9b_important_dat_1_2, color = "#F00000", size = 1, inherit.aes = TRUE)


#load necessary libraries
library(scales)
library(reshape2)
library(tidyr)
library(egg)
library(ggpubr)
library(dplyr)
## Data exploration for figure 3
# Directory for expression matrix files
setwd("/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_6")

Bd21_cluster_info <- read.csv("network_clusters.SR_brachy_2.csv", header = TRUE)
W2691_cluster_info <- read.csv("network_clusters.SR_W2691_2.csv", header = TRUE)
Sr9b_cluster_info <- read.csv("network_clusters.SR_Sr9b_2.csv", header = TRUE)
bd_expression_info <- read.delim("brachy_counts_fpkm.txt", header = TRUE)
wheat_expression_info <- read.delim("wheat_counts_fpkm.txt", header = TRUE)
cols <- c(1,20:37)
W2691_expression_info <- wheat_expression_info[,cols]
Sr9b_expression_info <- wheat_expression_info[,1:19]

Bd21_complete <- left_join(x = Bd21_cluster_info, y = bd_expression_info, by = "geneid")
W2691_complete <- left_join(x = W2691_cluster_info, y = W2691_expression_info, by = "geneid")
Sr9b_complete <- left_join(x = Sr9b_cluster_info, y = Sr9b_expression_info, by = "geneid")

Bd21_melted <- melt(Bd21_complete, id.vars = c("geneid", "cluster"))
W2691_melted <- melt(W2691_complete, id.vars = c("geneid", "cluster"))
Sr9b_melted <- melt(Sr9b_complete, id.vars = c("geneid", "cluster"))

Bd21_met <- separate(data = Bd21_melted, col = variable, into = c("genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)
W2691_met <- separate(data = W2691_melted, col = variable, into = c("genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)
Sr9b_met <- separate(data = Sr9b_melted, col = variable, into = c("genotype", "day", "treatment", "rep"), sep = "_", remove = TRUE)
Bd21_met$value <- Bd21_met$value + 0.001
W2691_met$value <- W2691_met$value + 0.001
Sr9b_met$value <- Sr9b_met$value + 0.001

Bd21_rep_means <- Bd21_met %>%
  group_by(geneid, cluster, genotype, day, treatment) %>%
  summarise(Mean = mean(value)) %>%
  as.data.frame()
W2691_rep_means <- W2691_met %>%
  group_by(geneid, cluster, genotype, day, treatment) %>%
  summarise(Mean = mean(value)) %>%
  as.data.frame()
Sr9b_rep_means <- Sr9b_met %>%
  group_by(geneid, cluster, genotype, day, treatment) %>%
  summarise(Mean = mean(value)) %>%
  as.data.frame()

#data exploration
Bd21_quot <- Bd21_rep_means %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Quotient = (Mean[treatment == "treated"]/Mean[treatment == "mock"])) %>%
  as.data.frame()
Bd21_log2fc <- Bd21_quot %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Log2FC = log2(Quotient)) %>%
  as.data.frame()
W2691_quot <- W2691_rep_means %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Quotient = (Mean[treatment == "treated"]/Mean[treatment == "mock"])) %>%
  as.data.frame()
W2691_log2fc <- W2691_quot %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Log2FC = log2(Quotient)) %>%
  as.data.frame()
Sr9b_quot <- Sr9b_rep_means %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Quotient = (Mean[treatment == "treated"]/Mean[treatment == "mock"])) %>%
  as.data.frame()
Sr9b_log2fc <- Sr9b_quot %>%
  group_by(geneid, cluster, genotype, day) %>%
  summarise(Log2FC = log2(Quotient)) %>%
  as.data.frame()

Bd21_max <- Bd21_log2fc %>% group_by(cluster, day) %>%
  slice_max(Log2FC)
W2691_max <- W2691_log2fc %>% group_by(cluster, day) %>%
  slice_max(Log2FC)
Sr9b_max <- Sr9b_log2fc %>% group_by(cluster, day) %>%
  slice_max(Log2FC)

###

Bd21_rep_means$log2 <- log2(Bd21_rep_means$Mean)
W2691_rep_means$log2 <- log2(W2691_rep_means$Mean)
Sr9b_rep_means$log2 <- log2(Sr9b_rep_means$Mean)

Bd21_rep_means$combined <- paste(Bd21_rep_means$day, Bd21_rep_means$treatment)
W2691_rep_means$combined <- paste(W2691_rep_means$day, Bd21_rep_means$treatment)
Sr9b_rep_means$combined <- paste(Sr9b_rep_means$day, Sr9b_rep_means$treatment)

Bd21_rep_means$log2fc <- Bd21_log2fc$Log2FC
W2691_rep_means$log2fc <- W2691_log2fc$Log2FC
Sr9b_rep_means$log2fc <- Sr9b_log2fc$Log2FC

bd_cluster_list <- c(4,35,51,272,652,1662,1848,2962,3087)
W2691_cluster_list <- c(0,3,4,5,8,11,60,178,3242,20182)
Sr9b_cluster_list <- c(0,4,122,1916,2729,2772,3133,11647)

bd_cluster_subsets <- lapply(bd_cluster_list, FUN = function(x){
  subset(Bd21_log2fc, cluster == x)
})

W2691_cluster_subsets <- lapply(W2691_cluster_list, FUN = function(x){
  subset(W2691_log2fc, cluster == x)
})

Sr9b_cluster_subsets <- lapply(Sr9b_cluster_list, FUN = function(x){
  subset(Sr9b_log2fc, cluster == x)
})

## printing info to use for table s5
# bd_printing <- bind_rows(bd_cluster_subsets)
# bd_printing <- bd_printing[,1:2]
# bd_printing <- unique(bd_printing)
# write.table(bd_printing, file = "/Users/evahenningsen/Documents/MPGI_paper/Table_S5/bd21_prnt_out.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
# w_printing <- bind_rows(W2691_cluster_subsets)
# w_printing <- w_printing[,1:2]
# w_printing <- unique(w_printing)
# write.table(w_printing, file = "/Users/evahenningsen/Documents/MPGI_paper/Table_S5/w2691_prnt_out.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
# s_printing <- bind_rows(Sr9b_cluster_subsets)
# s_printing <- s_printing[,1:2]
# s_printing <- unique(s_printing)
# write.table(s_printing, file = "/Users/evahenningsen/Documents/MPGI_paper/Table_S5/sr9b_prnt_out.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

interest_list <- c("BDIBD1G0357000","BDIBD2G0000500","BDIBD4G0022900","BDIBD1G0091300","BDIBD4G0011900","BDIBD1G0234900","BDIBD1G0280300","BDIBD1G0628300",
                   "BDIBD1G0110600","BDIBD1G1026800","BDIBD3G0030500","TRAESCS4A02G116000","TRAESCS4D02G189600","TRAESCS7D02G452900","TRAESCS6A02G083200",
                   "TRAESCS6B02G113900","TRAESCS6D02G077000","TRAESCS5A02G019200","TRAESCS5D02G024600","TRAESCS5D02G424200","TRAESCS1A02G070400","TRAESCS1B02G088900",
                   "TRAESCS1D02G072900","TRAESCS2A02G109700","TRAESCS2B02G127700","TRAESCS2D02G109900","TRAESCS2A02G231900","TRAESCS2B02G253500","TRAESCS2D02G236800",
                   "TRAESCS4B02G346900","TRAESCS4D02G341800","TRAESCS5D02G404600")

## Brachy
Bd21_plotting <- lapply(bd_cluster_subsets, FUN = function(x){
  ggplot(x, aes(x = day, y = Log2FC, group = geneid)) +
    geom_line(size = 0.1) +
    labs(x = "dpi",
         y = "") +
    scale_x_discrete(labels = c("2", "4", "6")) +
    scale_y_continuous(breaks = breaks_extended(3), limits = c(min(x$Log2FC), max(x$Log2FC))) +
    scale_color_manual(values = c("#636363")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          #axis.text.x = element_text(color = "black"),
          axis.text.x = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 8),
          legend.position = "none")
})

for (i in 1:length(Bd21_plotting)) {
  
  for (j in 1:length(interest_list)) {
    
  Bd21_plotting[[i]] <- Bd21_plotting[[i]] + geom_line(data = subset(bd_cluster_subsets[[i]], geneid == interest_list[j]), inherit.aes = TRUE, color = "#D0270C")
    
  }
  
}

# for (k in 1:length(Bd21_plotting)) {
#   ggsave(filename = paste("Bd213_",k,"expr.tiff", sep = ""), plot = Bd21_plotting[[k]], device = "tiff", width = 2, height = 0.75, units = "in")
# }

## W2691
W2691_plotting <- lapply(W2691_cluster_subsets, FUN = function(x){
  ggplot(x, aes(x = day, y = Log2FC, group = geneid)) +
    geom_line(size = 0.1) +
    labs(x = "dpi",
         y = expression(paste(log[2], " fold change", sep = ""))) +
    scale_x_discrete(labels = c("2", "4", "6")) + 
    scale_y_continuous(breaks = breaks_extended(3), limits = c(min(x$Log2FC), max(x$Log2FC))) +
    scale_color_manual(values = c("#636363")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          #axis.text.x = element_text(color = "black"),
          axis.text.x = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.y = element_text(color = "black", size = 8),
          axis.title.x = element_text(color = "black", size = 8),
          legend.position = "none")
})

for (i in 1:length(W2691_plotting)) {
  
  for (j in 1:length(interest_list)) {
    
    W2691_plotting[[i]] <- W2691_plotting[[i]] + geom_line(data = subset(W2691_cluster_subsets[[i]], geneid == interest_list[j]), inherit.aes = TRUE, color = "#D0270C")
    
  }
  
}

# for (k in 1:length(W2691_plotting)) {
#   ggsave(filename = paste("W2691_",k,"expr.tiff", sep = ""), plot = W2691_plotting[[k]], device = "tiff", width = 2, height = 0.75, units = "in")
# }

## Sr9b
Sr9b_plotting <- lapply(Sr9b_cluster_subsets, FUN = function(x){
  ggplot(x, aes(x = day, y = Log2FC, group = geneid)) +
    geom_line(size = 0.1) +
    labs(x = "dpi",
         y = "") +
    scale_x_discrete(labels = c("2", "4", "6")) + 
    scale_y_continuous(breaks = breaks_extended(3), limits = c(min(x$Log2FC), max(x$Log2FC))) +
    scale_color_manual(values = c("#636363")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(color = "black"), 
          #axis.text.x = element_text(color = "black"),
          axis.text.x = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 8),
          legend.position = "none")
})

for (i in 1:length(Sr9b_plotting)) {
  
  for (j in 1:length(interest_list)) {
    
    Sr9b_plotting[[i]] <- Sr9b_plotting[[i]] + geom_line(data = subset(Sr9b_cluster_subsets[[i]], geneid == interest_list[j]), inherit.aes = TRUE, color = "#D0270C")
    
  }
  
}

# for (k in 1:length(Sr9b_plotting)) {
#   ggsave(filename = paste("Sr9b_",k,"expr.tiff", sep = ""), plot = Sr9b_plotting[[k]], device = "tiff", width = 2, height = 0.75, units = "in")
# }


# trying to print everything out at once

W2691_name <- ggplot(W2691_cluster_subsets[[1]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(W2691_cluster_subsets[[1]]$Log2FC), max(W2691_cluster_subsets[[1]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black"),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")
W2691_name <- W2691_name + geom_line(data = subset(W2691_cluster_subsets[[1]], geneid == "TRAESCS4B02G346900"), inherit.aes = TRUE, color = "#D0270C")
W2691_name <- W2691_name + geom_line(data = subset(W2691_cluster_subsets[[1]], geneid == "TRAESCS4D02G341800"), inherit.aes = TRUE, color = "#D0270C")


Sr9b_name <- ggplot(Sr9b_cluster_subsets[[1]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = "") +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(Sr9b_cluster_subsets[[1]]$Log2FC), max(Sr9b_cluster_subsets[[1]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black"),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")

Sr9b_name <- Sr9b_name + geom_line(data = subset(Sr9b_cluster_subsets[[1]], geneid == "TRAESCS4B02G346900"), inherit.aes = TRUE, color = "#D0270C")
Sr9b_name <- Sr9b_name + geom_line(data = subset(Sr9b_cluster_subsets[[1]], geneid == "TRAESCS4D02G341800"), inherit.aes = TRUE, color = "#D0270C")

Bd21_name <- ggplot(bd_cluster_subsets[[1]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = "") +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(bd_cluster_subsets[[1]]$Log2FC), max(bd_cluster_subsets[[1]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black"),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")

for (j in 1:length(interest_list)) {
  
  Bd21_name <- Bd21_name + geom_line(data = subset(bd_cluster_subsets[[1]], geneid == interest_list[j]), inherit.aes = TRUE, color = "#D0270C")
  
}

single_point_plot <- ggplot() +
  geom_point(aes(x = 0.5, y = 0.5), alpha = 1, color = "#F00000") +
  theme_blank() +
  theme(legend.position = "none")


W2691_lines_4 <- ggplot(W2691_cluster_subsets[[3]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(W2691_cluster_subsets[[3]]$Log2FC), max(W2691_cluster_subsets[[3]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 8),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")
W2691_lines_4 <- W2691_lines_4 + geom_line(data = subset(W2691_cluster_subsets[[3]], geneid == "TRAESCS5D02G404600"), inherit.aes = TRUE, color = "#D0270C")


W2691_lines_0 <- ggplot(W2691_cluster_subsets[[1]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = expression(paste(log[2], " fold change", sep = ""))) +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(W2691_cluster_subsets[[1]]$Log2FC), max(W2691_cluster_subsets[[1]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 8),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")

W2691_lines_0 <- W2691_lines_0 + geom_line(data = subset(W2691_cluster_subsets[[1]], geneid == "TRAESCS2B02G253500"), inherit.aes = TRUE, color = "#D0270C")

Sr9b_lines_0 <- ggplot(Sr9b_cluster_subsets[[1]], aes(x = day, y = Log2FC, group = geneid)) +
  geom_line(size = 0.1) +
  labs(x = "dpi",
       y = "") +
  scale_x_discrete(labels = c("2", "4", "6")) + 
  scale_y_continuous(breaks = breaks_extended(3), limits = c(min(Sr9b_cluster_subsets[[1]]$Log2FC), max(Sr9b_cluster_subsets[[1]]$Log2FC))) +
  scale_color_manual(values = c("#636363")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black"),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 8),
        legend.position = "none")

Sr9b_lines_0 <- Sr9b_lines_0 + geom_line(data = subset(Sr9b_cluster_subsets[[1]], geneid == "TRAESCS2B02G253500"), inherit.aes = TRUE, color = "#D0270C")


combined_plot <- egg::ggarrange(W2691_plot5_fix, Sr9b_networks[[2]], Bd21_networks[[7]], W2691_lines_4, Sr9b_plotting[[3]], Bd21_plotting[[5]], W2691_plot_f_1_1, Sr9b_final_plot_1_1, single_point_plot, W2691_lines_0, Sr9b_lines_0, Bd21_plotting[[8]], W2691_plot_f_1_2, Sr9b_final_plot_1_2, Bd21_networks[[5]], W2691_name, Sr9b_name, Bd21_name, nrow = 6, ncol = 3, widths = c(1,1,1), heights = c(2,0.75,2,0.75,2,0.75))
combined_plot_w_text <- ggdraw(combined_plot) + draw_label("W2691", x = 0.16, y = 0.98, size = 10, fontface = "bold") +
  draw_label("W2691+Sr9b", x = 0.53, y = 0.98, size = 10, fontface = "bold") + 
  draw_label("Bd21-3", x = 0.85, y = 0.98, size = 10, fontface = "bold") +
  draw_label("cluster ID:4", x = 0.06, y = 0.96, size = 8, fontface = "bold") +
  draw_label("cluster ID:122", x = 0.38, y = 0.96, size = 8, fontface = "bold") +
  draw_label("cluster ID:652", x = 0.7, y = 0.96, size = 8, fontface = "bold") +
  draw_label("cluster ID:0", x = 0.06, y = 0.659, size = 8, fontface = "bold") +
  draw_label("cluster ID:0", x = 0.38, y = 0.659, size = 8, fontface = "bold") +
  draw_label("cluster ID:3087", x = 0.7, y = 0.659, size = 8, fontface = "bold") +
  draw_label("cluster ID:0", x = 0.06, y = 0.32, size = 8, fontface = "bold") +
  draw_label("cluster ID:0", x = 0.38, y = 0.32, size = 8, fontface = "bold") +
  draw_label("cluster ID:4", x = 0.7, y = 0.32, size = 8, fontface = "bold") +
  draw_label("n = 556", x = 0.14, y = 0.78, color = "black", size = 8) +
  draw_label("n = 1", x = 0.23, y = 0.78, color = "red", size = 8) +
  draw_label("n = 20", x = 0.47, y = 0.78, color = "black", size = 8) +
  draw_label("n = 1", x = 0.57, y = 0.78, color = "red", size = 8) +
  draw_label("n = 3", x = 0.80, y = 0.78, color = "black", size = 8) +
  draw_label("n = 1", x = 0.89, y = 0.78, color = "red", size = 8) +
  draw_label("n = 4526", x = 0.14, y = 0.45, color = "black", size = 8) +
  draw_label("n = 1", x = 0.23, y = 0.45, color = "red", size = 8) +
  draw_label("n = 3399", x = 0.47, y = 0.45, color = "black", size = 8) +
  draw_label("n = 1", x = 0.57, y = 0.45, color = "red", size = 8) +
  draw_label("n = 0", x = 0.80, y = 0.45, color = "black", size = 8) +
  draw_label("n = 1", x = 0.89, y = 0.45, color = "red", size = 8) +
  draw_label("n = 4525", x = 0.14, y = 0.12, color = "black", size = 8) +
  draw_label("n = 2", x = 0.23, y = 0.12, color = "red", size = 8) +
  draw_label("n = 3398", x = 0.47, y = 0.12, color = "black", size = 8) +
  draw_label("n = 2", x = 0.57, y = 0.12, color = "red", size = 8) +
  draw_label("n = 442", x = 0.80, y = 0.12, color = "black", size = 8) +
  draw_label("n = 1", x = 0.89, y = 0.12, color = "red", size = 8)

ggsave("Figure_6_together.tiff", combined_plot_w_text, device = "tiff", width = 7.08, height = 10, units = "in")

