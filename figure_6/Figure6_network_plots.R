library(network)
library(ggplot2)
library(GGally)
library(sna)
library(ggnetwork)

## Brachy VAD1
brachy_vad1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Bd21_networks/Bd21_slim_goterms_BDIBD213.1G0357000.txt")
brachy_vad1_network <- network(brachy_vad1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
brachy_vad1_network <- network.edgelist(x = brachy_vad1[,1:3], g = brachy_vad1_network, ignore.eval = FALSE, names.eval = "score")
brachy_vad1_names_vec <- ggnetwork(brachy_vad1_network)$vertex.names
brachy_vad1_names_vec <- unique(brachy_vad1_names_vec)
brachy_vad1_network %v% "importance" = ifelse(brachy_vad1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                               "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                               "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                               "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                               "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(brachy_vad1_network, layout = "fruchtermanreingold", weights = "score")
brachy_vad1_plot <- ggplot(brachy_vad1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.3) +
  geom_nodes(aes(color = importance), alpha = 0.75, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")

ggsave("brachy_VAD1.tiff", plot = brachy_vad1_plot, device = "tiff", width = 2, height = 2, units = "in")

## W2691 VAD1
W2691_vad1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/W2691_networks/W2691_slim_goterms_TRAESCS2A01G231900.txt")
W2691_vad1_network <- network(W2691_vad1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_vad1_network <- network.edgelist(x = W2691_vad1[,1:3], g = W2691_vad1_network, ignore.eval = FALSE, names.eval = "score")
W2691_vad1_names_vec <- ggnetwork(W2691_vad1_network)$vertex.names
W2691_vad1_names_vec <- unique(W2691_vad1_names_vec)
W2691_vad1_network %v% "importance" = ifelse(W2691_vad1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                           "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                           "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                           "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                           "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(W2691_vad1_network, layout = "fruchtermanreingold", weights = "score")
W2691_vad1_plot <- ggplot(W2691_vad1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.15) +
  geom_nodes(aes(color = importance), alpha = 0.6, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("W2691_VAD1.tiff",  plot = W2691_vad1_plot, device = "tiff", width = 2, height = 2, units = "in")


## Sr9b VAD1
Sr9b_vad1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Sr9b_networks/Sr9b_slim_goterms_TRAESCS2A01G231900.txt")
Sr9b_vad1_network <- network(Sr9b_vad1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
Sr9b_vad1_network <- network.edgelist(x = Sr9b_vad1[,1:3], g = Sr9b_vad1_network, ignore.eval = FALSE, names.eval = "score")
Sr9b_vad1_names_vec <- ggnetwork(Sr9b_vad1_network)$vertex.names
Sr9b_vad1_names_vec <- unique(Sr9b_vad1_names_vec)
Sr9b_vad1_network %v% "importance" = ifelse(Sr9b_vad1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                           "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                           "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                           "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                           "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(Sr9b_vad1_network, layout = "fruchtermanreingold", weights = "score")
Sr9b_vad1_plot <- ggplot(Sr9b_vad1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.2) +
  geom_nodes(aes(color = importance), alpha = 0.6, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("Sr9b_VAD1.tiff",  plot = Sr9b_vad1_plot, device = "tiff", width = 2, height = 2, units = "in")

## Brachy DND1
brachy_dnd1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Bd21_networks/Bd21_slim_goterms_BDIBD213.1G0110600.txt")
brachy_dnd1_network <- network(brachy_dnd1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
brachy_dnd1_network <- network.edgelist(x = brachy_dnd1[,1:3], g = brachy_dnd1_network, ignore.eval = FALSE, names.eval = "score")
brachy_dnd1_names_vec <- ggnetwork(brachy_dnd1_network)$vertex.names
brachy_dnd1_names_vec <- unique(brachy_dnd1_names_vec)
brachy_dnd1_network %v% "importance" = ifelse(brachy_dnd1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                           "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                           "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                           "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                           "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(brachy_dnd1_network, layout = "fruchtermanreingold", weights = "score")
brachy_dnd1_plot <- ggplot(brachy_dnd1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.3) +
  geom_nodes(aes(color = importance), shape = 16, alpha = 0.75) +
  theme_blank() +
  theme(legend.position = "none")

ggsave("brachy_dnd1.tiff", plot = brachy_dnd1_plot, device = "tiff", width = 2, height = 2, units = "in")

## W2691 DND1
W2691_dnd1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/W2691_networks/W2691_slim_goterms_TRAESCS5D01G404600.txt")
W2691_dnd1_network <- network(W2691_dnd1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_dnd1_network <- network.edgelist(x = W2691_dnd1[,1:3], g = W2691_dnd1_network, ignore.eval = FALSE, names.eval = "score")
W2691_dnd1_names_vec <- ggnetwork(W2691_dnd1_network)$vertex.names
W2691_dnd1_names_vec <- unique(W2691_dnd1_names_vec)
W2691_dnd1_network %v% "importance" = ifelse(W2691_dnd1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                         "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                         "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                         "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                         "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(W2691_dnd1_network, layout = "fruchtermanreingold", weights = "score")
W2691_dnd1_plot <- ggplot(W2691_dnd1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.2) +
  geom_nodes(aes(color = importance), alpha = 0.6, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("/Users/evahenningsen/Documents/MPGI_paper/Networks/W2691_dnd1.tiff",  plot = W2691_dnd1_plot, device = "tiff", width = 2, height = 2, units = "in")

## Sr9b DND1
Sr9b_dnd1 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Sr9b_networks/Sr9b_slim_goterms_TRAESCS5D01G404600.txt")
Sr9b_dnd1_network <- network(Sr9b_dnd1[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
Sr9b_dnd1_network <- network.edgelist(x = Sr9b_dnd1[,1:3], g = Sr9b_dnd1_network, ignore.eval = FALSE, names.eval = "score")
Sr9b_dnd1_names_vec <- ggnetwork(Sr9b_dnd1_network)$vertex.names
Sr9b_dnd1_names_vec <- unique(Sr9b_dnd1_names_vec)
Sr9b_dnd1_network %v% "importance" = ifelse(Sr9b_dnd1_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                       "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                       "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                       "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                       "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(Sr9b_dnd1_network, layout = "fruchtermanreingold", weights = "score")
Sr9b_dnd1_plot <- ggplot(Sr9b_dnd1_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.3) +
  geom_nodes(aes(color = importance), alpha = 0.75, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("/Users/evahenningsen/Documents/MPGI_paper/Networks/Sr9b_dnd1.tiff",  plot = Sr9b_dnd1_plot, device = "tiff", width = 2, height = 2, units = "in")

## Brachy DMR6
brachy_dmr6 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Bd21_networks/Bd21_slim_goterms_BDIBD213.1G1026800.txt")
brachy_dmr6_network <- network(brachy_dmr6[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
brachy_dmr6_network <- network.edgelist(x = brachy_dmr6[,1:3], g = brachy_dmr6_network, ignore.eval = FALSE, names.eval = "score")
brachy_dmr6_names_vec <- ggnetwork(brachy_dmr6_network)$vertex.names
brachy_dmr6_names_vec <- unique(brachy_dmr6_names_vec)
brachy_dmr6_network %v% "importance" = ifelse(brachy_dmr6_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                           "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                           "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                           "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                           "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(brachy_dmr6_network, layout = "fruchtermanreingold", weights = "score")
brachy_dmr6_plot <- ggplot(brachy_dmr6_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.3) +
  geom_nodes(aes(color = importance), shape = 16, alpha = 0.75) +
  theme_blank() +
  theme(legend.position = "none")

ggsave("brachy_dmr6.tiff", plot = brachy_dmr6_plot, device = "tiff", width = 2, height = 2, units = "in")


## W2691 DMR6
W2691_dmr6 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/W2691_networks/W2691_slim_goterms_TRAESCS4B01G346900.txt")
W2691_dmr6_network <- network(W2691_dmr6[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
W2691_dmr6_network <- network.edgelist(x = W2691_dmr6[,1:3], g = W2691_dmr6_network, ignore.eval = FALSE, names.eval = "score")
W2691_dmr6_names_vec <- ggnetwork(W2691_dmr6_network)$vertex.names
W2691_dmr6_names_vec <- unique(W2691_dmr6_names_vec)
W2691_dmr6_network %v% "importance" = ifelse(W2691_dmr6_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                         "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                         "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300", 
                                                                         "BDIBD213.3G0030500", "BDIBD213.1G0357000", "BDIBD213.2G0590100", "BDIBD213.2G0641600", 
                                                                         "BDIBD213.1G0110600", "BDIBD213.1G1026800", "BDIBD213.4G0315600"), "yes", "no")
ggnetwork(W2691_dmr6_network, layout = "fruchtermanreingold", weights = "score")
W2691_dmr6_plot <- ggplot(W2691_dmr6_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.25) +
  geom_nodes(aes(color = importance), alpha = 0.65, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("W2691_dmr6.tiff",  plot = W2691_dmr6_plot, device = "tiff", width = 2, height = 2, units = "in")


## Sr9b DMR6
Sr9b_dmr6 <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Networks/Sr9b_networks/Sr9b_slim_goterms_TRAESCS4B01G346900.txt")
Sr9b_dmr6_network <- network(Sr9b_dmr6[,1:3], directed = FALSE, multiple = FALSE, bipartite = FALSE, matrix.type = "edgelist")
#Sr9b_dmr6_network <- network.edgelist(x = Sr9b_dmr6[,1:3], g = Sr9b_dmr6_network, ignore.eval = FALSE, names.eval = "score")
Sr9b_dmr6_names_vec <- ggnetwork(Sr9b_dmr6_network)$vertex.names
Sr9b_dmr6_names_vec <- unique(Sr9b_dmr6_names_vec)
Sr9b_dmr6_network %v% "importance" = ifelse(Sr9b_dmr6_names_vec %in% c("TRAESCS6A01G083200", "TRAESCS6B01G113900", "TRAESCS6D01G077000", "TRAESCS2A01G231900", 
                                                                       "TRAESCS2B01G253500","TRAESCS2D01G236800", "TRAESCS3D01G246000", "TRAESCS5D01G404600", 
                                                                       "TRAESCS4B01G346900", "TRAESCS4D01G341800", "TRAESCS5A01G515600", "TRAESCS4B01G106300"), 
                                            "yes", "no")
ggnetwork(Sr9b_dmr6_network, layout = "fruchtermanreingold", weights = "score")
Sr9b_dmr6_plot <- ggplot(Sr9b_dmr6_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  geom_edges(color = "grey50", alpha = 0.25) +
  geom_nodes(aes(color = importance), alpha = 0.5, shape = 16) +
  theme_blank() +
  theme(legend.position = "none")
ggsave("/Users/evahenningsen/Documents/MPGI_paper/Networks/Sr9b_dmr6.tiff",  plot = Sr9b_dmr6_plot, device = "tiff", width = 2, height = 2, units = "in")



