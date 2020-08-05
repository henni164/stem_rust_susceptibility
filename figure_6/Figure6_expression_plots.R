# Figure 3 plotting code
## Eva and Cory
## Started 10/30/18

#load necessary libraries
library(ggplot2)
library(scales)
## Data exploration for figure 3
# Directory for expression matrix files
setwd("~/Documents/GitHub/cory/MPGI/figure4/Expression_graphs")

########DND1

## Import network data: BRACHY
Bd21_BDIBD213.1G0110600 <- read.table("Bd21_BDIBD213.1G0110600_network_expression.txt", header = TRUE, sep = "\t")
attach(Bd21_BDIBD213.1G0110600)
Bd21_BDIBD213.1G0110600$geneid <- as.character(Bd21_BDIBD213.1G0110600$geneid)
Bd21_BDIBD213.1G0110600day2 <- as.matrix(rep(2, nrow(Bd21_BDIBD213.1G0110600)))
Bd21_BDIBD213.1G0110600day4 <- as.matrix(rep(4, nrow(Bd21_BDIBD213.1G0110600)))
Bd21_BDIBD213.1G0110600day6 <- as.matrix(rep(6, nrow(Bd21_BDIBD213.1G0110600)))
Bd21_BDIBD213.1G0110600mock <- as.matrix(rep("Mock", nrow(Bd21_BDIBD213.1G0110600)))
Bd21_BDIBD213.1G0110600inoc <- as.matrix(rep("Inoc", nrow(Bd21_BDIBD213.1G0110600)))
Bd21_BDIBD213.1G0110600geneid <- Bd21_BDIBD213.1G0110600$geneid
Bd21_BDIBD213.1G0110600tmp1 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day2, Bd21_BDIBD213.1G0110600mock, Bd21_BDIBD213.1G0110600$X2dpi_mock)
Bd21_BDIBD213.1G0110600tmp2 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day4, Bd21_BDIBD213.1G0110600mock, Bd21_BDIBD213.1G0110600$X4dpi_mock)
Bd21_BDIBD213.1G0110600tmp3 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day6, Bd21_BDIBD213.1G0110600mock, Bd21_BDIBD213.1G0110600$X6dpi_mock)
Bd21_BDIBD213.1G0110600tmp4 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day2, Bd21_BDIBD213.1G0110600inoc, Bd21_BDIBD213.1G0110600$X2dpi_inoc)
Bd21_BDIBD213.1G0110600tmp5 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day4, Bd21_BDIBD213.1G0110600inoc, Bd21_BDIBD213.1G0110600$X4dpi_inoc)
Bd21_BDIBD213.1G0110600tmp6 <- cbind(Bd21_BDIBD213.1G0110600geneid, Bd21_BDIBD213.1G0110600day6, Bd21_BDIBD213.1G0110600inoc, Bd21_BDIBD213.1G0110600$X6dpi_inoc)
Bd21_BDIBD213.1G0110600_ALL <- rbind(Bd21_BDIBD213.1G0110600tmp1, Bd21_BDIBD213.1G0110600tmp2, Bd21_BDIBD213.1G0110600tmp3, Bd21_BDIBD213.1G0110600tmp4, Bd21_BDIBD213.1G0110600tmp5, Bd21_BDIBD213.1G0110600tmp6)
Bd21_BDIBD213.1G0110600_ALL <- as.data.frame(Bd21_BDIBD213.1G0110600_ALL, )
colnames(Bd21_BDIBD213.1G0110600_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Bd21_BDIBD213.1G0110600_ALL)
Bd21_BDIBD213.1G0110600_ALL$Average.Expression <- as.numeric(as.character(Bd21_BDIBD213.1G0110600_ALL$Average.Expression))
Bd21_BDIBD213.1G0110600_ALL$combined <- paste(Bd21_BDIBD213.1G0110600_ALL$Day, Bd21_BDIBD213.1G0110600_ALL$Treatment)
attach(Bd21_BDIBD213.1G0110600_ALL)

Bd21_BDIBD213.1G0110600_ALL$Average.Expression <- (Bd21_BDIBD213.1G0110600_ALL$Average.Expression + 1)
Bd21_BDIBD213.1G0110600_ALL$log2_exp <- log2(Bd21_BDIBD213.1G0110600_ALL$Average.Expression)
#brachy graph DND1
Bd21_BDIBD213.1G0110600_plot <- ggplot(Bd21_BDIBD213.1G0110600_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = (Geneid == "BDIBD213.1G0110600"))) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""),  limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.3G0030500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.1G0357000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.2G0590100"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.2G0641600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.1G0110600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.1G1026800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0110600_ALL, Geneid == "BDIBD213.4G0315600"), aes(x = combined, y = log2_exp), color = "#D0270C")
  


ggsave("DND1_Bd21.pdf", plot = Bd21_BDIBD213.1G0110600_plot, device = "pdf", width = 2, height = 0.75, units = "in")
######### W2691
W2691_TRAESCS5D01G404600 <- read.table("W2691_TRAESCS5D01G404600_network_expression.txt", header = TRUE, sep = "\t")
attach(W2691_TRAESCS5D01G404600)
W2691_TRAESCS5D01G404600$geneid <- as.character(W2691_TRAESCS5D01G404600$geneid)
W2691_TRAESCS5D01G404600day2 <- as.matrix(rep(2, nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600day4 <- as.matrix(rep(4, nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600day6 <- as.matrix(rep(6, nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600mock <- as.matrix(rep("Mock", nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600inoc <- as.matrix(rep("Inoc", nrow(W2691_TRAESCS5D01G404600)))
W2691_TRAESCS5D01G404600geneid <- W2691_TRAESCS5D01G404600$geneid
W2691_TRAESCS5D01G404600tmp1 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day2, W2691_TRAESCS5D01G404600mock, W2691_TRAESCS5D01G404600$X2dpi_mock)
W2691_TRAESCS5D01G404600tmp2 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day4, W2691_TRAESCS5D01G404600mock, W2691_TRAESCS5D01G404600$X4dpi_mock)
W2691_TRAESCS5D01G404600tmp3 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day6, W2691_TRAESCS5D01G404600mock, W2691_TRAESCS5D01G404600$X6dpi_mock)
W2691_TRAESCS5D01G404600tmp4 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day2, W2691_TRAESCS5D01G404600inoc, W2691_TRAESCS5D01G404600$X2dpi_inoc)
W2691_TRAESCS5D01G404600tmp5 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day4, W2691_TRAESCS5D01G404600inoc, W2691_TRAESCS5D01G404600$X4dpi_inoc)
W2691_TRAESCS5D01G404600tmp6 <- cbind(W2691_TRAESCS5D01G404600geneid, W2691_TRAESCS5D01G404600day6, W2691_TRAESCS5D01G404600inoc, W2691_TRAESCS5D01G404600$X6dpi_inoc)
W2691_TRAESCS5D01G404600_ALL <- rbind(W2691_TRAESCS5D01G404600tmp1, W2691_TRAESCS5D01G404600tmp2, W2691_TRAESCS5D01G404600tmp3, W2691_TRAESCS5D01G404600tmp4, W2691_TRAESCS5D01G404600tmp5, W2691_TRAESCS5D01G404600tmp6)
W2691_TRAESCS5D01G404600_ALL <- as.data.frame(W2691_TRAESCS5D01G404600_ALL, )
colnames(W2691_TRAESCS5D01G404600_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(W2691_TRAESCS5D01G404600_ALL)
W2691_TRAESCS5D01G404600_ALL$Average.Expression <- as.numeric(as.character(W2691_TRAESCS5D01G404600_ALL$Average.Expression))
W2691_TRAESCS5D01G404600_ALL$combined <- paste(W2691_TRAESCS5D01G404600_ALL$Day, W2691_TRAESCS5D01G404600_ALL$Treatment)
attach(W2691_TRAESCS5D01G404600_ALL)
W2691_TRAESCS5D01G404600_ALL$Average.Expression <- (W2691_TRAESCS5D01G404600_ALL$Average.Expression+1)
W2691_TRAESCS5D01G404600_ALL$log2_exp <- log2(W2691_TRAESCS5D01G404600_ALL$Average.Expression)
#######W2691 plot

W2691_TRAESCS5D01G404600_plot <- ggplot(W2691_TRAESCS5D01G404600_ALL, aes(x = combined,
                                                                          y = log2_exp,
                                                                          group = Geneid,
                                                                          color = "#636363")) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = expression(log[2](FPKM))) +
  scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title=element_text(size = 1, face = "plain")) +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")

ggsave("DND1_W2691.pdf", plot = W2691_TRAESCS5D01G404600_plot, device = "pdf", width = 2, height = 0.75, units = "in")
#######

######Sr9b

Sr9b_TRAESCS5D01G404600 <- read.table("Sr9b_TRAESCS5D01G404600_network_expression.txt", header = TRUE, sep = "\t")
attach(Sr9b_TRAESCS5D01G404600)
Sr9b_TRAESCS5D01G404600$geneid <- as.character(Sr9b_TRAESCS5D01G404600$geneid)
Sr9b_TRAESCS5D01G404600day2 <- as.matrix(rep(2, nrow(Sr9b_TRAESCS5D01G404600)))
Sr9b_TRAESCS5D01G404600day4 <- as.matrix(rep(4, nrow(Sr9b_TRAESCS5D01G404600)))
Sr9b_TRAESCS5D01G404600day6 <- as.matrix(rep(6, nrow(Sr9b_TRAESCS5D01G404600)))
Sr9b_TRAESCS5D01G404600mock <- as.matrix(rep("Mock", nrow(Sr9b_TRAESCS5D01G404600)))
Sr9b_TRAESCS5D01G404600inoc <- as.matrix(rep("Inoc", nrow(Sr9b_TRAESCS5D01G404600)))
Sr9b_TRAESCS5D01G404600geneid <- Sr9b_TRAESCS5D01G404600$geneid
Sr9b_TRAESCS5D01G404600tmp1 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day2, Sr9b_TRAESCS5D01G404600mock, Sr9b_TRAESCS5D01G404600$X2dpi_mock)
Sr9b_TRAESCS5D01G404600tmp2 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day4, Sr9b_TRAESCS5D01G404600mock, Sr9b_TRAESCS5D01G404600$X4dpi_mock)
Sr9b_TRAESCS5D01G404600tmp3 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day6, Sr9b_TRAESCS5D01G404600mock, Sr9b_TRAESCS5D01G404600$X6dpi_mock)
Sr9b_TRAESCS5D01G404600tmp4 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day2, Sr9b_TRAESCS5D01G404600inoc, Sr9b_TRAESCS5D01G404600$X2dpi_inoc)
Sr9b_TRAESCS5D01G404600tmp5 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day4, Sr9b_TRAESCS5D01G404600inoc, Sr9b_TRAESCS5D01G404600$X4dpi_inoc)
Sr9b_TRAESCS5D01G404600tmp6 <- cbind(Sr9b_TRAESCS5D01G404600geneid, Sr9b_TRAESCS5D01G404600day6, Sr9b_TRAESCS5D01G404600inoc, Sr9b_TRAESCS5D01G404600$X6dpi_inoc)
Sr9b_TRAESCS5D01G404600_ALL <- rbind(Sr9b_TRAESCS5D01G404600tmp1, Sr9b_TRAESCS5D01G404600tmp2, Sr9b_TRAESCS5D01G404600tmp3, Sr9b_TRAESCS5D01G404600tmp4, Sr9b_TRAESCS5D01G404600tmp5, Sr9b_TRAESCS5D01G404600tmp6)
Sr9b_TRAESCS5D01G404600_ALL <- as.data.frame(Sr9b_TRAESCS5D01G404600_ALL, )
colnames(Sr9b_TRAESCS5D01G404600_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Sr9b_TRAESCS5D01G404600_ALL)
Sr9b_TRAESCS5D01G404600_ALL$Average.Expression <- as.numeric(as.character(Sr9b_TRAESCS5D01G404600_ALL$Average.Expression))
Sr9b_TRAESCS5D01G404600_ALL$combined <- paste(Sr9b_TRAESCS5D01G404600_ALL$Day, Sr9b_TRAESCS5D01G404600_ALL$Treatment)
attach(Sr9b_TRAESCS5D01G404600_ALL)

Sr9b_TRAESCS5D01G404600_ALL$Average.Expression <- (Sr9b_TRAESCS5D01G404600_ALL$Average.Expression+1)
Sr9b_TRAESCS5D01G404600_ALL$log2_exp <- log2(Sr9b_TRAESCS5D01G404600_ALL$Average.Expression)
#######Sr9b plot

Sr9b_TRAESCS5D01G404600_plot <- ggplot(Sr9b_TRAESCS5D01G404600_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = (Geneid == "TRAESCS5D01G404600"))) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title=element_text(size = 1, face = "plain")) +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS5D01G404600_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")


ggsave("DND1_Sr9b.pdf", plot = Sr9b_TRAESCS5D01G404600_plot, device = "pdf", width = 2, height =0.75, units = "in")
#######

############### DMR6 #######
###Brachy
Bd21_BDIBD213.1G1026800 <- read.table("Bd21_BDIBD213.1G1026800_network_expression.txt", header = TRUE, sep = "\t")
attach(Bd21_BDIBD213.1G1026800)
Bd21_BDIBD213.1G1026800$geneid <- as.character(Bd21_BDIBD213.1G1026800$geneid)
Bd21_BDIBD213.1G1026800day2 <- as.matrix(rep(2, nrow(Bd21_BDIBD213.1G1026800)))
Bd21_BDIBD213.1G1026800day4 <- as.matrix(rep(4, nrow(Bd21_BDIBD213.1G1026800)))
Bd21_BDIBD213.1G1026800day6 <- as.matrix(rep(6, nrow(Bd21_BDIBD213.1G1026800)))
Bd21_BDIBD213.1G1026800mock <- as.matrix(rep("Mock", nrow(Bd21_BDIBD213.1G1026800)))
Bd21_BDIBD213.1G1026800inoc <- as.matrix(rep("Inoc", nrow(Bd21_BDIBD213.1G1026800)))
Bd21_BDIBD213.1G1026800geneid <- Bd21_BDIBD213.1G1026800$geneid
Bd21_BDIBD213.1G1026800tmp1 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day2, Bd21_BDIBD213.1G1026800mock, Bd21_BDIBD213.1G1026800$X2dpi_mock)
Bd21_BDIBD213.1G1026800tmp2 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day4, Bd21_BDIBD213.1G1026800mock, Bd21_BDIBD213.1G1026800$X4dpi_mock)
Bd21_BDIBD213.1G1026800tmp3 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day6, Bd21_BDIBD213.1G1026800mock, Bd21_BDIBD213.1G1026800$X6dpi_mock)
Bd21_BDIBD213.1G1026800tmp4 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day2, Bd21_BDIBD213.1G1026800inoc, Bd21_BDIBD213.1G1026800$X2dpi_inoc)
Bd21_BDIBD213.1G1026800tmp5 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day4, Bd21_BDIBD213.1G1026800inoc, Bd21_BDIBD213.1G1026800$X4dpi_inoc)
Bd21_BDIBD213.1G1026800tmp6 <- cbind(Bd21_BDIBD213.1G1026800geneid, Bd21_BDIBD213.1G1026800day6, Bd21_BDIBD213.1G1026800inoc, Bd21_BDIBD213.1G1026800$X6dpi_inoc)
Bd21_BDIBD213.1G1026800_ALL <- rbind(Bd21_BDIBD213.1G1026800tmp1, Bd21_BDIBD213.1G1026800tmp2, Bd21_BDIBD213.1G1026800tmp3, Bd21_BDIBD213.1G1026800tmp4, Bd21_BDIBD213.1G1026800tmp5, Bd21_BDIBD213.1G1026800tmp6)
Bd21_BDIBD213.1G1026800_ALL <- as.data.frame(Bd21_BDIBD213.1G1026800_ALL, )
colnames(Bd21_BDIBD213.1G1026800_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Bd21_BDIBD213.1G1026800_ALL)
Bd21_BDIBD213.1G1026800_ALL$Average.Expression <- as.numeric(as.character(Bd21_BDIBD213.1G1026800_ALL$Average.Expression))
Bd21_BDIBD213.1G1026800_ALL$combined <- paste(Bd21_BDIBD213.1G1026800_ALL$Day, Bd21_BDIBD213.1G1026800_ALL$Treatment)
attach(Bd21_BDIBD213.1G1026800_ALL)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("2 Mock", "2-M", Bd21_BDIBD213.1G1026800_ALL$combined)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("2 Inoc", "2-I", Bd21_BDIBD213.1G1026800_ALL$combined)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("4 Mock", "4-M", Bd21_BDIBD213.1G1026800_ALL$combined)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("4 Inoc", "4-I", Bd21_BDIBD213.1G1026800_ALL$combined)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("6 Mock", "6-M", Bd21_BDIBD213.1G1026800_ALL$combined)
Bd21_BDIBD213.1G1026800_ALL$combined <- gsub("6 Inoc", "6-I", Bd21_BDIBD213.1G1026800_ALL$combined)

Bd21_BDIBD213.1G1026800_ALL$Average.Expression <- (Bd21_BDIBD213.1G1026800_ALL$Average.Expression+1)
Bd21_BDIBD213.1G1026800_ALL$log2_exp <- log2(Bd21_BDIBD213.1G1026800_ALL$Average.Expression)

##################

##Brachy graph
Bd21_BDIBD213.1G1026800_plot <- ggplot(Bd21_BDIBD213.1G1026800_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = (Geneid == "BDIBD213.1G1026800"))) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.3G0030500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.1G0357000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.2G0590100"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.2G0641600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.1G0110600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.1G1026800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G1026800_ALL, Geneid == "BDIBD213.4G0315600"), aes(x = combined, y = log2_exp), color = "#D0270C")


ggsave("DMR6_Bd21.pdf", plot = Bd21_BDIBD213.1G1026800_plot, device = "pdf", width = 2, height = 0.75, units = "in")
######

### W2691
W2691_TRAESCS4B01G346900 <- read.table("W2691_TRAESCS4B01G346900_network_expression.txt", header = TRUE, sep = "\t")
attach(W2691_TRAESCS4B01G346900)
W2691_TRAESCS4B01G346900$geneid <- as.character(W2691_TRAESCS4B01G346900$geneid)
W2691_TRAESCS4B01G346900day2 <- as.matrix(rep(2, nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900day4 <- as.matrix(rep(4, nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900day6 <- as.matrix(rep(6, nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900mock <- as.matrix(rep("Mock", nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900inoc <- as.matrix(rep("Inoc", nrow(W2691_TRAESCS4B01G346900)))
W2691_TRAESCS4B01G346900geneid <- W2691_TRAESCS4B01G346900$geneid
W2691_TRAESCS4B01G346900tmp1 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day2, W2691_TRAESCS4B01G346900mock, W2691_TRAESCS4B01G346900$X2dpi_mock)
W2691_TRAESCS4B01G346900tmp2 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day4, W2691_TRAESCS4B01G346900mock, W2691_TRAESCS4B01G346900$X4dpi_mock)
W2691_TRAESCS4B01G346900tmp3 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day6, W2691_TRAESCS4B01G346900mock, W2691_TRAESCS4B01G346900$X6dpi_mock)
W2691_TRAESCS4B01G346900tmp4 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day2, W2691_TRAESCS4B01G346900inoc, W2691_TRAESCS4B01G346900$X2dpi_inoc)
W2691_TRAESCS4B01G346900tmp5 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day4, W2691_TRAESCS4B01G346900inoc, W2691_TRAESCS4B01G346900$X4dpi_inoc)
W2691_TRAESCS4B01G346900tmp6 <- cbind(W2691_TRAESCS4B01G346900geneid, W2691_TRAESCS4B01G346900day6, W2691_TRAESCS4B01G346900inoc, W2691_TRAESCS4B01G346900$X6dpi_inoc)
W2691_TRAESCS4B01G346900_ALL <- rbind(W2691_TRAESCS4B01G346900tmp1, W2691_TRAESCS4B01G346900tmp2, W2691_TRAESCS4B01G346900tmp3, W2691_TRAESCS4B01G346900tmp4, W2691_TRAESCS4B01G346900tmp5, W2691_TRAESCS4B01G346900tmp6)
W2691_TRAESCS4B01G346900_ALL <- as.data.frame(W2691_TRAESCS4B01G346900_ALL, )
colnames(W2691_TRAESCS4B01G346900_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(W2691_TRAESCS4B01G346900_ALL)
W2691_TRAESCS4B01G346900_ALL$Average.Expression <- as.numeric(as.character(W2691_TRAESCS4B01G346900_ALL$Average.Expression))
W2691_TRAESCS4B01G346900_ALL$combined <- paste(W2691_TRAESCS4B01G346900_ALL$Day, W2691_TRAESCS4B01G346900_ALL$Treatment)
attach(W2691_TRAESCS4B01G346900_ALL)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("2 Mock", "2-M", W2691_TRAESCS4B01G346900_ALL$combined)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("2 Inoc", "2-I", W2691_TRAESCS4B01G346900_ALL$combined)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("4 Mock", "4-M", W2691_TRAESCS4B01G346900_ALL$combined)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("4 Inoc", "4-I", W2691_TRAESCS4B01G346900_ALL$combined)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("6 Mock", "6-M", W2691_TRAESCS4B01G346900_ALL$combined)
W2691_TRAESCS4B01G346900_ALL$combined <- gsub("6 Inoc", "6-I", W2691_TRAESCS4B01G346900_ALL$combined)

W2691_TRAESCS4B01G346900_ALL$Average.Expression <- (W2691_TRAESCS4B01G346900_ALL$Average.Expression+1)
W2691_TRAESCS4B01G346900_ALL$log2_exp <- log2(W2691_TRAESCS4B01G346900_ALL$Average.Expression)

########


### W2691 plot
W2691_TRAESCS4B01G346900_plot <- ggplot(W2691_TRAESCS4B01G346900_ALL, aes(x = combined,
                                                                          y = log2_exp,
                                                                          group = Geneid,
                                                                          color = "#636363")) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = expression(log[2](FPKM))) +
  scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")

ggsave("DMR6_W2691.pdf", plot = W2691_TRAESCS4B01G346900_plot, device = "pdf", width = 2, height = 0.78, units = "in")


#### Sr9b
Sr9b_TRAESCS4B01G346900 <- read.table("Sr9b_TRAESCS4B01G346900_network_expression.txt", header = TRUE, sep = "\t")
attach(Sr9b_TRAESCS4B01G346900)
Sr9b_TRAESCS4B01G346900$geneid <- as.character(Sr9b_TRAESCS4B01G346900$geneid)
Sr9b_TRAESCS4B01G346900day2 <- as.matrix(rep(2, nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900day4 <- as.matrix(rep(4, nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900day6 <- as.matrix(rep(6, nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900mock <- as.matrix(rep("Mock", nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900inoc <- as.matrix(rep("Inoc", nrow(Sr9b_TRAESCS4B01G346900)))
Sr9b_TRAESCS4B01G346900geneid <- Sr9b_TRAESCS4B01G346900$geneid
Sr9b_TRAESCS4B01G346900tmp1 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day2, Sr9b_TRAESCS4B01G346900mock, Sr9b_TRAESCS4B01G346900$X2dpi_mock)
Sr9b_TRAESCS4B01G346900tmp2 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day4, Sr9b_TRAESCS4B01G346900mock, Sr9b_TRAESCS4B01G346900$X4dpi_mock)
Sr9b_TRAESCS4B01G346900tmp3 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day6, Sr9b_TRAESCS4B01G346900mock, Sr9b_TRAESCS4B01G346900$X6dpi_mock)
Sr9b_TRAESCS4B01G346900tmp4 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day2, Sr9b_TRAESCS4B01G346900inoc, Sr9b_TRAESCS4B01G346900$X2dpi_inoc)
Sr9b_TRAESCS4B01G346900tmp5 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day4, Sr9b_TRAESCS4B01G346900inoc, Sr9b_TRAESCS4B01G346900$X4dpi_inoc)
Sr9b_TRAESCS4B01G346900tmp6 <- cbind(Sr9b_TRAESCS4B01G346900geneid, Sr9b_TRAESCS4B01G346900day6, Sr9b_TRAESCS4B01G346900inoc, Sr9b_TRAESCS4B01G346900$X6dpi_inoc)
Sr9b_TRAESCS4B01G346900_ALL <- rbind(Sr9b_TRAESCS4B01G346900tmp1, Sr9b_TRAESCS4B01G346900tmp2, Sr9b_TRAESCS4B01G346900tmp3, Sr9b_TRAESCS4B01G346900tmp4, Sr9b_TRAESCS4B01G346900tmp5, Sr9b_TRAESCS4B01G346900tmp6)
Sr9b_TRAESCS4B01G346900_ALL <- as.data.frame(Sr9b_TRAESCS4B01G346900_ALL, )
colnames(Sr9b_TRAESCS4B01G346900_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Sr9b_TRAESCS4B01G346900_ALL)
Sr9b_TRAESCS4B01G346900_ALL$Average.Expression <- as.numeric(as.character(Sr9b_TRAESCS4B01G346900_ALL$Average.Expression))
Sr9b_TRAESCS4B01G346900_ALL$combined <- paste(Sr9b_TRAESCS4B01G346900_ALL$Day, Sr9b_TRAESCS4B01G346900_ALL$Treatment)
attach(Sr9b_TRAESCS4B01G346900_ALL)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("2 Mock", "2-M", Sr9b_TRAESCS4B01G346900_ALL$combined)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("2 Inoc", "2-I", Sr9b_TRAESCS4B01G346900_ALL$combined)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("4 Mock", "4-M", Sr9b_TRAESCS4B01G346900_ALL$combined)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("4 Inoc", "4-I", Sr9b_TRAESCS4B01G346900_ALL$combined)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("6 Mock", "6-M", Sr9b_TRAESCS4B01G346900_ALL$combined)
Sr9b_TRAESCS4B01G346900_ALL$combined <- gsub("6 Inoc", "6-I", Sr9b_TRAESCS4B01G346900_ALL$combined)

Sr9b_TRAESCS4B01G346900_ALL$Average.Expression <- (Sr9b_TRAESCS4B01G346900_ALL$Average.Expression+1)
Sr9b_TRAESCS4B01G346900_ALL$log2_exp <- log2(Sr9b_TRAESCS4B01G346900_ALL$Average.Expression)

########

##Sr9b plot
Sr9b_TRAESCS4B01G346900_plot <- ggplot(Sr9b_TRAESCS4B01G346900_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = "#636363")) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""), limits = c(0, 10)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS4B01G346900_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")

ggsave("DMR6_Sr9b.pdf", plot = Sr9b_TRAESCS4B01G346900_plot, device = "pdf", width = 2, height = 0.75, units = "in")
#######

############### VAD1 ##############

## Brachy
Bd21_BDIBD213.1G0357000 <- read.table("Bd21_BDIBD213.1G0357000_network_expression.txt", header = TRUE, sep = "\t")
attach(Bd21_BDIBD213.1G0357000)
Bd21_BDIBD213.1G0357000$geneid <- as.character(Bd21_BDIBD213.1G0357000$geneid)
Bd21_BDIBD213.1G0357000day2 <- as.matrix(rep(2, nrow(Bd21_BDIBD213.1G0357000)))
Bd21_BDIBD213.1G0357000day4 <- as.matrix(rep(4, nrow(Bd21_BDIBD213.1G0357000)))
Bd21_BDIBD213.1G0357000day6 <- as.matrix(rep(6, nrow(Bd21_BDIBD213.1G0357000)))
Bd21_BDIBD213.1G0357000mock <- as.matrix(rep("Mock", nrow(Bd21_BDIBD213.1G0357000)))
Bd21_BDIBD213.1G0357000inoc <- as.matrix(rep("Inoc", nrow(Bd21_BDIBD213.1G0357000)))
Bd21_BDIBD213.1G0357000geneid <- Bd21_BDIBD213.1G0357000$geneid
Bd21_BDIBD213.1G0357000tmp1 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day2, Bd21_BDIBD213.1G0357000mock, Bd21_BDIBD213.1G0357000$X2dpi_mock)
Bd21_BDIBD213.1G0357000tmp2 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day4, Bd21_BDIBD213.1G0357000mock, Bd21_BDIBD213.1G0357000$X4dpi_mock)
Bd21_BDIBD213.1G0357000tmp3 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day6, Bd21_BDIBD213.1G0357000mock, Bd21_BDIBD213.1G0357000$X6dpi_mock)
Bd21_BDIBD213.1G0357000tmp4 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day2, Bd21_BDIBD213.1G0357000inoc, Bd21_BDIBD213.1G0357000$X2dpi_inoc)
Bd21_BDIBD213.1G0357000tmp5 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day4, Bd21_BDIBD213.1G0357000inoc, Bd21_BDIBD213.1G0357000$X4dpi_inoc)
Bd21_BDIBD213.1G0357000tmp6 <- cbind(Bd21_BDIBD213.1G0357000geneid, Bd21_BDIBD213.1G0357000day6, Bd21_BDIBD213.1G0357000inoc, Bd21_BDIBD213.1G0357000$X6dpi_inoc)
Bd21_BDIBD213.1G0357000_ALL <- rbind(Bd21_BDIBD213.1G0357000tmp1, Bd21_BDIBD213.1G0357000tmp2, Bd21_BDIBD213.1G0357000tmp3, Bd21_BDIBD213.1G0357000tmp4, Bd21_BDIBD213.1G0357000tmp5, Bd21_BDIBD213.1G0357000tmp6)
Bd21_BDIBD213.1G0357000_ALL <- as.data.frame(Bd21_BDIBD213.1G0357000_ALL, )
colnames(Bd21_BDIBD213.1G0357000_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Bd21_BDIBD213.1G0357000_ALL)
Bd21_BDIBD213.1G0357000_ALL$Average.Expression <- as.numeric(as.character(Bd21_BDIBD213.1G0357000_ALL$Average.Expression))
Bd21_BDIBD213.1G0357000_ALL$combined <- paste(Bd21_BDIBD213.1G0357000_ALL$Day, Bd21_BDIBD213.1G0357000_ALL$Treatment)
attach(Bd21_BDIBD213.1G0357000_ALL)

Bd21_BDIBD213.1G0357000_ALL$Average.Expression <- (Bd21_BDIBD213.1G0357000_ALL$Average.Expression+1)
Bd21_BDIBD213.1G0357000_ALL$log2_exp <- log2(Bd21_BDIBD213.1G0357000_ALL$Average.Expression)

########################

#brachy plot
Bd21_BDIBD213.1G0357000_plot <- ggplot(Bd21_BDIBD213.1G0357000_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = (Geneid == "BDIBD213.1G0357000"))) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.3G0030500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.1G0357000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.2G0590100"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.2G0641600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.1G0110600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.1G1026800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Bd21_BDIBD213.1G0357000_ALL, Geneid == "BDIBD213.4G0315600"), aes(x = combined, y = log2_exp), color = "#D0270C")


ggsave("VAD1_Bd21.pdf", plot = Bd21_BDIBD213.1G0357000_plot, device = "pdf", width = 2, height = 0.75, units = "in")

######### W2691
W2691_TRAESCS2A01G231900 <- read.table("W2691_TRAESCS2A01G231900_network_expression.txt", header = TRUE, sep = "\t")
attach(W2691_TRAESCS2A01G231900)
W2691_TRAESCS2A01G231900$geneid <- as.character(W2691_TRAESCS2A01G231900$geneid)
W2691_TRAESCS2A01G231900day2 <- as.matrix(rep(2, nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900day4 <- as.matrix(rep(4, nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900day6 <- as.matrix(rep(6, nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900mock <- as.matrix(rep("Mock", nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900inoc <- as.matrix(rep("Inoc", nrow(W2691_TRAESCS2A01G231900)))
W2691_TRAESCS2A01G231900geneid <- W2691_TRAESCS2A01G231900$geneid
W2691_TRAESCS2A01G231900tmp1 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day2, W2691_TRAESCS2A01G231900mock, W2691_TRAESCS2A01G231900$X2dpi_mock)
W2691_TRAESCS2A01G231900tmp2 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day4, W2691_TRAESCS2A01G231900mock, W2691_TRAESCS2A01G231900$X4dpi_mock)
W2691_TRAESCS2A01G231900tmp3 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day6, W2691_TRAESCS2A01G231900mock, W2691_TRAESCS2A01G231900$X6dpi_mock)
W2691_TRAESCS2A01G231900tmp4 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day2, W2691_TRAESCS2A01G231900inoc, W2691_TRAESCS2A01G231900$X2dpi_inoc)
W2691_TRAESCS2A01G231900tmp5 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day4, W2691_TRAESCS2A01G231900inoc, W2691_TRAESCS2A01G231900$X4dpi_inoc)
W2691_TRAESCS2A01G231900tmp6 <- cbind(W2691_TRAESCS2A01G231900geneid, W2691_TRAESCS2A01G231900day6, W2691_TRAESCS2A01G231900inoc, W2691_TRAESCS2A01G231900$X6dpi_inoc)
W2691_TRAESCS2A01G231900_ALL <- rbind(W2691_TRAESCS2A01G231900tmp1, W2691_TRAESCS2A01G231900tmp2, W2691_TRAESCS2A01G231900tmp3, W2691_TRAESCS2A01G231900tmp4, W2691_TRAESCS2A01G231900tmp5, W2691_TRAESCS2A01G231900tmp6)
W2691_TRAESCS2A01G231900_ALL <- as.data.frame(W2691_TRAESCS2A01G231900_ALL, )
colnames(W2691_TRAESCS2A01G231900_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(W2691_TRAESCS2A01G231900_ALL)
W2691_TRAESCS2A01G231900_ALL$Average.Expression <- as.numeric(as.character(W2691_TRAESCS2A01G231900_ALL$Average.Expression))
W2691_TRAESCS2A01G231900_ALL$combined <- paste(W2691_TRAESCS2A01G231900_ALL$Day, W2691_TRAESCS2A01G231900_ALL$Treatment)
attach(W2691_TRAESCS2A01G231900_ALL)

W2691_TRAESCS2A01G231900_ALL$Average.Expression <- (W2691_TRAESCS2A01G231900_ALL$Average.Expression+1)
W2691_TRAESCS2A01G231900_ALL$log2_exp <- log2(W2691_TRAESCS2A01G231900_ALL$Average.Expression)

#########


####W2691 plot
W2691_TRAESCS2A01G231900_plot <- ggplot(W2691_TRAESCS2A01G231900_ALL, aes(x = combined,
                                                                          y = log2_exp,
                                                                          group = Geneid,
                                                                          color = "#636363")) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = expression(log[2](FPKM))) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3), limits = c(0, 10)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_text(color = "black", size = 8),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(W2691_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")

ggsave("VAD1_W2691.pdf", plot = W2691_TRAESCS2A01G231900_plot, device = "pdf", width = 2, height = 0.75, units = "in")
#######

#### Sr9b
Sr9b_TRAESCS2A01G231900 <- read.table("Sr9b_TRAESCS2A01G231900_network_expression.txt", header = TRUE, sep = "\t")
attach(Sr9b_TRAESCS2A01G231900)
Sr9b_TRAESCS2A01G231900$geneid <- as.character(Sr9b_TRAESCS2A01G231900$geneid)
Sr9b_TRAESCS2A01G231900day2 <- as.matrix(rep(2, nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900day4 <- as.matrix(rep(4, nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900day6 <- as.matrix(rep(6, nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900mock <- as.matrix(rep("Mock", nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900inoc <- as.matrix(rep("Inoc", nrow(Sr9b_TRAESCS2A01G231900)))
Sr9b_TRAESCS2A01G231900geneid <- Sr9b_TRAESCS2A01G231900$geneid
Sr9b_TRAESCS2A01G231900tmp1 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day2, Sr9b_TRAESCS2A01G231900mock, Sr9b_TRAESCS2A01G231900$X2dpi_mock)
Sr9b_TRAESCS2A01G231900tmp2 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day4, Sr9b_TRAESCS2A01G231900mock, Sr9b_TRAESCS2A01G231900$X4dpi_mock)
Sr9b_TRAESCS2A01G231900tmp3 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day6, Sr9b_TRAESCS2A01G231900mock, Sr9b_TRAESCS2A01G231900$X6dpi_mock)
Sr9b_TRAESCS2A01G231900tmp4 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day2, Sr9b_TRAESCS2A01G231900inoc, Sr9b_TRAESCS2A01G231900$X2dpi_inoc)
Sr9b_TRAESCS2A01G231900tmp5 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day4, Sr9b_TRAESCS2A01G231900inoc, Sr9b_TRAESCS2A01G231900$X4dpi_inoc)
Sr9b_TRAESCS2A01G231900tmp6 <- cbind(Sr9b_TRAESCS2A01G231900geneid, Sr9b_TRAESCS2A01G231900day6, Sr9b_TRAESCS2A01G231900inoc, Sr9b_TRAESCS2A01G231900$X6dpi_inoc)
Sr9b_TRAESCS2A01G231900_ALL <- rbind(Sr9b_TRAESCS2A01G231900tmp1, Sr9b_TRAESCS2A01G231900tmp2, Sr9b_TRAESCS2A01G231900tmp3, Sr9b_TRAESCS2A01G231900tmp4, Sr9b_TRAESCS2A01G231900tmp5, Sr9b_TRAESCS2A01G231900tmp6)
Sr9b_TRAESCS2A01G231900_ALL <- as.data.frame(Sr9b_TRAESCS2A01G231900_ALL, )
colnames(Sr9b_TRAESCS2A01G231900_ALL) <- c("Geneid", "Day", "Treatment", "Average.Expression")
attach(Sr9b_TRAESCS2A01G231900_ALL)
Sr9b_TRAESCS2A01G231900_ALL$Average.Expression <- as.numeric(as.character(Sr9b_TRAESCS2A01G231900_ALL$Average.Expression))
Sr9b_TRAESCS2A01G231900_ALL$combined <- paste(Sr9b_TRAESCS2A01G231900_ALL$Day, Sr9b_TRAESCS2A01G231900_ALL$Treatment)
attach(Sr9b_TRAESCS2A01G231900_ALL)


Sr9b_TRAESCS2A01G231900_ALL$Average.Expression <- (Sr9b_TRAESCS2A01G231900_ALL$Average.Expression+1)
Sr9b_TRAESCS2A01G231900_ALL$log2_exp <- log2(Sr9b_TRAESCS2A01G231900_ALL$Average.Expression)
#########

##Sr9b plot
Sr9b_TRAESCS2A01G231900_plot <- ggplot(Sr9b_TRAESCS2A01G231900_ALL, aes(x = combined,
                                                                        y = log2_exp,
                                                                        group = Geneid,
                                                                        color = "#636363")) +
  geom_line(size = 0.1) +
  labs(x = "",
       y = "") +
  scale_y_continuous(breaks = c(0, 5, 10), labels = c("", "", ""), limits = c(0, 10)) +
  scale_color_manual(values = c("#636363", "#D0270C")) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6A01G083200"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6B01G113900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS6D01G077000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2A01G231900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2B01G253500"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS2D01G236800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS3D01G246000"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS5D01G404600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4B01G346900"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4D01G341800"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS5A01G515600"), aes(x = combined, y = log2_exp), color = "#D0270C") +
  geom_line(data = subset(Sr9b_TRAESCS2A01G231900_ALL, Geneid == "TRAESCS4B01G106300"), aes(x = combined, y = log2_exp), color = "#D0270C")

ggsave("VAD1_Sr9b_log2.pdf", plot = Sr9b_TRAESCS2A01G231900_plot, device = "pdf", width = 2, height = 0.75, units = "in")
#######