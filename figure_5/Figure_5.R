#BiocManager::install("topGO")

library(topGO)
library(GO.db)
library(data.table)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(gtable)

#QC
# wheat_go <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/3_IWGSC_v1.1_HC.cleaned.gaf", sep = "\t", header = TRUE)
# brachy_go <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/Bd21_v1.2_annotation_GO.txt", sep = "\t", header = TRUE)
# wheat_slim_go <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/wheat_1.1_gslim_unique.gaf", sep = "\t", header = FALSE)
# brachy_slim_go <- read.delim("/Users/evahenningsen/Documents/MPGI_paper/brachy1.2_gslim_unique.gaf", sep = "\t", header = FALSE)
# 
# colnames(wheat_slim_go)[5] <- "term_accession"
# 
# ontology_table <- as.data.frame(Ontology(GOTERM))
# ontology_table$term_accession <- rownames(ontology_table)
# 
# wheat_go_ont <- left_join(x = wheat_go, y = ontology_table, by = "term_accession")
# wheat_slim_ont <- left_join(x = wheat_slim_go, y = ontology_table, by = "term_accession")
# 
# write.table(wheat_go_ont, file = "/Users/evahenningsen/Documents/MPGI_paper/wheat_go_with_ontology.txt", sep = "\t")
# write.table(wheat_slim_ont, file = "/Users/evahenningsen/Documents/MPGI_paper/wheat_slim_with_ontology.txt", sep = "\t")
# 
# colnames(brachy_slim_go)[5] <- "GO"
# 
# ontology_table <- as.data.frame(Ontology(GOTERM))
# ontology_table$GO <- rownames(ontology_table)
# 
# brachy_go_ont <- left_join(x = brachy_go, y = ontology_table, by = "GO")
# brachy_slim_ont <- left_join(x = brachy_slim_go, y = ontology_table, by = "GO")
# write.table(brachy_go_ont, file = "/Users/evahenningsen/Documents/MPGI_paper/brachy_go_with_ontology.txt", sep = "\t")
# write.table(brachy_slim_ont, file = "/Users/evahenningsen/Documents/MPGI_paper/brachy_slim_with_ontology.txt", sep = "\t")
# automated creation
goterms <- as.data.frame(Term(GOTERM))
setDT(goterms, keep.rownames = TRUE)[]
colnames(goterms) <- c("GOID", "GOTERM")
brachyGENESLIM2GO <- readMappings(file = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/brachy_goslims_space.txt")
brachyGENESLIMUniverse <- names(brachyGENESLIM2GO)
wheatGENESLIM2GO <- readMappings(file = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/wheat_goslims_space.txt")
wheatslimUniverse <- names(wheatGENESLIM2GO)

#brachypodium automation MF
brachy_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/brachy_networks", full.names = TRUE, recursive = FALSE)
brachy_files <- as.vector(brachy_files)

brachy_dat <- lapply(brachy_files, FUN = read.delim, sep = "\t", header = FALSE)

brachy_slim <- lapply(brachy_dat, FUN = function(x) {
  unique(as.character(x$V1))
})

brachy_genelist <- lapply(brachy_slim, function(y) {
  factor(as.integer(brachyGENESLIMUniverse %in% y))
})

for (i in 1:5) {
  names(brachy_genelist[[i]]) <- brachyGENESLIMUniverse
}

brachy_slim_results_list <- lapply(brachy_genelist, function(a) {
  brachy_GOdata <- new("topGOdata", description = "brachy_slim", ontology = "MF", allGenes = a, annot = annFUN.gene2GO, gene2GO = brachyGENESLIM2GO)
  brachy_results <- runTest(brachy_GOdata, algorithm = "weight01", statistic = "fisher")
  brachy_prop <- termStat(brachy_GOdata)
  setDT(brachy_prop, keep.rownames = TRUE)
  colnames(brachy_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  brachy_results_score <- score(brachy_results)
  brachy_results_score <- as.data.frame(brachy_results_score)
  setDT(brachy_results_score, keep.rownames = TRUE)[]
  colnames(brachy_results_score) <- c("GOID", "FISHERSCORE")
  brachy_combined_results <- left_join(brachy_results_score, brachy_prop, by = "GOID")
  brachy_combined_results
})

brachy_results_final <- lapply(brachy_slim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("Bd21-3", 5)
cluster_list <- c(1848,272,35,4,51)
domain_list <- rep("MF", 5)

for(k in 1:5) {
  
  brachy_results_final[[k]]$genotype <- rep(genotype_list[[k]], length(brachy_results_final[[k]]$GOID))
  brachy_results_final[[k]]$cluster <- rep(cluster_list[[k]], length(brachy_results_final[[k]]$GOID))
  brachy_results_final[[k]]$domain <- rep(domain_list[[k]], length(brachy_results_final[[k]]$GOID))
  
}

#W2691 automation
W2691_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/W2691_networks", full.names = TRUE, recursive = FALSE)
W2691_files <- as.vector(W2691_files)

W2691_dat <- lapply(W2691_files, FUN = read.delim, sep = "\t", header = FALSE)

W2691_slim <- lapply(W2691_dat, FUN = function(x) {
  unique(as.character(x$V1))
})

W2691_genelist <- lapply(W2691_slim, function(y) {
  factor(as.integer(wheatslimUniverse %in% y))
})

for (l in 1:8) {
  names(W2691_genelist[[l]]) <- wheatslimUniverse
}

W2691_slim_results_list <- lapply(W2691_genelist, function(a) {
  W2691_GOdata <- new("topGOdata", description = "W2691_slim", ontology = "MF", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  W2691_results <- runTest(W2691_GOdata, algorithm = "weight01", statistic = "fisher")
  W2691_prop <- termStat(W2691_GOdata)
  setDT(W2691_prop, keep.rownames = TRUE)
  colnames(W2691_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  W2691_results_score <- score(W2691_results)
  W2691_results_score <- as.data.frame(W2691_results_score)
  setDT(W2691_results_score, keep.rownames = TRUE)[]
  colnames(W2691_results_score) <- c("GOID", "FISHERSCORE")
  W2691_combined_results <- left_join(W2691_results_score, W2691_prop, by = "GOID")
  W2691_combined_results
})

W2691_results_final <- lapply(W2691_slim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("W2691", 8)
cluster_list <- c(0,11,178,3,4,5,60,8)
domain_list <- rep("MF", 8)

for(m in 1:8) {
  
  W2691_results_final[[m]]$genotype <- rep(genotype_list[[m]], length(W2691_results_final[[m]]$GOID))
  W2691_results_final[[m]]$cluster <- rep(cluster_list[[m]], length(W2691_results_final[[m]]$GOID))
  W2691_results_final[[m]]$domain <- rep(domain_list[[m]], length(W2691_results_final[[m]]$GOID))
  
}

## Sr9b automation

Sr9b_files <- list.files(path = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/Sr9b_networks", full.names = TRUE, recursive = FALSE)
Sr9b_files <- as.vector(Sr9b_files)

Sr9b_dat <- lapply(Sr9b_files, FUN = read.delim, sep = "\t", header = FALSE)

Sr9b_slim <- lapply(Sr9b_dat, FUN = function(x) {
  unique(as.character(x$V1))
})

Sr9b_genelist <- lapply(Sr9b_slim, function(y) {
  factor(as.integer(wheatslimUniverse %in% y))
})

for (l in 1:4) {
  names(Sr9b_genelist[[l]]) <- wheatslimUniverse
}

Sr9b_slim_results_list <- lapply(Sr9b_genelist, function(a) {
  Sr9b_GOdata <- new("topGOdata", description = "Sr9b_slim", ontology = "MF", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  Sr9b_results <- runTest(Sr9b_GOdata, algorithm = "weight01", statistic = "fisher")
  Sr9b_prop <- termStat(Sr9b_GOdata)
  setDT(Sr9b_prop, keep.rownames = TRUE)
  colnames(Sr9b_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  Sr9b_results_score <- score(Sr9b_results)
  Sr9b_results_score <- as.data.frame(Sr9b_results_score)
  setDT(Sr9b_results_score, keep.rownames = TRUE)[]
  colnames(Sr9b_results_score) <- c("GOID", "FISHERSCORE")
  Sr9b_combined_results <- left_join(Sr9b_results_score, Sr9b_prop, by = "GOID")
  Sr9b_combined_results
})

Sr9b_results_final <- lapply(Sr9b_slim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("Sr9b", 4)
cluster_list <- c(0,122,2772,4)
domain_list <- rep("MF", 4)

for(m in 1:4) {
  
  Sr9b_results_final[[m]]$genotype <- rep(genotype_list[[m]], length(Sr9b_results_final[[m]]$GOID))
  Sr9b_results_final[[m]]$cluster <- rep(cluster_list[[m]], length(Sr9b_results_final[[m]]$GOID))
  Sr9b_results_final[[m]]$domain <- rep(domain_list[[m]], length(Sr9b_results_final[[m]]$GOID))
  
}

#brachypodium automation CC

brachy_CC_slim_results_list <- lapply(brachy_genelist, function(a) {
  brachy_CC_GOdata <- new("topGOdata", description = "brachy_CC_slim", ontology = "CC", allGenes = a, annot = annFUN.gene2GO, gene2GO = brachyGENESLIM2GO)
  brachy_CC_results <- runTest(brachy_CC_GOdata, algorithm = "weight01", statistic = "fisher")
  brachy_CC_prop <- termStat(brachy_CC_GOdata)
  setDT(brachy_CC_prop, keep.rownames = TRUE)
  colnames(brachy_CC_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  brachy_CC_results_score <- score(brachy_CC_results)
  brachy_CC_results_score <- as.data.frame(brachy_CC_results_score)
  setDT(brachy_CC_results_score, keep.rownames = TRUE)[]
  colnames(brachy_CC_results_score) <- c("GOID", "FISHERSCORE")
  brachy_CC_combined_results <- left_join(brachy_CC_results_score, brachy_CC_prop, by = "GOID")
  brachy_CC_combined_results
})

brachy_CC_results_final <- lapply(brachy_CC_slim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("Bd21-3", 5)
cluster_list <- c(1848,272,35,4,51)
domain_list <- rep("CC", 5)

for(k in 1:5) {
  
  brachy_CC_results_final[[k]]$genotype <- rep(genotype_list[[k]], length(brachy_CC_results_final[[k]]$GOID))
  brachy_CC_results_final[[k]]$cluster <- rep(cluster_list[[k]], length(brachy_CC_results_final[[k]]$GOID))
  brachy_CC_results_final[[k]]$domain <- rep(domain_list[[k]], length(brachy_CC_results_final[[k]]$GOID))
  
}

#brachypodium automation BP: No results so commented out

# brachy_BP_slim_results_list <- lapply(brachy_genelist, function(a) {
#   brachy_BP_GOdata <- new("topGOdata", description = "brachy_BP_slim", ontology = "BP", allGenes = a, annot = annFUN.gene2GO, gene2GO = brachyGENESLIM2GO)
#   brachy_BP_results <- runTest(brachy_BP_GOdata, algorithm = "classic", statistic = "fisher")
#   brachy_BP_results_score <- score(brachy_BP_results)
#   brachy_BP_results_score <- as.data.frame(brachy_BP_results_score)
#   setDT(brachy_BP_results_score, keep.rownames = TRUE)[]
#   colnames(brachy_BP_results_score) <- c("GOID", "FISHERSCORE")
#   brachy_BP_results_score
# })
# 
# brachy_BP_results_final <- lapply(brachy_BP_slim_results_list, function(b) {
#   b[FISHERSCORE < 0.01]
# })
# 
# genotype_list <- rep("Bd21-3", 6)
# day_list <- c(2,2,4,4,6,6)
# direction_list <-c("down","up","down","up","down","up")
# domain_list <- rep("BP", 6)
# 
# for(k in 1:6) {
# 
#   brachy_BP_results_final[[k]]$genotype <- rep(genotype_list[[k]], length(brachy_BP_results_final[[k]]$GOID))
#   brachy_BP_results_final[[k]]$day <- rep(day_list[[k]], length(brachy_BP_results_final[[k]]$GOID))
#   brachy_BP_results_final[[k]]$direction <- rep(direction_list[[k]], length(brachy_BP_results_final[[k]]$GOID))
#   brachy_BP_results_final[[k]]$domain <- rep(domain_list[[k]], length(brachy_BP_results_final[[k]]$GOID))
# 
# }

## W2691 CC
W2691_CCslim_results_list <- lapply(W2691_genelist, function(a) {
  W2691_CCGOdata <- new("topGOdata", description = "W2691_CCslim", ontology = "CC", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  W2691_CCresults <- runTest(W2691_CCGOdata, algorithm = "weight01", statistic = "fisher")
  W2691_CC_prop <- termStat(W2691_CCGOdata)
  setDT(W2691_CC_prop, keep.rownames = TRUE)
  colnames(W2691_CC_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  W2691_CCresults_score <- score(W2691_CCresults)
  W2691_CCresults_score <- as.data.frame(W2691_CCresults_score)
  setDT(W2691_CCresults_score, keep.rownames = TRUE)[]
  colnames(W2691_CCresults_score) <- c("GOID", "FISHERSCORE")
  W2691_CC_combined_results <- left_join(W2691_CCresults_score, W2691_CC_prop, by = "GOID")
  W2691_CC_combined_results
})

W2691_CCresults_final <- lapply(W2691_CCslim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("W2691", 8)
cluster_list <- c(0,11,178,3,4,5,60,8)
domain_list <- rep("CC", 8)

for(m in 1:8) {
  
  W2691_CCresults_final[[m]]$genotype <- rep(genotype_list[[m]], length(W2691_CCresults_final[[m]]$GOID))
  W2691_CCresults_final[[m]]$cluster <- rep(cluster_list[[m]], length(W2691_CCresults_final[[m]]$GOID))
  W2691_CCresults_final[[m]]$domain <- rep(domain_list[[m]], length(W2691_CCresults_final[[m]]$GOID))
  
}

## W2691 BP
W2691_BPslim_results_list <- lapply(W2691_genelist, function(a) {
  W2691_BPGOdata <- new("topGOdata", description = "W2691_BPslim", ontology = "BP", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  W2691_BPresults <- runTest(W2691_BPGOdata, algorithm = "weight01", statistic = "fisher")
  W2691_BP_prop <- termStat(W2691_BPGOdata)
  setDT(W2691_BP_prop, keep.rownames = TRUE)
  colnames(W2691_BP_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  W2691_BPresults_score <- score(W2691_BPresults)
  W2691_BPresults_score <- as.data.frame(W2691_BPresults_score)
  setDT(W2691_BPresults_score, keep.rownames = TRUE)[]
  colnames(W2691_BPresults_score) <- c("GOID", "FISHERSCORE")
  W2691_BP_combined_results <- left_join(W2691_BPresults_score, W2691_BP_prop, by = "GOID")
  W2691_BP_combined_results
})

W2691_BPresults_final <- lapply(W2691_BPslim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("W2691", 8)
cluster_list <- c(0,11,178,3,4,5,60,8)
domain_list <- rep("BP", 8)

for(m in 1:8) {
  
  W2691_BPresults_final[[m]]$genotype <- rep(genotype_list[[m]], length(W2691_BPresults_final[[m]]$GOID))
  W2691_BPresults_final[[m]]$cluster <- rep(cluster_list[[m]], length(W2691_BPresults_final[[m]]$GOID))
  W2691_BPresults_final[[m]]$domain <- rep(domain_list[[m]], length(W2691_BPresults_final[[m]]$GOID))
  
}

## Sr9b CC
Sr9b_CCslim_results_list <- lapply(Sr9b_genelist, function(a) {
  Sr9b_CCGOdata <- new("topGOdata", description = "Sr9b_CCslim", ontology = "CC", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  Sr9b_CCresults <- runTest(Sr9b_CCGOdata, algorithm = "weight01", statistic = "fisher")
  Sr9b_CC_prop <- termStat(Sr9b_CCGOdata)
  setDT(Sr9b_CC_prop, keep.rownames = TRUE)
  colnames(Sr9b_CC_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  Sr9b_CCresults_score <- score(Sr9b_CCresults)
  Sr9b_CCresults_score <- as.data.frame(Sr9b_CCresults_score)
  setDT(Sr9b_CCresults_score, keep.rownames = TRUE)[]
  colnames(Sr9b_CCresults_score) <- c("GOID", "FISHERSCORE")
  Sr9b_CC_combined_results <- left_join(Sr9b_CCresults_score, Sr9b_CC_prop, by = "GOID")
  Sr9b_CC_combined_results
})

Sr9b_CCresults_final <- lapply(Sr9b_CCslim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("Sr9b", 4)
cluster_list <- c(0,122,2772,4)
domain_list <- rep("CC", 4)

for(m in 1:4) {
  
  Sr9b_CCresults_final[[m]]$genotype <- rep(genotype_list[[m]], length(Sr9b_CCresults_final[[m]]$GOID))
  Sr9b_CCresults_final[[m]]$cluster <- rep(cluster_list[[m]], length(Sr9b_CCresults_final[[m]]$GOID))
  Sr9b_CCresults_final[[m]]$domain <- rep(domain_list[[m]], length(Sr9b_CCresults_final[[m]]$GOID))
  
}

## Sr9b BP
Sr9b_BPslim_results_list <- lapply(Sr9b_genelist, function(a) {
  Sr9b_BPGOdata <- new("topGOdata", description = "Sr9b_BPslim", ontology = "BP", allGenes = a, annot = annFUN.gene2GO, gene2GO = wheatGENESLIM2GO)
  Sr9b_BPresults <- runTest(Sr9b_BPGOdata, algorithm = "weight01", statistic = "fisher")
  Sr9b_BP_prop <- termStat(Sr9b_BPGOdata)
  setDT(Sr9b_BP_prop, keep.rownames = TRUE)
  colnames(Sr9b_BP_prop) <- c("GOID", "Annotated", "Significant", "Expected")
  Sr9b_BPresults_score <- score(Sr9b_BPresults)
  Sr9b_BPresults_score <- as.data.frame(Sr9b_BPresults_score)
  setDT(Sr9b_BPresults_score, keep.rownames = TRUE)[]
  colnames(Sr9b_BPresults_score) <- c("GOID", "FISHERSCORE")
  Sr9b_BP_combined_results <- left_join(Sr9b_BPresults_score, Sr9b_BP_prop, by = "GOID")
  Sr9b_BP_combined_results
})

Sr9b_BPresults_final <- lapply(Sr9b_BPslim_results_list, function(b) {
  b[FISHERSCORE < 0.01]
})

genotype_list <- rep("Sr9b", 4)
cluster_list <- c(0,122,2772,4)
domain_list <- rep("BP", 4)

for(m in 1:4) {
  
  Sr9b_BPresults_final[[m]]$genotype <- rep(genotype_list[[m]], length(Sr9b_BPresults_final[[m]]$GOID))
  Sr9b_BPresults_final[[m]]$cluster <- rep(cluster_list[[m]], length(Sr9b_BPresults_final[[m]]$GOID))
  Sr9b_BPresults_final[[m]]$domain <- rep(domain_list[[m]], length(Sr9b_BPresults_final[[m]]$GOID))
  
}

all_brachy_CC <- bind_rows(brachy_CC_results_final)

all_brachy_MF <- bind_rows(brachy_results_final)

all_W2691_MF <- bind_rows(W2691_results_final)

all_W2691_CC <- bind_rows(W2691_CCresults_final)

all_W2691_BP <- bind_rows(W2691_BPresults_final)

all_Sr9b_MF <- bind_rows(Sr9b_results_final)

all_Sr9b_CC <- bind_rows(Sr9b_CCresults_final)

all_Sr9b_BP <- bind_rows(Sr9b_BPresults_final)

all_results <- rbind(all_brachy_MF, all_brachy_CC, all_W2691_MF, all_W2691_CC, all_W2691_BP, all_Sr9b_MF, all_Sr9b_CC, all_Sr9b_BP)

all_results_w_go_descriptions <- left_join(x = all_results, y = goterms, by = "GOID")

s_gene_info <- read.delim("/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/clusters_and_sgenes.txt", sep = "\t", header = TRUE)

all_results_w_go_descriptions <- left_join(x = all_results_w_go_descriptions, y = s_gene_info, by = c("genotype", "cluster"))


all_results_w_go_descriptions$genotype <- as.factor(all_results_w_go_descriptions$genotype)
all_results_w_go_descriptions$genotype <- factor(all_results_w_go_descriptions$genotype, levels=c("W2691", "Sr9b", "Bd21-3"))
#levels(all_results_w_go_descriptions$genotype) = c("W2691"=expression(paste("W2691")), "Sr9b"=expression(paste(italic("Sr9b"))), "Bd21-3"=expression(paste("Bd21-3")))
all_results_w_go_descriptions$GOTERM <- as.factor(all_results_w_go_descriptions$GOTERM)

all_results_w_go_descriptions$OoverE <- all_results_w_go_descriptions$Significant/all_results_w_go_descriptions$Expected
all_results_w_go_descriptions$OoverALL <- all_results_w_go_descriptions$Significant/all_results_w_go_descriptions$Annotated

all_results_w_go_descriptions$gene <- as.factor(all_results_w_go_descriptions$gene)
levels(all_results_w_go_descriptions$gene) <- c("AGD2"=expression(paste(italic("AGD2"))), "BI-1"=expression(paste(italic("BI-1"))), "DMR6"=expression(paste(italic("DMR6"))), "DND1"=expression(paste(italic("DND1"))), "FAH1"=expression(paste(italic("FAH1"))), "IBR3"=expression(paste(italic("IBR3"))), "VAD1"=expression(paste(italic("VAD1"))), "WRKY25"=expression(paste(italic("WRKY25"))))
all_results_w_go_descriptions$cluster <- as.factor(all_results_w_go_descriptions$cluster)

levels(all_results_w_go_descriptions$GOTERM)[4] <- gsub("DNA-binding transcription factor activity", "DNA-\nbinding\n transcription\n factor\n activity", levels(all_results_w_go_descriptions$GOTERM)[4])
levels(all_results_w_go_descriptions$GOTERM)[15] <- gsub("structural molecule activity", "structural\n molecule\n activity", levels(all_results_w_go_descriptions$GOTERM)[15])
levels(all_results_w_go_descriptions$GOTERM)[14] <- gsub("signaling receptor binding", "signaling\n receptor\n binding", levels(all_results_w_go_descriptions$GOTERM)[14])
levels(all_results_w_go_descriptions$GOTERM)[5] <- gsub("endoplasmic reticulum", "endoplasmic\n reticulum", levels(all_results_w_go_descriptions$GOTERM)[5])
levels(all_results_w_go_descriptions$GOTERM)[1] <- gsub("catalytic activity", "catalytic\n activity", levels(all_results_w_go_descriptions$GOTERM)[1])
levels(all_results_w_go_descriptions$GOTERM)[2] <- gsub("cell wall", "cell\nwall", levels(all_results_w_go_descriptions$GOTERM)[2])
levels(all_results_w_go_descriptions$GOTERM)[3] <- gsub("chromatin binding", "chromatin\nbinding", levels(all_results_w_go_descriptions$GOTERM)[3])
levels(all_results_w_go_descriptions$GOTERM)[7] <- gsub("Golgi apparatus", "Golgi\napparatus", levels(all_results_w_go_descriptions$GOTERM)[7])
levels(all_results_w_go_descriptions$GOTERM)[8] <- gsub("motor activity", "motor\nactivity", levels(all_results_w_go_descriptions$GOTERM)[8])
levels(all_results_w_go_descriptions$GOTERM)[9] <- gsub("nuclear envelope", "nuclear\nenvelope", levels(all_results_w_go_descriptions$GOTERM)[9])
levels(all_results_w_go_descriptions$GOTERM)[12] <- gsub("protein binding", "protein\nbinding", levels(all_results_w_go_descriptions$GOTERM)[12])
levels(all_results_w_go_descriptions$GOTERM)[16] <- gsub("transporter activity", "transporter\nactivity", levels(all_results_w_go_descriptions$GOTERM)[16])
levels(all_results_w_go_descriptions$GOTERM)[13] <- gsub("RNA binding", "RNA\nbinding", levels(all_results_w_go_descriptions$GOTERM)[13])

## For next time: fix font size and scale for printing
topgo_figure2 <- ggplot(all_results_w_go_descriptions, aes(x = genotype, y=GOTERM)) +
  geom_tile(width=1, height=1) +
  labs(x = NULL) +
  scale_x_discrete(labels = c("W2691", expression(paste(italic("Sr9b"))), "Bd21-3")) + 
  #scale_fill_gradient2(low = "white", mid = "blue", high = "red", space = "Lab", name = "Significant:Annotated", breaks = c(0, 0.05, 0.1 ,0.15), labels = c("0", "0.05", "0.1", "0.15"), limits = c(0,0.15)) + 
  facet_grid(domain ~ gene, scales = "free_y", labeller = label_parsed) +
  theme(
    axis.text.x = element_text(size = 10, color="black", angle = 270),
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size=8, color="black", hjust = 1),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    #panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "transparent", color = "white", size = 1),
    #strip.text = element_text(face = "bold", size = 10, color = "black"),
    strip.text = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box = "horizontal",
    #legend.box.background = element_rect(colour = "black"),
    #legend.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    #legend.box.background = element_rect(fill = "transparent"),
    legend.box.background=element_blank(),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10),
    strip.text.x=element_text(size=10),
    legend.key.height = unit(0.1, "in"))

figure2_plot_more = ggplot_gtable(ggplot_build(topgo_figure2))
gtable_show_layout(figure2_plot_more)
figure2_plot_more$heights[8] <- 0.7*figure2_plot_more$heights[8]
as_ggplot(figure2_plot_more)

ggsave(filename = "/Users/evahenningsen/Documents/GitHub/stem_rust_susceptibility/figure_5/Fig_5_GOSLIM.tiff", plot = figure2_plot_more, device = "tiff", width= 10, height=7.5, units = "in")

