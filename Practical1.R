setwd("/home/eudald/Downloads/Practical1/")

source("Utils.R")

pov_table <- as.data.frame(read.table("PovertyStudy.dat", header = T))

pov_numeric <- as.data.frame(pov_table[,1:6])

# Get mean
pov_mean <- as.data.frame(t(apply(pov_numeric, MARGIN  = 2, FUN = mean)))

# Centered Matrix
pov_centered <- pov_numeric - pov_mean[rep(seq_len(nrow(pov_mean)), each=nrow(pov_numeric)),]
rownames(pov_centered) <- pov_table$Country
head(pov_centered, 6)

# Testing I didn't mess up
apply(pov_centered, MARGIN = 2, FUN = mean)

#Get Var-Cov
pov_cov <- cov(pov_numeric)

# Get Cor
pov_cor <- cor(pov_numeric)

# Get Standardized
pov_sd <- as.data.frame(t(sqrt(diag(pov_cov))))
pov_standardized <- pov_centered / pov_sd[rep(seq_len(nrow(pov_sd)), each=nrow(pov_centered)),]

# Get Var-Cov of standardized
pov_st_cov <- cov(pov_standardized)
rownames(pov_st_cov) <- pov_table$Country
head(pov_st_cov, 6)

# Eucl dist
pov_eucl_dist <- as.matrix(dist(pov_numeric, method = "euclidean"))
rownames(pov_eucl_dist) <- pov_table$Country
colnames(pov_eucl_dist) <- pov_table$Country
head(pov_eucl_dist[,1:6], 6)

# Mahala dist
pov_mahala_dist <- mahalanobis.dist(pov_numeric)
rownames(pov_mahala_dist) <- pov_table$Country
colnames(pov_mahala_dist) <- pov_table$Country
head(pov_mahala_dist[,1:6], 6)

# Eucl dist standardized data
pov_eucl_dist_standardized <- as.matrix(dist(pov_standardized, method = "euclidean"))
rownames(pov_eucl_dist_standardized) <- pov_table$Country
colnames(pov_eucl_dist_standardized) <- pov_table$Country
head(pov_eucl_dist_standardized[,1:6], 6)

# Max Indices
cbind(t(which(pov_eucl_dist == max(pov_eucl_dist), arr.ind = TRUE)),t(which(pov_mahala_dist == max(pov_mahala_dist), arr.ind = TRUE)))

