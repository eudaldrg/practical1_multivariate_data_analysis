require(MASS)
require(ggplot2)
require(vegan)
require(cluster)
require(factoextra)
require(clusterSim)

df<-read.table("./2nda_parte/efinsa.txt", sep = "\t", header=T)
data.hel = decostand(df[,-1], "hel")
cluster.H.AV <- hclust(d.hel, method = "average")
cluster.H.W <- hclust(d.hel, method = "ward.D2")
par(mfrow=c(1,2))
plot(cluster.H.AV, hang = -1, cex = 0.7)
plot(cluster.H.W, hang = -1, cex = 0.7)


d.bray <- vegdist(df[,-1], method = "bray")
clusterAV <- hclust(d.bray, method = "average")
clusterW <- hclust(d.bray, method = "ward.D2")
par(mfrow=c(1,2))
plot(clusterAV, hang = -1, cex = 0.7)
plot(clusterW, hang = -1, cex = 0.7)

# PAM
pam.d.bray.3 <- pam(d.bray, 3, diss=T)

# Stadisticos TESS deltaTess PseudoF Silueta

#TESS
TESS <- fviz_nbclust(as.matrix(d.bray), pam, method = "wss")
TESS
TESS$data

left <- TESS$data$y[1:(length(TESS$data$y)-1)]
right <- TESS$data$y[2:(length(TESS$data$y))]
deltaTESS <- (left - right) / left * 100

# Silhouette
pam.d.bray.3$silinfo$avg.width
plot(pam.d.bray.3)

# Pseudo F
results<-data.frame()
for(x in c(2,3,4,5,6,7)){
  pam.d.bray.x <-pam(d.bray, x, diss = TRUE)
  pam.d.bray.x.PseudoF<-index.G1(df[,-1],pam.d.bray.x$cluster, centrotypes="centroids")
  pam.d.bray.x.silh <-index.S(d.bray,pam.d.bray.x$cluster)
  results[(x-1),1]<-x
  results[(x-1),2]<-pam.d.bray.x.PseudoF
  results[(x-1),3]<-pam.d.bray.x.silh
}
colnames(results)<-c("K", "PseudoF", "Silh")
results

#Analysis of the medioids
pam.d.bray.3
pam.d.bray.3$medoids
df[pam.d.bray.3$medoids,]

# Cluster analysis (how consistent they are)
plot(pam.d.bray.3)

# Plot result

pam.d.bray.3$clustering
sum(pam.d.bray.3$clustering == 1)
sum(pam.d.bray.3$clustering == 2)
sum(pam.d.bray.3$clustering == 3)

mds.pam.d.bray.3 <- cmdscale(as.dist(d.bray), eig = T)
plot(mds.pam.d.bray.3$points[,1], mds.pam.d.bray.3$points[,2], col=pam.d.bray.3$cluster, pch = pam.d.bray.3$cluster)
text(mds.pam.d.bray.3$points[,1], mds.pam.d.bray.3$points[,2], labels = df[,1], pos = 1, cex = 0.5, offset=0.3)

# Interpretation of the axis
Axis1 <- round(cor(mds.pam.d.bray.3$points[,1], df[,-1]), 3)
Axis1

Axis2 <- round(cor(mds.pam.d.bray.3$points[,2], df[,-1]), 3)
Axis2
