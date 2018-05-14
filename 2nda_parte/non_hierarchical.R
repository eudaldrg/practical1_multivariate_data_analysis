# countries<-read.table("./2nda_parte/Paises.txt",sep="\t",header=TRUE)
countries<-read.table("./2nda_parte/FANGA_TAUFA_del_tab.txt",sep="\t",header=TRUE)

str(countries)

countries2 <- scale(countries[,-1])
countries.D1 = dist(countries2, method = "euclid")

#clustering
require(cluster)
clusterW<-hclust(countries.D1^2, method="ward.D2")

#Agglomerateive clustering, UPGMA
plot(clusterW, hang=-1, labels = countries[,1])


#KMeans
result.km.2 <- kmeans(countries2, centers=2, nstart=1000)
names(result.km.2)
result.km.2$size
result.km.2$centers
result.km.2$cluster
result.km.2$totss # The total error sum of squares (TESS)
result.km.2$tot.withinss # The total error sum of squares within groups

#Compute different cluester validity
require(cclust)
PseudoF.km.2<-clustIndex(result.km.2, countries2, index="calinski")
PseudoF.km.2


# Overall mean silhouette
require(cluster)
Silh.km.2<-silhouette(result.km.2$cluster, dist(countries2))
Overall.Silh.km.2<-mean(Silh.km.2[,3])
Overall.Silh.km.2

results<-data.frame()
for(x in c (2,3,4,5)){
  result.km.x =kmeans(countries2, centers=x, nstart=1000)
  PseudoF.km.x<-clustIndex(result.km.x, countries2, index="calinski")
  Silh.km.x<-silhouette(result.km.x$cluster,dist(countries2))
  Overall.Silh.km.x<-mean(Silh.km.x[,3])
  results[(x-1),1]<-x
  results[(x-1),2]<-result.km.x$totss
  results[(x-1),3]<-result.km.x$tot.withinss
  results[(x-1),4]<-PseudoF.km.x
  results[(x-1),5]<-Overall.Silh.km.x
}

colnames(results)<-c("K","TOTSS","WITHINSS","PseudoF","Silh")
results

# Compute K-means for a range of values of K
require(vegan)
result.cascadeKM = cascadeKM(countries2, inf.gr=2, sup.gr=5, iter = 1000, criterion ="calinski")
attributes(result.cascadeKM)

result.cascadeKM$partition
result.cascadeKM$results
require(cluster)
clusplot(countries2, result.km.2$cluster, main = "Kmeans plot, k = 2", color = TRUE, labels=2)


# Clusters
aggregate(countries2, by=list(cluster=result.km.2$cluster), mean)

# Overall Silhouettes
k=5
#define number max of the groups (k)
Overall.Silh=rep(0,k)
#initialization silhouette statistic vector
for(i in 2:k){
  out.i= kmeans(countries2, centers=i, nstart=1000)
  # kmeans
  Silh.km.i=silhouette(out.i$cluster,dist(countries2))
  #silhouette stat
  Overall.Silh[(i-1)]=mean(Silh.km.i[,3])
  #overall silhouette stat
}
Overall.Silh

# Partitioning Around Medoids (PAM) (kmeans with non-euclid)
require(vegan)
countries.MAH <- vegdist(countries[,-1], method = "mahalanobis")
require(cluster)
pam.res.2 <- pam(countries.MAH, 2, diss = T)

pam.res.2$medoids
pam.res.2$clustering
pam.res.2$silinfo$avg.width

mds.pam.res.2 <- cmdscale(as.dist(countries.MAH), eig=TRUE)
plot(mds.pam.res.2$points[,1], mds.pam.res.2$points[,2], main="PAM(k=2) Mahalanobis distance", xlab="Axis 1", ylab="Axis 2", col=pam.res.2$cluster, pch=19)
text(mds.pam.res.2$points[,1], mds.pam.res.2$points[,2], labels= countries[,1], pos=1, cex=0.5, offset=0)

# Summary
Axis1<-round(cor(mds.pam.res.2$points[,1], countries[,2:11]),3)
Axis1

Axis2<-round(cor(mds.pam.res.2$points[,2], countries[,2:11]),3)
Axis2

require(cluster)
mds.fuzzy.res.2 <- fanny(countries.MAH, 2, diss = TRUE, memb.exp=1.2)

attributes(mds.fuzzy.res.2)

mds.fuzzy.res.2$membership
