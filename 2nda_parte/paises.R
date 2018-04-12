countries<-read.table("./2nda_parte/Paises.txt",sep="\t",header=TRUE)
str(countries)

countries2 <- scale(countries[,-1])
countries.D1 = dist(countries2, method = "euclid")

#clustering
require(cluster)
clusterW<-hclust(countries.D1^2, method="ward.D2")

#Agglomerateive clustering, UPGMA
plot(clusterW, hang=-1, labels = countries[,1])


#KMeans
result.km.2 = kmeans(countries2, centers=2, nstart=1000)
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
