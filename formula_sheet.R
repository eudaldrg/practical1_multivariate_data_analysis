require(stats) # ?dist 
require(vegan) # ?vegdist, ?cascadeKM
require(ade4)  # ?dist.binary
require(ape)
require(cclust) # ?clusplot (better explained at ?clusplot.default)
require(cluster)
require(clusterSim) # lda and qda or predict or something
require(mvnTest) # Multivariate normality test
require(factoextra)
require(MASS)
require(biotools) # Covariance Homogeneity test
require(ICSNP) # T2
require(Hotelling) # T2 Permutation Test
source("./Utils.R")

data <- read.table("./2nda_parte/ecosystems_windows.txt",sep="\t",header=T)

# Take other stuff away
data.numerical <- data

### Dissimilarities

## Profiles
# Bray-Curtis
data.dist <- vegdist(data.numerical, method="bray")
# Hellinger
data.scaled = decostand(data.numerical, "hel")
data.dist = dist(data.scaled)

## Quantitative data
# Euclidean (Scale if needed)
data.scaled <- scale(data.numerical)
data.dist <- dist(data.scaled, method = "euclid")

# Mahalanobis
data.dist <- vegdist(data.numerical, method="mahalanobis")
# data.dist <- mahalanobis.dist(data.numerical)

## Independence
# Chi squared TODO!

### Group Structure

## Method
# Hierarchical (UPGMA/Ward2)
data.hierarchical <- hclust(data.dist, method="average") # or single, complete, or ward.D2
plot(data.hierarchical, hang=-1) #, labels=data[,1])
data.cophenetic = cophenetic(clusterAV)
plot(data.cophenetic, data.dist)

# Kmeans/Pam (it's still kmeans, but R calls it this way)
number_of_clusters <- 2
data.kmeans <- kmeans(data.scaled, centers = number_of_clusters)
data$cluster <- data.kmeans$cluster

# Pam
# Can also be called with diss = F and a matrix of observations, but then we need to specify metric
# only supports euclidean and manhattan
# Only needs silhouettes. They are already computed
data.pam <- pam(countries.dist, number_of_clusters, diss = TRUE)
# set the clustering

# Fuzzy (non hard boundaries, everything is diffuminated)
data.fuzzy <- fanny(countries.MAH, 2, diss = TRUE, memb.exp=1.2)

## Stop criterion (for kmeans)
# Silhouettes
# Delta TESS
# Pseudo F
results<-data.frame()
for(x in c (1,2,3,4,5,6,7,8)){
  result.km.x = kmeans(data.scaled, centers=x, nstart=1000)
  Silh.km.x<-silhouette(result.km.x$cluster,dist(data.scaled))
  
  result.pam.x <- pam(countries.dist, centers = x, diss = TRUE)
  Silh.pam.x <- result.pam.x$
  
  results[(x),1]<-x
  results[(x),2]<-result.km.x$totss
  results[(x),3]<-result.km.x$tot.withinss
  
  if (x != 1) {
    PseudoF.km.x<-clustIndex(result.km.x, data.scaled, index="calinski")
    results[(x),4]<-PseudoF.km.x
  } else {
    results[x,4] <- NA
  }
  
  if (x == 1) {
    results[(x),5]<- NA
    results[(x),6]<- NA
  } else {
    Overall.Silh.km.x<-mean(Silh.km.x[,3])
    results[(x),5]<-Overall.Silh.km.x
    results[(x),6]<- (results[(x-1),3] - results[(x),3]) / results[(x-1),3]
  }
  if (x != 1 && x < 4) plot(Silh.km.x)
}
colnames(results)<-c("K","TOTSS","WITHINSS","PseudoF","AvgSilh","DeltaTESS")

### Representatives description

### 2D Representation

# Both PCA and MDS can use clusplot with diss = T or F
clusplot(data.scaled, data.kmeans$cluster, 
         col.txt = c("blue", "red")[data.kmeans$cluster], 
         labels = 2, color = TRUE, main = "Kmeans plot", cex = 0)

# If you need a fancier thing, compute it yourself:
# PCA
pca <- princomp(data.scaled)
# plot() weights of the n components you want. add arrows of the loadings you need

# MDS
mds = cmdscale(data.D2, 3, eig=TRUE)
plot(mds$points[,1], mds$points[,2],asp=1, xlab="Axis 1", ylab="Axis 2")
names = rownames(data)
text(mds$points[,1], mds$points[,2], labels= names, pos=2, cex=0.5, offset=0.15)

# FA
# CA

### Group differentiation
g1<-as.matrix(data[which(data$cluster==1),-8]) # Take cluster out
g2<-as.matrix(data[which(data$cluster==2),-8]) # Take cluster out

## Multivariate Normal case H0 not normal mv
mvn(g1, mvnTest = "mardia") # can also do royston and henze-zirkler
AD.test(g1, qqplot = T) # hztest or roystontest for others

# Covariance matrix homogeneity
boxM(data[,-8], data[,8])
# or manova
manova<-manova(cbind(param_1, param_2, param_3)~cluster, data = data) #relate everything to clust
summary(efinsa.manova, test = "Pillai")

# Means homogeneity
HotellingsT2(g1, g2, test = "chi")

## Non-normal
# Permutation test
Hotelling.perm12<-hotelling.test(g1, g2, perm = TRUE, progBar=F)
Hotelling.perm12$pval

### Discriminant analysis

## Prediction

# LDA/QDA (just change the function)
data.fit <- lda(cluster ~ ., data = data, na.action = "na.omit", CV = T) # Linear and crossvalidated
data.model <- lda(cluster ~ ., data = data, na.action = "na.omit", CV = F)

# Logistic (only found code for 2 groups)
data.model <- glm(cluster ~ ., family="binomial", data=dfx)

# Actual prediction
element <- data.frame(param_a = 251, param_b = 241)
predict(element, data.model)

## Quality
## Cross-validation
sum(diag(prop.table(table(data$cluster, data.fit$class)))) # how good it is