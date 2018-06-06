## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
library(knitr)
# Leemos los datos
# setwd("C:\\Users\\TOSHIBA\\Documents\\GitHub\\practical1_multivariate_data_analysis\\TRABAJO FINAL")
data_raw <- read.table("diagnosis breast cancer.txt", sep = ",", header = TRUE)
colnames(data_raw) <- c("Id", "Thickness", "Size", "Shape", "Adhesion", "SingleCellSize", "Nuclei", "Chromatin", "Nucleoli", "Mitoses", "Class")

data_raw[which(data_raw[, 11] == 2), 11] <- 0
data_raw[which(data_raw[, 11] == 4), 11] <- 1

for (i in seq(from = 1, to = nrow(data_raw))) {
  if (i <= 367) {
    data_raw$Day[i] <- 1
  } else if (i <= 367 + 70) {
    data_raw$Day[i] <- 2
  } else if (i <= 367 + 70 + 31) {
    data_raw$Day[i] <- 3
  } else if (i <= 367 + 70 + 31 +  17){
    data_raw$Day[i] <- 4
  } else if (i <= 367 + 70 + 31 +  17 + 48){
    data_raw$Day[i] <- 5
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49){
    data_raw$Day[i] <- 6
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49 + 31){
    data_raw$Day[i] <- 7
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49 + 31 + 86){
    data_raw$Day[i] <- 8
  }
}

# Detectar los missing values
missings <- which(data_raw == "?", arr.ind=T) 
library(gdata)

data_raw[, 7] <- as.numeric(gsub("?", round(mean(as.numeric(drop.levels(data_raw[!(rownames(data_raw) %in% missings[, 1]), 7]))), 2), data_raw[, 7], fixed = TRUE))

# Definimos los datos de cada uno de los 8 grupos
day_1 <- data_raw[data_raw$Day == 1, ]
day_2 <- data_raw[data_raw$Day == 2, ]
day_3 <- data_raw[data_raw$Day == 3, ]
day_4 <- data_raw[data_raw$Day == 4, ]
day_5 <- data_raw[data_raw$Day == 5, ]
day_6 <- data_raw[data_raw$Day == 6, ]
day_7 <- data_raw[data_raw$Day == 7, ]
day_8 <- data_raw[data_raw$Day == 8, ]


day_1.n.dup <- subset(day_1, !duplicated(subset(day_1, select = Id)))
day_2.n.dup <- subset(day_2, !duplicated(subset(day_2, select = Id)))
day_3.n.dup <- subset(day_3, !duplicated(subset(day_3, select = Id)))
day_4.n.dup <- subset(day_4, !duplicated(subset(day_4, select = Id)))
day_5.n.dup <- subset(day_5, !duplicated(subset(day_5, select = Id)))
day_6.n.dup <- subset(day_6, !duplicated(subset(day_6, select = Id)))
day_7.n.dup <- subset(day_7, !duplicated(subset(day_7, select = Id)))
day_8.n.dup <- subset(day_8, !duplicated(subset(day_8, select = Id)))

library(plyr)
table.cont <- count(day_1.n.dup[,], "Class")
table.cont <- as.data.frame(table.cont)
colnames(table.cont) <- c("Clase", "Frecuencia")
table.cont$Prop <- round(100*table.cont$Frecuencia/
                           sum(table.cont[1:2, 2]), 2)
table.cont <- table.cont[1:2, ]

table.cont <- kable(table.cont, format = "markdown")

# sum(day_1$Id %in% day_2$Id)
# sum(day_1$Id %in% day_3$Id)
# sum(day_1$Id %in% day_4$Id)
# data_raw$Day
# data_raw$Class==2cor(data_raw)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
benign <- subset(day_1.n.dup[,], Class == 0)
round(apply(benign[, 2:10], 2, mean), 2)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
malign <- subset(day_1.n.dup[,], Class == 1)
round(apply(malign[, 2:10], 2, mean), 2)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
table.cont

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
round(cor(day_1.n.dup[, 2:10]), 2)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
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
# source("./Utils.R")

data <- day_1.n.dup
data.numerical <- day_1.n.dup[, 2:10]
data.dist <- dist(data.numerical, method = "euclid")
data.hierarchical <- hclust(data.dist, method="ward.D2") # or single, complete, or ward.D2
plot(data.hierarchical, hang=-1) #, labels=data[,1])


## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
require(mvnTest)
require(ICSNP)
require(Hotelling)
require(MASS)
require(cclust)
library(cluster)

results<-data.frame()
for(x in c (1,2,3,4,5)){
  result.km.x = kmeans(data.numerical, centers=x, nstart=1000)
  Silh.km.x<-silhouette(result.km.x$cluster,dist(data.numerical))
  
  # result.pam.x <- pam(countries.dist, centers = x, diss = TRUE)
  # Silh.pam.x <- result.pam.x$
  
  results[(x),1]<-x
  results[(x),2]<-result.km.x$totss
  results[(x),3]<-result.km.x$tot.withinss
  
  if (x != 1) {
    PseudoF.km.x<-clustIndex(result.km.x, data.numerical, index="calinski")
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
  # if (x != 1 && x < 4) plot(Silh.km.x)
}
colnames(results)<-c("K","TOTSS","WITHINSS","PseudoF","AvgSilh","DeltaTESS")

results

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
number_of_clusters <- 2
data.kmeans <- kmeans(data.numerical, centers = number_of_clusters)
data$cluster <- data.kmeans$cluster

cor(data$Class, data$cluster)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
data.kmeans$centers

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
pca <- princomp(data.numerical)
pca$sdev^2 / sum(pca$sdev^2)
pca$loadings

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
clusplot(data.numerical, data.kmeans$cluster, 
         col.txt = c("blue", "red")[data.kmeans$cluster], 
         labels = 2, color = TRUE, main = "Kmeans plot", cex = 0)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
g1<-as.matrix(data.numerical[which(data$cluster==1),]) # Take cluster out
g2<-as.matrix(data.numerical[which(data$cluster==2),]) # Take cluster out

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
AD.test(g1, qqplot = FALSE) # can also do royston and henze-zirkler

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
AD.test(g2, qqplot = FALSE) # can also do royston and henze-zirkler

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
# library(Hotelling)
# hotelling.12 <- hotelling.test(g1, g2, perm = TRUE, progBar=F)
# pvalue.perm <- hotelling.12$pval

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
data$cluster <- as.factor(data$cluster)
data.fit <- lda(cluster ~ ., data = data[, c(-1, -11, -12)], na.action = "na.omit", CV = T)
data.model <- lda(cluster ~ ., data = data[, c(-1, -11, -12)], na.action = "na.omit", CV = F)
good_clusters <- sum(diag(prop.table(table(data$cluster, data.fit$class))))
good_classes <- sum(diag(prop.table(table(data$Class, data.fit$class))))

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
data_other_days <- rbind(day_2.n.dup, day_3.n.dup, day_4.n.dup, day_5.n.dup, day_6.n.dup, day_7.n.dup, day_8.n.dup)
good_rate_other_days <- round(sum(diag(prop.table(table(data_other_days$Class + 1, predict(data.model, data_other_days[,c(-1, -11, -12)])$class)))), 2)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
mu1 <- apply(g1, 2, mean)
mu2 <- apply(g2, 2, mean)
s1 <- cov(g1)
s2 <- cov(g2)
n1 <- nrow(g1)
n2 <- nrow(g2)
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)
cov <- ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
library(matlib)
a <- inv(cov) %*% (mu1 - mu2)
F <- function(x) {
  t(a) %*% (x - 0.5 * (mu1 + mu2))
  # predict(data.model, x)$x
}

# log(pi2/pi1)

x1 <- c(t(day_1.n.dup[day_1.n.dup$Id == "1182404", c(-1, -11, -12)]))
x2 <- c(t(day_3.n.dup[day_3.n.dup$Id == "1182404", c(-1, -11, -12)]))
x3 <- c(t(day_5.n.dup[day_5.n.dup$Id == "1182404", c(-1, -11, -12)]))

# x1 <- c(day_1.n.dup[day_1.n.dup$Id == "1182404", c(-1, -11, -12)])
# x2 <- c(day_3.n.dup[day_3.n.dup$Id == "1182404", c(-1, -11, -12)])
# x3 <- c(day_5.n.dup[day_5.n.dup$Id == "1182404", c(-1, -11, -12)])
v1 <- c(F(x1), F(x2), F(x3))

y1 <- c(t(day_1.n.dup[day_1.n.dup$Id == "1276091", c(-1, -11, -12)]))
y2 <- c(t(day_2.n.dup[day_2.n.dup$Id == "1276091", c(-1, -11, -12)]))
y3 <- c(t(day_3.n.dup[day_3.n.dup$Id == "1276091", c(-1, -11, -12)]))

# y1 <- c(day_1.n.dup[day_1.n.dup$Id == "1276091", c(-1, -11, -12)])
# y2 <- c(day_2.n.dup[day_2.n.dup$Id == "1276091", c(-1, -11, -12)])
# y3 <- c(day_3.n.dup[day_3.n.dup$Id == "1276091", c(-1, -11, -12)])
v2 <- c(F(y1), F(y2), F(y3))

plot(v1, ylim = range(c(v1, v2)), type = "l")
lines(v2, col = "blue")

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
purl("FINAL_WORK - BREAST_CANCER.Rmd")

