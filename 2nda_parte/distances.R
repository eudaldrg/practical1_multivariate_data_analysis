require(stats)

x <-matrix (rnorm (100), nrow = 5)
dim(x)
dist(x)
class(dist(x))

dist(x, diag = T)
dist(x, upper = T)
m <- as.matrix(dist(x))


# Actual practice
require(vegan)
require(ade4)

data<-read.table("./2nda_parte/FANGA_TAUFA_del_tab.txt",sep="\t",header=TRUE)
#gastropod species
str(data)
head(data)

# Bray curtis
data.D2<-vegdist(data[,-1], method="bray")
class(data.D2)

outBC = cmdscale(data.D2, 3, eig=TRUE)
plot(outBC$points[,1], outBC$points[,2], main="Bray-Curtis distance",asp=1, xlab="Axis 1", ylab="Axis 2")
names = rownames(data)
text(outBC$points[,1], outBC$points[,2], labels= names, pos=2, cex=0.5, offset=0.15)

# Hellinger
data.hel = decostand(data[,-1], "hel")
data.DHell = dist(data.hel)

outDHell = cmdscale(data.DHell, 3, eig=TRUE)
plot(outDHell$points[,1], outDHell$points[,2], main="Hellinger distance",asp=1,xlab="Axis 1", ylab="Axis 2")
names = rownames(data)
text(outDHell$points[,1], outDHell$points[,2], labels= names, pos=2, cex=0.5, offset=0.15)

# PCA
data.out = prcomp(data[,-1], scale=FALSE)
names(data.out)
data.out$sdev
data.out$rotation #variable loadings (columns are eigenvectors) 
summary(data.out)


#non scale biplot
plot(data.out, main="Scree plot", xlab="Principal Components")
biplot(data.out, main="Non standardized data", cex=0.7)

#scale biplot
data.out = prcomp(data[,-1], scale=TRUE)
biplot(data.out, main="Standardized data", cex=0.7)
