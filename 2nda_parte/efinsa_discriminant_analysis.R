require(MASS)
require(ggplot2)
require(vegan)
require(cluster)
require(factoextra)
require(clusterSim)

require(mvnTest) # Multivariate normality test
require(biotools) # Covariance Homogeneity test
require(ICSNP) # T2
require(Hotelling) # T2 Permutation Test

df<-read.table("./2nda_parte/efinsa.txt", sep = "\t", header=T)

# Cluster
d.bray <- vegdist(df[,-1], method = "bray")
pam.d.bray.3 <- pam(d.bray, 3, diss=T)
pam.d.bray.3$clustering

# adding cluster variables
df$cluster <- as.factor(pam.d.bray.3$clustering)
str(df)

#Linear Discriminant Ananlysis (LDA)

# re-substitution, this is bad, because you will never have data
# to test your model against, as you use all the data in the model,
# so you end up using info about an individual to 
dat <- df[,-1]
fit <- lda(cluster ~ ., data = dat, CV = F)
fit

# Ideally, do a extreme cross validation leaving out only one 
# every time (leave-one-out cross-validation), if not regular cross validation

# Assess the accuracy of the prediction percent correct for each category
ct <- table(dat$cluster, predict(fit,dat[,-11])$class)
ct

diag(prop.table(ct, 1))

# Total percent correct
sum(diag(prop.table(ct)))

# Prediction
usu1 <- data.frame(Juegos = 11.11, Cartera = 33.33, Broquer = 0.00, An.lisi = 0.00, Foros = 0.00,
                   Noticies = 0.00, Previsi. = 11.11, Inversi. = 11.11, Ahorro = 22.22, 
                   Temps.real = 88.88)
pred.usu1.lda <- predict(fit, usu1)
pred.usu1.lda$posterior

# Panels of histograms and overlayed density plots
# for its first discriminant function
plot(fit, dimen = 1, type = "both")


plot(fit, dimen = 2)


# Loo-cv (Leave one out cross validation)
fit.cv <- lda(cluster ~ ., data = dat, na.action = "na.omit", CV = T)

# Assess the accuracy of the prediction percent correct for each category
ct <- table(dat$cluster, fit.cv$class)
ct

# Total percent correct
sum(diag(prop.table(ct)))

dat <- df[, -1]
# Quadratic (cross or not crossvalidated)
# fitq <- qda(cluster ~ ., data = dat)

dfx <- df[which(df$cluster != 3), ]
dfx$cluster <- as.factor(as.character(dfx$cluster))
table(dfx$cluster)

str(dfx)

dfx <- dfx[, -c(1)]

# Logistic can provide problems :'(
model <- glm(cluster ~ ., family = "binomial", data = dfx) 


### INFERENCE ###
# First we check if they follow normal multivariate or not. If they do not, we use the same as now but using
# Permutation tests. If few data or weird situations, we use PERMANOVA

g1<-as.matrix(dat[which(dat$cluster==1),-11]) # Multivariate normality
AD.test(u, qqplot = FALSE)

g2<-as.matrix(dat[which(dat$cluster==2),-11]) # Multivariate normality
AD.test(u, qqplot = FALSE)

g3<-as.matrix(dat[which(dat$cluster==3),-11]) # Multivariate normality
# AD.test(u, qqplot = FALSE)

# If they were all normal multivariate, then we would check covariance homogeneity
# Covariance Homogeneity
boxM(dat[,-11], dat[,11])

#
efinsa.manova<-manova(cbind(Juegos,Cartera,Broquer,An.lisi,Noticies,Foros,Previsi.,Inversi.,Ahorro,Temps.real)~cluster, data = dat)
summary(efinsa.manova,test = "Pillai")

# Pairwise comparison 
#
# Normality and same cov
# Now should check if same mean
HotellingsT2(g1, g2, test="chi")
HotellingsT2(g1, g3, test="chi")
HotellingsT2(g2, g3, test="chi")

# When not normall, we use Hotellings with permutation
Hotelling.perm12<-hotelling.test(g1, g2, perm = TRUE, progBar=T)
Hotelling.perm13<-hotelling.test(g1, g3, perm = TRUE, progBar=T)
Hotelling.perm23<-hotelling.test(g2, g3, perm = TRUE, progBar=T)

# Just to see it more graphically, these are the distributions that follos each of the null hypothesis for a pair of
# groups. The statistic can be obtained too, so you can check if what the function says make sense. SPOILER: It does
hist(Hotelling.perm12$results, 50)
hist(Hotelling.perm23$results, 50)
hist(Hotelling.perm13$results, 50)
Hotelling.perm13$stats$statistic # The statistic realized value goes 'Pa cuenca', so it makes sense the pvalue is 0

Hotelling.perm13$pval
