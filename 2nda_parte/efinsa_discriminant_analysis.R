require(MASS)
require(ggplot2)
require(vegan)
require(cluster)
require(factoextra)
require(clusterSim)

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
str(dat)
fitq <- qda(cluster ~ ., data = dat)

dfx <- df[which(df$cluster != 3), ]
dfx$cluster <- as.factor(as.character(dfx$cluster))
table(dfx$cluster)

str(dfx)

dfx <- dfx[, -c(1)]

model <- glm(cluster ~ ., family = "binomial", data = dfx)
