library(car)
library(heplots)
data(Diabetes)
str(Diabetes)

sel1 <- which(Diabetes$group == "Normal")
sel2 <- which(Diabetes$group == "Chemical_Diabetic")
sel3 <- which(Diabetes$group == "Overt_Diabetic")
S1 <- cov(Diabetes[sel1, -6])
S2 <- cov(Diabetes[sel2, -6])
S3 <- cov(Diabetes[sel3, -6])

n1 <- length(sel1)
n2 <- length(sel2)
n3 <- length(sel3)

n<-n1+n2+n3

k<-3
S1 <- (n1 - 1) * S1 / n1
S2 <- (n2 - 1) * S2 / n2
S3 <- (n3 - 1) * S3 / n3
W <- (n1 * S1 + n2 * S2 + n3 * S3) # within
Sp <- W/(n - k) # pooled

m<-apply(Diabetes[,-6],2,mean)
m
m1<-apply(Diabetes[sel1,-6],2,mean)
m2<-apply(Diabetes[sel2,-6],2,mean)
m3<-apply(Diabetes[sel3,-6],2,mean)
v1<-as.matrix(m1-m)
v2<-as.matrix(m2-m)
v3<-as.matrix(m3-m)

B<-n1*v1%*%t(v1)+n2*v2%*%t(v2)+n3*v3%*%t(v3)

A<-solve(Sp)%*%B
vecs<-eigen(A)$vectors
Y<-as.matrix(Diabetes[,-6])%*%vecs
ym1<-t(as.matrix(m1))%*%vecs
ym2<-t(as.matrix(m2))%*%vecs
ym3<-t(as.matrix(m3))%*%vecs

Diabetes$g<-0
Diabetes[sel1,"g"]<-1
Diabetes[sel2,"g"]<-2
Diabetes[sel3,"g"]<-3
plot(Y[,1],Y[,2],type="n",xlab="1st canonical dim",ylab="2nd canonical dim")
points(Y[,1],Y[,2],col=Diabetes$g)
points(ym[,1],ym[,2],col="blue",pch=19)