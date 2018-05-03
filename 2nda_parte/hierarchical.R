data<-read.table("./2nda_parte/FANGA_TAUFA_del_tab.txt",sep="\t",header=TRUE)
data2<-scale(data[,2:14])
data.D1 <- dist(data2, method="eucl")

# Some clusters