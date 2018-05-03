ecosystems <-read.table("./2nda_parte/ecosystems.txt",sep="\t",header=TRUE)
cor(ecosystems)

# Comentario del profe: Podemos hacer todos los metodos mas robustos ante outliers, usamos la mediana como estimador de
# E(x) para calcular matriz de covarianzas.
# Le pregunte al profe si esta era una buena aproximacion cuando las variables no siguen normales

# Para decidir si maha o euclidea tenemos que saber si la intencion es tener en cuenta 3 veces las variables de
# Contaminacion organica

ecosystems.scale <- scale(ecosystems)

ecosystems.d1 <- dist(ecosystems.scale, method = "euclidean")

#clustering
require(cluster)
clusterW<-hclust(ecosystems.d1^2, method="ward.D2")

plot(clusterW)

#Agglomerateive clustering, UPGMA
plot(clusterW, hang=-1)#, labels = colnames(ecosystems))


#KMeans
result.km.2 = kmeans(countries2, centers=2, nstart=1000)
names(result.km.2)
result.km.2$size
result.km.2$centers
result.km.2$cluster
result.km.2$totss # The total error sum of squares (TESS)
result.km.2$tot.withinss # The total error sum of squares within groups