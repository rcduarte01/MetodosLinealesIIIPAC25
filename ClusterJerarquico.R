library(factoextra)
data <- cbind(c(2,4,0,3),
              c(3.5,3,6,3),
              c(0,1.5,4,1),
              c(4,5,2,4),
              c(7,6,3,77))

round(dist(data),2) # por defecto tenemos la distancia euclidea

dist(data, method = "manhattan",
     upper = T, diag = T)

# Ejemplo de clase


datos <- c(7, 10, 20, 28, 35)
datos

# calculamos la matriz de disimilaridad(distancia)
dist_euc <- dist(datos)
dist_euc

class(dist_euc)

help("hclust")

# analisis de cluster jerarquico con metodo Single
clust_sing <- hclust(dist_euc, method = "single")
clust_sing

clust_sing$dist.method
clust_sing$height
clust_sing$order
clust_sing$merge

plot(clust_sing, hang = -1)

# cambiemos el método y la distancia
dist_man <- dist(datos, method = "manhattan")
clust_com_man <- hclust(dist_man)
clust_com_man

plot(clust_com_man, hang = -1)

par(mfrow=c(1,2))
plot(clust_sing, hang = -1,
     main = "Single-euclidea")
plot(clust_com_man, hang = -1,
     main = "Complete-Manhattan")
par(mfrow=c(1,1))

# podemos hacer cortes en el dendograma para encontrar
# la cantidad de clusters 

cutree(clust_com_man, k=3)

# Analisis de cluster jerarquico con los datos 
# de violencia

datos <- USArrests

dist <- dist(datos)

as.matrix(dist)[1:5,1:5]

fviz_dist(dist.obj = dist, lab_size = 5)

# construyamos los clusters
cluster_euc_single <- hclust(dist,method = "single")


plot(cluster_euc_single, hang = -1)

# queremos 10 clusters
gruposk10 <- cutree(cluster_euc_single , k=10)
datos <- cbind(datos,gruposk10)
table(datos$gruposk10)


fviz_dend(x = cluster_euc_single, k = 5, cex = 0.6) +
  labs(title = "Cluster jerarquico",
       subtitle = "Distancia euclídea, Single Linkage, K=5")

fviz_dend(x = cluster_euc_single, k = 3, cex = 0.6) +
  labs(title = "Cluster jerarquico",
       subtitle = "Distancia euclídea, Single Linkage, K=3")

## analisis solo con variables de violencia
dist <- dist(datos[,c(1,2,4)])
dist

cluster_violencia <- hclust(dist,method = "average")

fviz_dend(x = cluster_violencia, k = 3, cex = 0.6) +
  labs(title = "Cluster jerarquico",
       subtitle = "Distancia euclídea, Average Linkage, K=3")
