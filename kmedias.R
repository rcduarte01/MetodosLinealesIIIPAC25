Notas <- data.frame(Estudiante=c("Lucía","Pedro","Inés",
                                 "Luis","Andrés","Ana",
                                 "Carlos","José","Sonia","María"),
                    Matemática=c(7,7.5,7.6,5,6,7.8,6.3,7.9,6,6.8),
                    Ciencias =c(6.5,9.4,9.2,6.5,6,9.6,6.4,9.7,6,7.2),
                    Español=c(9.2,7.3,8, 6.5, 7.8, 7.7, 8.2, 7.5, 6.5, 8.7),
                    Historia =c(8.6, 7, 8, 7, 8.9, 8,9, 8, 5.5, 9),
                    Ed.Física=c(8, 7, 7.5, 9.0, 7.3, 6.5, 7.2, 6, 8.7,7))
####
data <- Notas[,2:6]

kmeans(x = data, centers = 3)

set.seed(2025)
cluster_kmedias <- kmeans(x = data, centers = 3)

help("kmeans")
cluster_kmedias$withinss
cluster_kmedias$tot.withinss

cluster_kmedias$size
cluster_kmedias$iter


library(factoextra)
fviz_cluster(object = cluster_kmedias, data = data,
             show.clust.cent = TRUE, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")

#cuantos clusters
#wss es Elowb
fviz_nbclust(x = data, FUNcluster = kmeans, method = "wss", k.max = 9, 
             diss = get_dist(data, method = "euclidean"), nstart = 25)+
  labs(title = "Número óptimo de clusters")


# records Nacionales femeninos de atletismo
# Ejercicio 12.16 Libro
records <- read.table("T1-9.dat",sep="\t")

# calculamos la matriz de distancias euclideas
dist <- dist(records[,2:8])
cluust_S <- hclust(dist,method = "single")
cluust_C <- hclust(dist,method = "complete")

# graficamos los dendogramas
par(mfrow=c(1,2))
plot(cluust_S, hang = -1)
plot(cluust_C, hang = -1)
par(mfrow=c(1,1))

#queremos k=5 clusters


records$CHS <-cutree(cluust_S, k=5)
records$CHC <-cutree(cluust_C, k=5)

# aplicamos k medias

set.seed(123)
cluster_kmedias <- kmeans(x = records[,2:8], centers = 5)

records$kmedias <-cluster_kmedias$cluster

# Elegir el número optimo de cluster usando el metodo Elbow

fviz_nbclust(x = records[,2:8], FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(records[,2:8], method = "euclidean"), nstart = 25)+
  labs(title = "Número óptimo de clusters")

# según el método Elbow, el número óptimo de cluster es k=3

set.seed(123)
cluster_kmedias3 <- kmeans(x = records[,2:8], centers = 3)

records$kmedias3 <-cluster_kmedias3$cluster

cluster_kmedias3$centers

# escalando los datos
records_s <- scale(records[,2:8])
cov(records_s)
round(colMeans(records_s),4)

# número óptimo de clusters con la data escalada
fviz_nbclust(x = records_s, FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(records_s, method = "euclidean"), nstart = 25)+
  labs(title = "Número óptimo de clusters")

set.seed(123)
cluster_kmedias_s <- kmeans(x = records_s, centers = 3)
records_s<-cbind(records_s,cluster= cluster_kmedias_s$cluster)

# analissi de componentes principales

fviz_cluster(object = cluster_kmedias_s, data = records_s[,1:7],
             show.clust.cent = TRUE, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")

table(cluster_kmedias_s$cluster)
records[c(11,40,46),1]

componentes <- prcomp(records_s[,1:7])
componentes$rotation
