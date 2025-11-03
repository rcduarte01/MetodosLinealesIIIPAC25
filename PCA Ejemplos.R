
# Ejemplo demografico
# El archivo demograficos.csv contiene datos censales de españa (Baillo 2008)
# x1: Población total (miles)
# x2: Inmigrantes extranjeros de los últimos 5 años (decenas).
# x3: Número promedio de hijos por pareja.
# x4: porcentaje de desocupados
# x5: porcentaje de hogares ocupados por una sola persona.

datos <- read.csv("demograficos.csv", sep=";")
n <- 14
p <- 5

# calculemos el vector de medias muestrales
Xbar <- colMeans(datos[,2:6])
Xbar


matriz_medias <- matrix(rep(Xbar,c(n,n,n,n,n)), byrow = F, ncol = p)
matriz_medias



data_c <- datos[,2:6] - matriz_medias
data_c 
round(colMeans(data_c),4)

S <- cov(data_c)
S

autovalores <- eigen(S)$values
autovectores <- eigen(S)$vectors
autovalores
autovectores


# Variabilidad explicada por cada componente
autovalores/sum(autovalores)*100

# variabilidad acumulada explicada por cada componente
cumsum(autovalores)/sum(autovalores)*100

# con las primeras dos componentes tenemos el 89.3% de la
# varibilidad explicada.

# proyectamos en las primeras dos componentes 
y1 <- t(autovectores[,1]%*%t(as.matrix(data_c)))
y2 <- t(autovectores[,2]%*%t(as.matrix(data_c)))
proyecciones <- data.frame(y1,y2)

cor(y1,y2)

cor(y1, data_c[,1])

# prcomp
componetes <- prcomp(datos[,2:6])

# obtenemos los autovectores (coeficientes de las componentes)
componetes$rotation
autovectores

componetes$center
componetes$scale
componetes$sdev
sqrt(autovalores)

biplot(componetes)
datos[c(11,13,14),]



### Ejemplo violencia
library(factoextra)
library(corrplot)
#El set de datos USArrests del paquete básico de R contiene el porcentaje
#de asaltos (Assault), asesinatos (Murder) y violaciones (Rape) 
# por cada 100,000 habitantes para cada uno de los 50 estados de USA (1973).
#Además, también incluye el porcentaje de la población de cada estado 
#que vive en zonas rurales(UrbanPoP).

datos <- USArrests

stars(datos, labels = abbreviate(rownames( datos ),4), 
      nrow = 8, key.loc = c( 18, 19 ), full = TRUE )

corrplot(cor(datos), order = "hclust")

apply(datos,2,mean)
apply(datos,2,var)


analisi1<- prcomp(datos)
analisi1$rotation
analisi1$x
analisi1$scale 
analisi1$center
analisi1$sdev

# hagamos un escalod en los datos
analisis2 <- prcomp(datos, scale. = TRUE)
analisis2$rotation

# realizar el analisis de componentes principales con los datos escalados
# es equivalente a realizar la descomposición en autovalores y atovectores
# de la matriz de correlacion.

R <- cor(datos)
R
eigen(R)$vectors


# exploremos los resultados
analisis2$rotation

eigen(R)$values
analisis2$sdev^2 # estos son los autovalores

sum(eigen(R)$values)

analisis2$sdev^2/sum(analisis2$sdev^2)*100
cumsum(analisis2$sdev^2/sum(analisis2$sdev^2)*100)



eig.val <- get_eigenvalue(analisis2)
eig.val

#GRAFICOS
fviz_eig(analisis2,choice="variance")

# grafico de variables
fviz_pca_var(analisis2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#grafico de los individuos
fviz_pca_ind(analisis2,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)