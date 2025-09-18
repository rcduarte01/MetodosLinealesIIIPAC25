n <-4 # número de individuos
p <-2 # número de variables

X <- matrix(c(42,52,48,58,4,5,4,3), ncol = 2 )
X

# vector de medias
xbar <- colMeans(X)
xbar

# matriz de covarianza 
Sn <- var(X)*(n-1)/n
Sn

R <- cor(X)
R


# Crear un data frame
datos <- data.frame(venta_en_dolares =c(42,52,48,58),
                    numero_de_libros =c(4,5,4,3))

colMeans(datos)
(n-1)/n*var(datos)

cor(datos)


