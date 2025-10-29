Sigma <- matrix(c(1,-2,0,-2,5,0,0,0,2), byrow = T, ncol = 3)
Sigma

descomposicion <- eigen(Sigma)

descomposicion$values
descomposicion$vectors


sum(diag(Sigma))


library(scatterplot3d)
library(misc3d)
data(teapot)

dat <- matrix(unlist(teapot[[1]]), ncol = 3, byrow = TRUE)
dat<-cbind(dat[ , 1], dat[ , 3], dat[ , 2])



scatterplot3d(dat, highlight.3d = TRUE, angle = 75, pch = 19,  xlab = "", ylab = "", zlab = "", main = "")

library(rgl)
open3d()


library(plotly)
plot_ly(x=dat[,1], y=dat[,2], z=dat[,3],
        size = 1)

plot3d( 
  x=dat[,1], y=dat[,2], z=dat[,3], 
  type = 's',
  radius = 0.1)

# Calculamos la matriz de varianza y covarianza de los datos
S <- cov(dat)
S
autovalores <- eigen(S)$values
autovectores <- eigen(S)$vectors

# varianza acumulada por cada componente principal
cumsum(autovalores)/sum(autovalores)*100

autovectores

# primera componente principal
# Y1 = 0.999X1 + 0.0406X3
# Y2 = 0.0406X1-0.9992X3


# proyectamos los datos originales en las componentes principales
Y1 <- dat%*%autovectores[,1]
Y1

Y2 <- dat%*%autovectores[,2]
Y2

cor(Y1,Y2)
cor(Y1,dat[,1])

plot(Y1,Y2)

CompPrin <- data.frame(Y1,Y2)
plot_ly(data=CompPrin, x=~Y1, y=~Y2,
        size = 1) %>%
  layout(xaxis = list(title = "Primera componente principal"), 
         yaxis = list(title = "Segunda componente principal")) 


