SigmaXX <-matrix(c(1,0.4,
                   0.4,1), ncol = 2, byrow = T)
SigmaYY <- matrix(c(1,0.2,
                    0.2,1), ncol = 2, byrow = T)
SigmaXY <- matrix(c(0.5, 0.6,
                    0.3,0.4), ncol = 2, byrow = T)
SigmaYX <- t(SigmaXY)

# Encontremos las correlaciones canonicas 
# y las variables canonicas

# para los alpha
M1 <- solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX

# para los gamma
M2 <- solve(SigmaYY)%*%SigmaYX%*%solve(SigmaXX)%*%SigmaXY

M1
M2
cor_can<-sqrt(eigen(M1)$values)
cor_can

# la primera correlación canónica es 0.7387
# Cor(U1, V1)=0.7387

# U1= alpha11X1 + alpha12X2
# V1= gammma11Y1 + gamma12Y2

# encontremos los coeficientes de alpha
alpha <- eigen(M1)$vectors
alpha

alpha1 <- alpha[,1]
alpha1
t(alpha1)%*%SigmaXX%*%alpha1

alpha1_N <- alpha1/sqrt(t(alpha1)%*%SigmaXX%*%alpha1)

t(alpha1_N)%*%SigmaXX%*%alpha1_N

alpha1_N

# U1 = 0.8559647X1 + 0.2777371X2


# para los coeficientes gamma
gamma <- eigen(M2)$vectors
gamma

gamma1 <- gamma[,1]
gamma1

gamma1_N <- gamma1/sqrt(t(gamma1)%*%SigmaYY%*%gamma1)

t(gamma1_N)%*%SigmaYY%*%gamma1_N

gamma1_N

# U1 = 0.8559647X1 + 0.2777371X2
# V1 = -0.5448119Y1 -0.7366455Y2
# Cor(U1,V1) = 0.7387

# relación entre los gamma y alpha
solve(SigmaYY)%*%SigmaYX%*%alpha1_N/cor_can[1]
gamma1_N


##### Datos Decathlon

library(ade4)
library(CCA)
data("olympic")

# 100 metros (100), salto largo (largo), tiro (poid), salto alto (alto), 400 metros (400),
#obstáculos de 110 metros (110 ), lanzamiento de disco (disq), salto con p?rtiga (perc),
#jabalina (jave) y 1500 metros (1500)

#Vamos a separar en dos conjuntos de datos
#las actividades que tienen que ver con los brazos
#Y las que tiene que ver con las piernas
#X: tiro(poid), disco (disq), javelina (jave), (perc) pole vault (variables brazos)
#Y: 100, salto en largo (long), salto en alto (haut), 400, obstaculos (110),1500 (piernas)

olympic$tab[,c(1,5,6,10)]


olympic$tab[,c(1,5,6,10)]<- -olympic$tab[,c(1,5,6,10)]

X <- olympic$tab[,c( "poid",  "disq" ,"perc", "jave")]
Y <- olympic$tab[,c("100",  "long","haut", "400" , "110",   "1500")]

m <- 4

p <- 6

r <- min(m,p)
r

# matriz de varianza y covarianza
correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

# analisis de correlación canónica
# calcular las matrices de varianza y covarianza muestral de los X y Y
SXX <- cov(X)

SXX

SYY <- cov(Y)
SYY

SXY<- cov(X,Y)
SXY

SYX <- t(SXY)


# hagamos la descomposición espectral

M1 <- solve(SXX)%*%SXY%*%solve(SYY)%*%SYX

# para los gamma
M2 <- solve(SYY)%*%SYX%*%solve(SXX)%*%SXY
M1
M2

# correlaciones canónicas
corr_can <- sqrt(eigen(M1)$values)
corr_can

# calculemos los coeficientes de las variables canónicas
# para las X
a <- eigen(M1)$vectors
a

# para la primera variable canónica
a1_N <- a[,1]/sqrt(t(a[,1])%*%SXX%*%a[,1])
a1_N

(t(a1_N)%*%SXX%*%a1_N)

b1_N <- solve(SYY)%*%SYX%*%a1_N/corr_can[1]
b1_N

# las primeras variables canónicas son
#U1 = 0.71066443X1 - 0.18313809X2 + 2.19793095X3 - 0.05174114X4
#V1 = 1.031779Y1 + 0.2927195Y2 - 0.981536Y3 - 0.2374631Y4 + 1.834411Y5 + 0.003311523Y6

#cor(U1,V1)=0.5866073

U1 <- as.matrix(X)%*%a1_N
U1

V1 <- as.matrix(Y)%*%b1_N
V1

cor(U1,V1)

plot(U1,V1, col="firebrick", pch=19)


##############################################################################


rendimiento <- read.csv("rendimiento.csv")

# 3 Variables independientes 
# X1: Peso (En libras)
# X2: Cintura (En pulgadas)
# X3: Pulsaciones por minuto en reposo 


# 3 Variables dependientes
# Y1: Número de flexiones
# Y2: Número de sentadillas
# Y3: Número de saltos


# analisis exploratorio de los datos
colMeans(rendimiento)
apply(rendimiento,2,var)

# debemos escalar los datos
X <- rendimiento[,c("Peso","Cintura","Pulso")]
Y <- rendimiento[,c("flexiones","Sentadillas","Saltos")]

correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

SXX <- cov(X)
SXX
colMeans(X)

# Escalo los datos, centrado y escalado
X_e<- scale(X)
Y_e<- scale(Y)
round(colMeans(X_e),4)
cov(X_e)

SXX <- cov(X_e)
SYY <- cov(Y_e)
SXY<- cov(X_e,Y_e)
SYX <- t(SXY)

SXX
SYY
SYX
SYY

M1 <- solve(SXX)%*%SXY%*%solve(SYY)%*%SYX
correlaciones_canonicas<- sqrt(eigen(M1)$values)
correlaciones_canonicas
# calcular las primeras variables canónicas
# coeficientes para las variables independientes
a1 <- eigen(M1)$vectors[,1]

#normalizamos el autovector
a1_n <- a1/sqrt(t(a1)%*%SXX%*%a1)

t(a1_n)%*%SXX%*%a1_n

a1_n

# U1 = 0.77539761Peso - 1.57934657Cintura + 0.05912012Pulso

# coeficientes para las variables dependientes
b1_n <- solve(SYY)%*%SYX%*%a1_n/correlaciones_canonicas[1]
b1_n

t(b1_n)%*%SYY%*%b1_n

# V1 = 0.3494969flexiones + 1.0540110Sentadillas - 0.7164267Saltos



U1 <- X_e%*%a1_n
U1

V1 <- Y_e%*%b1_n
V1

cor(U1,V1)

plot(U1,V1, col="firebrick", pch=19)

# libraria CCA
library(CCA)
help(cc)

analisiCC <- cc(X_e,Y_e)

# correlaciones canónicas
analisiCC$cor
correlaciones_canonicas

# coeficientes a 
analisiCC$xcoef
eigen(M1)$vectors
a1_n

# coeficientes b
analisiCC$ycoef

# Proyecciones de las variables independientes
# sobre las correlaciones canóncias
U <- analisiCC$scores$xscores
U
# Proyecciones de las variables dependientes
# sobre las correlaciones canóncias

V <- analisiCC$scores$yscores
V

par(mfrow=c(1,3))
plot(U[,1],V[,1], col="firebrick", pch=19,
     main="Primer par canónico")
plot(U[,2],V[,2], col="firebrick", pch=19,
     main="Segundo par canónico")
plot(U[,3],V[,3], col="firebrick", pch=19,
     main="Tercer par canónico")
rendimiento <- read.csv("rendimiento.csv")


which.min(U[,1])





