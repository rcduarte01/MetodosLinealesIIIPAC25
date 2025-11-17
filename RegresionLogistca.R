hongos_clasificados <- read.csv("hongos_clasificados.txt", sep="")
View(hongos_clasificados)
plot(hongos_clasificados)

# debemos considerar la variable respuesta como 1 o 0
hongos_clasificados$Variety <- hongos_clasificados$Variety-1

plot(hongos_clasificados$Height, hongos_clasificados$Variety)

# hagamos un ajuste lineal de los datos
ajuste <- lm(Variety ~ Height, data=hongos_clasificados)

summary(ajuste)

plot(hongos_clasificados$Height, hongos_clasificados$Variety)

abline(ajuste)


# Función sigmoidea

curve(1/(1+exp(-x)), from=-10, to =10, lwd=2)

#En R el ajuste de la regresión logistica se puede hacer con la función
# glm indicando la función de enlace de la familia binomial.

# ajuste regresión logistica
ajuste_logistica <- glm(Variety ~ Height, data=hongos_clasificados,
                        family = "binomial")

summary(ajuste_logistica)
beta0 <- as.numeric(coef(ajuste_logistica)[1])
beta1 <- as.numeric(coef(ajuste_logistica)[2])

plot(hongos_clasificados$Height, hongos_clasificados$Variety)
curve(1/(1+exp(-(beta0+beta1*x))), add = T, col="red", lwd=2)

# ahora hagamos la clasificación

prob<-1/(1+exp(-(beta0+beta1*hongos_clasificados$Height)))
prob
hongos_clasificados$prob<- prob

clasificacion<- ifelse(prob<0.5,0,1)
hongos_clasificados$Clasf<- clasificacion
predict.glm(ajuste_logistica, type = "response")

predict.glm(ajuste_logistica, newdata = data.frame(Height=6.7),
            type = "response")



confusion<-table(hongos_clasificados$Variety, hongos_clasificados$Clasf)


accuracy<-(confusion[1,1]+confusion[2,2])/500

library(caret)
help("confusionMatrix")

confusionMatrix(as.factor(hongos_clasificados$Variety),
                as.factor(hongos_clasificados$Clasf), positive = "1")


# una forma de entrenar un modelo, es dividir la data en dos conjuntos
# Entrenamiento: Conjunto de datos para ajustar el modelo.(80%) 
# Testeo: Conjunto de datos para validar el ajuste del modelo.(20%)

# volver a leer la data

hongos_clasificados <- read.csv("hongos_clasificados.txt", sep="")
# debemos considerar la variable respuesta como 1 o 0
hongos_clasificados$Variety <- hongos_clasificados$Variety-1

# dividamos los datos originales en entrenamiento y testeo
set.seed(2025)
indices <- sample(1:500, 400)

entrenamiento_hongos <- hongos_clasificados[indices,]

testeo_hongos<-hongos_clasificados[-indices,]

# ajustemos el modelo con la data de entrenamiento
ajuste_entrenamiento <- glm(Variety ~ Height, data=entrenamiento_hongos,
                            family = "binomial")

plot(entrenamiento_hongos$Height, entrenamiento_hongos$Variety)
beta0 <- as.numeric(coef(ajuste_entrenamiento)[1])
beta1 <- as.numeric(coef(ajuste_entrenamiento)[2])
curve(1/(1+exp(-(beta0+beta1*x))), add = T, col="red", lwd=2)

# con R podemos hacer predicciones del modelo
1/(1+exp(-(beta0+beta1*8.5)))

predict.glm(ajuste_entrenamiento,
            newdata = data.frame(Height=8.5), type = "response")

# podemos calculñar las probabilidades para toda la data de testeo
testeo_hongos$prob <- predict.glm(ajuste_entrenamiento,
                                  newdata = data.frame(Height=testeo_hongos$Height),
                                  type = "response")


# definamos un umbral para las clasificaciones
testeo_hongos$clasificacion0.5 <- ifelse(testeo_hongos$prob >= 0.5,
                                         1,0)
# podemos ver que tan buenas fueron mis predicciones
table(testeo_hongos$clasificacion0.5,testeo_hongos$Variety)

# Accuracy
(40+54)/100

#precisión VP/(VP +FP)
47/(47+12)

# recall o sensibilidad VP/(VP + FN)
54/(54 + 5)



# Paquete caret
library(caret)

matrizConf<-confusionMatrix(as.factor(testeo_hongos$clasificacion0.5),
                            as.factor(testeo_hongos$Variety), positive = "1")
matrizConf
matrizConf$overall[1]

# debemos elegir un umbral adecuado para la clasificación
# tomamos una malla de valores de p
malla <- seq(from=0.1, to=0.9, by=0.01)
acc<- c()
n <- length(malla)
for(i in 1:n){
  clasificacion <- ifelse(testeo_hongos$prob < malla[i],0,1)
  matriz <- confusionMatrix(as.factor(clasificacion),
                            as.factor(testeo_hongos$Variety))
  acc[i]<-matriz$overall[1]
}

plot(malla,acc, col="firebrick", pch=19, main="Elección del umbral de clasificación",
     xlab = "Umbral",ylab = "Accuracy")

max(acc)
which.max(acc)
malla[39]

testeo_hongos$clasificacion0.48 <- ifelse(testeo_hongos$prob <0.48,0,1)
confusionMatrix(as.factor(testeo_hongos$Variety),
                as.factor(testeo_hongos$clasificacion0.48), positive = "1")
