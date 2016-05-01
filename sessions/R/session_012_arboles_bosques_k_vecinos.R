#############################################################################
# Ciencia de datos - R - Parte 12: regresi贸n lineal y log铆stica
# cgb@datanalytics.com, 2016-02-15
#
# El objetivo de esta sesi贸n es aprender los fundamentos de modelos de regresi贸n
#   y clasificaci贸n alternativos a los cl谩sicos (regresi贸n lineal y log铆stica):
#   谩rboles, bosques y k-vecinos
#############################################################################

install.packages("party")

library(party)
library(randomForest)
library(class)

# datos brutos (los dejo siempre en memoria "tal cual")
aceites <- read.table("Olive.txt", header = T, sep = "\t")

# datos limpios
dat.region <- aceites
dat.region$Test.Training <- NULL
dat.region$Area <- NULL

arbol.region <- ctree(Region ~ ., data = dat.region)
plot(arbol.region)

# 驴Cu谩ntos errores cometemos?
reales <- dat.region$Region
predichos <- predict(arbol.region)

# varias maneras de ver el error
sum(reales != predichos)
mean(reales != predichos)
table(reales, predichos)                  #  Matriz de confusi髇 !!!!!!!


# Ejercicio: repetir el ejercicio tratando de predecir la variable Area

dat.region <- aceites
dat.region$Test.Training <- NULL
dat.region$Region <- NULL

arbol.region <- ctree(Area ~ ., data = dat.region)
plot(arbol.region)

# Ejercicio: 驴cu谩ntos errores comete la predicci贸n del 谩rea?

# 驴Cu谩ntos errores cometemos?
reales <- dat.region$Area
predichos <- predict(arbol.region)

# varias maneras de ver el error
sum(reales != predichos)
mean(reales != predichos)
table(reales, predichos)                  #  Matriz de confusi髇 !!!!!!!



###############################################################################################
# curva ROC (basado en http://freakonometrics.hypotheses.org/9066)
###############################################################################################


datos <- read.table("http://freakonometrics.free.fr/db.txt", header = TRUE, sep = ";")

tmp <- rep("ACD", length(datos$X3))
tmp[datos$X3 %in% c("B","E")] <- "BE"

datos$X3bis <- as.factor(tmp)
modelo <- glm(Y ~ X1 + X2 + X3bis, family = binomial, data = datos)

S <- predict(modelo, type = "response")

roc.curve=function(s, real, predicho, print = FALSE){
 Ps <- (predicho > s)*1
 
 FP <- sum((Ps == 1)*(real == 0))/ sum(real == 0)
 # FP: de todos los noes (reales), a cu谩ntos les asigno S铆
 
 TP <- sum((Ps == 1)*(real == 1))/ sum(real == 1)
 # TP: de todos los s铆es (reales), a cu谩ntos les digo que s铆
 
  if (print == TRUE){
      print(table(Observed = real, Predicted = Ps))
  }
  vect = c(FP,TP)
  names(vect) = c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold, datos$Y, S, print = TRUE) # Matriz de confusi髇 cuando el criterio de corte es 0.5 (con 0.25 mal, s髄o pillo uno de los ceros)

res <- sapply(seq(0,1,by = .01), roc.curve, datos$Y, S)

plot(res[1,], res[2,], col= "grey", lwd = 2, type = "l",
     xlab = "FPR", ylab = "TPR")


###############################################################################################
# entrenamiento y validaci贸n
###############################################################################################

# T铆picamente, para crear y validar modelos, dividiremos nuestros datos en dos subconjuntos: 
#   entrenamiento, sobre el que construiremos el modelo
#   validaci贸n, en el que mediremos el error cometido

# nos gustar谩n aquellos modelos en los que el error en validaci贸n sea el menor posible

# escogemos el 80% para entrenamiento (p.e.)

filas.entrenamiento <- sample(1:nrow(aceites), 0.8 * nrow(aceites))

aceites.train <- aceites[filas.entrenamiento,]
aceites.test  <- aceites[-filas.entrenamiento,]


# ejercicio: partir "student-mat.csv" en entrenamiento / validaci贸n (70/30)


###############################################################################################
# comparaci贸n de modelos sobre entrenamiento y validaci贸n
###############################################################################################

aceites.train$Test.Training <- aceites.train$Region <- NULL

modelo.ctree <- ctree(Area ~ ., data = aceites.train)

reales <- aceites.test$Area
predichos <- predict(modelo.ctree, aceites.test)

table(reales, predichos)
error.rate <- mean(reales != predichos)

# Ejercicio: comparar los modelos lineales dist ~ speed y dist ~ speed + I(speed^2) usando entrenamiento y 
#   validaci贸n. Comparadlos en t茅rminos del rmse en el conjunto de validaci贸n.

# Crear entrenamiento y validaci髇

filas.entrenamiento <- sample(1:nrow(cars), 0.8 * nrow(cars))

cars.train <- cars[filas.entrenamiento,]
cars.test  <- cars[-filas.entrenamiento,]

# Crear los modelos usando entrenamiento

mod.0 <- lm(dist ~ speed, data=cars.train)
mod.1 <- lm(dist ~ speed+I(speed^2), data=cars.train)

# predecir ambos modelos sobre validaci髇

real <- cars.test$dist

pred.0 <- predict(mod.0,cars.test)
pred.1 <- predict(mod.1,cars.test)

plot(real,pred.0)
plot(real,pred.1)


# Comparar el RMSE

rmse <- function(real,predicho) sqrt(mean((real-predicho)^2))

rmse(real,pred.0)
rmse(real,pred.1)

###############################################################################################
# sobreajuste
###############################################################################################

modelo.profundo <- ctree(Area ~ ., data = aceites.train, control = ctree_control(minbucket = 1, mincriterion = 0.1, minsplit = 2))

# Error en el conjunto de entrenamiento IMP!!!= un modelo m醩 complicado tiene menos error en entrenamiento siempre!!!!!
mean(aceites.train$Area != predict(modelo.ctree))
mean(aceites.train$Area != predict(modelo.profundo))

# Error en el conjunto de validaci贸n !!! Sin embargo un modelo m醩 complicado falla m醩 en validaci髇 que en entrenamiento 
mean(aceites.test$Area != predict(modelo.ctree, aceites.test))
mean(aceites.test$Area != predict(modelo.profundo, aceites.test))


# En resumen:
#   un modelo demasiado simple lo hace mal en entrenamiento y en validaci贸n
#   un modelo demasiado complejo lo hace muy bien en entrenamiento y mal en validaci贸n
#   el modelo "ideal" no es ni demasiado simple ni demasiado complejo


###############################################################################################
# jackknife
###############################################################################################

preds.0 <- sapply(1:nrow(cars), function(i){
  predict(lm(dist ~ speed, data = cars[-i,]), cars[i,])
})

rmse.0 <- sqrt(mean((cars$speed - preds.0)^2))

# Ejercicio: calcular el rmse para el modelo dist ~ speed + I(speed^2) y comparadlos.

preds.1 <- sapply(1:nrow(cars), function(i){
  predict(lm(dist ~ speed+I(speed^2), data = cars[-i,]), cars[i,])
})

rmse.1 <- sqrt(mean((cars$speed - preds.1)^2))

rmse.0
rmse.1  # Una diferencia tan peque馻 hace pensar que no merece la pena el nuevo modelo 1



###############################################################################################
# selecci贸n de modelos (con k-vecinos)
###############################################################################################

aceites.test$Test.Training <- aceites.test$Region <- NULL

test <- function(k){
  res <- knn(aceites.train[,-1], aceites.test[,-1], aceites.train$Area, k = k)
  mean(res != aceites.test$Area) 
}

k.values <- 1:20
errores <- sapply(k.values, test)
plot(k.values, 100 * errores, type = "l", 
     main = "% error en validaci贸n seg煤n el\nn煤mero de vecinos", xlab = "n煤mero de vecinos", ylab = "% de error")

# !!! Veo que el k optimo es 4 

# Ejercicio, comparar con primero normalizando los datos


# Ejercicio: en lugar de usar entrenamiento y validaci贸n, usar la funci贸n knn.cv para 
#   hacer un jackknife (ver primero ?knn.cv)

tmp <- aceites
tmp$Region <- tmp$Test.Training <- NULL

test <- function(k){
  res <- knn.cv(tmp[,-1], tmp$Area, k = k)
  mean(res != tmp$Area) 
}

k.values <- 1:20
errores <- sapply(k.values, test)
plot(k.values, 100 * errores, type = "l", 
     main = "% error en validaci贸n seg煤n el\nn煤mero de vecinos", xlab = "n煤mero de vecinos", ylab = "% de error")

# Me quedo con k=3 !!!!!!!!!!1
knn.cv(tmp[,-1], tmp$Area, k = 3)

###############################################################################################
# validaci贸n cruzada
###############################################################################################

ids <- rep(1:10, length.out = nrow(cars))
table(ids)

# Ejercicio: 驴qu茅 pasa si length.out no es m煤ltiplo de 10?

ids <- sample(ids)      # las aleatorizo

preds.cv <- lapply(unique(ids), function(i){
  preds <- predict(lm(dist ~ speed, data = cars[ids != i,]), cars[ids == i,])
  data.frame(preds = preds, real = cars[ids == i,]$dist)
})

preds.cv <- do.call(rbind, preds.cv)

rmse.0 <- sqrt(mean((preds.cv$preds - preds.cv$real)^2))

# Ejercicio: calcular el rmse para el otro modelo

ids <- rep(1:10, length.out = nrow(cars))
table(ids)

ids <- sample(ids)

preds.cv.1 <- lapply(unique(ids), function(i){
  preds <- predict(lm(dist ~ speed+I(speed^2), data = cars[ids != i,]), cars[ids == i,])
  data.frame(preds = preds, real = cars[ids == i,]$dist)
})

preds.cv.1 <- do.call(rbind, preds.cv.1)

rmse.1 <- sqrt(mean((preds.cv.1$preds - preds.cv.1$real)^2))

rmse.0
rmse.1


###############################################################################################
# Introducci贸n al paquete caret
###############################################################################################

install.packages("caret")
library(caret)

# Dos ejemplos basados en 
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

## validaci贸n cruzada con lm 

train.control <- trainControl(method = "cv", number = 10)
# train the model
model <- train(dist ~ speed, data=cars, trControl=train.control, method="lm")
# summarize results
print(model)  


# validaci贸n cruzada y selecci贸n del k 贸ptimo con k-vecinos

train.control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = 1:10)
model <- train(dist ~ speed, data = cars, trControl = train.control, method = "knn", tuneGrid = grid)
print(model)

# Ejercicio: ajustar modelos de los planteados hoy usando caret y las distintas opciones que
#   aparecen en http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/


  