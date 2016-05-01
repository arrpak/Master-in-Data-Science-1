#############################################################################
# Ciencia de datos - R - Parte 13: svm y gbm
# cgb@datanalytics.com, 2016-02-23
#
# El objetivo de esta sesiÃ³n es aprender a usar dos mÃ©todos modernos de ML:
#   svm y glm
#############################################################################



library(party)
library(randomForest)

library(caret)

library(e1071)    # svm
library(gbm)      # gbm

# Nota: existe una alternativa a gbm, xgboost, que implementa los mismos modelos (aproximadamente)
#   y que tiene bastante buena fama


# datos brutos (los dejo siempre en memoria "tal cual")
billetes <- read.table("data_banknote_authentication.txt", header = T, sep = ",")
billetes$class <- factor(billetes$class)

# inspecciÃ³n rÃ¡pida
summary(billetes)

mod.ctree <- ctree(class ~ ., data = billetes)

plot(mod.ctree)

# Â¿CuÃ¡ntos errores cometemos?
reales <- billetes$class
pred.ctree <- predict(mod.ctree)

# varias maneras de ver el error
sum(reales != pred.ctree)
mean(reales != pred.ctree)
table(reales, pred.ctree)

 
# con gbm . El funcionamiento es datos, residuos. Luego APLICO EL MODELO AL RESIDUO como si fuesen datos, y dan otros residuos menoires, etc,etc
# ejemplo pisos Munich Martin Theus

dat.gbm <- billetes
dat.gbm$class <- as.character(dat.gbm$class)

mod.gbm <- gbm(class ~ ., data = dat.gbm, interaction.depth = 6, n.trees = 10000, cv.folds = 3)

tmp <- gbm.perf(mod.gbm, method = "cv")

table(dat.gbm$class, predict(mod.gbm, type = "response", n.trees = tmp) > 0.5) # PERFECTO EN TRAINING

# Ejercicio: Â¿cÃ³mo se puede ver la importancia de variables en el modelo? (Busca en Google)


# con svm # CASI SIEMPRE SE USA CON CLASIFICACIÓN (poco en regresión)
dat.svm <- billetes
mod.svm <- svm(class ~ ., data = dat.svm, kernel = "radial")
table(dat.gbm$class, predict(mod.svm))

# Ejercicio: probar con random forests


###############################################################################################
# con entrenamiento y validaciÃ³n
###############################################################################################

filas.entrenamiento <- sample(1:nrow(billetes), 0.8 * nrow(billetes))

billetes.train <- billetes[filas.entrenamiento,]
billetes.test  <- billetes[-filas.entrenamiento,]

mod.ctree <- ctree(class ~ ., data = billetes.train)   # En R se puede salvar a disco y enviar por correo por ejemplo (el modelo se puede serializar)

# Â¿CuÃ¡ntos errores cometemos?
reales <- billetes.test$class

pred.ctree <- predict(mod.ctree, billetes.test)
table(reales, pred.ctree)                             # No está mal para lo simple del modelo, pero no voy a ganar kaggle


# con gbm
dat.gbm <- billetes.train
dat.gbm$class <- as.character(dat.gbm$class)

mod.gbm <- gbm(class ~ ., data = dat.gbm, interaction.depth = 6, n.trees = 10000, cv.folds = 3)

tmp <- gbm.perf(mod.gbm, method = "cv")

table(reales, predict(mod.gbm, billetes.test, type = "response", n.trees = tmp) > 0.5)  # Mucho mejor


# con svm
mod.svm <- svm(class ~ ., data = billetes.train, kernel = "radial")
table(reales, predict(mod.svm, billetes.test))                                          # Mucho mejor


# Ejercicio: probar con random forests



###############################################################################################
# validaciÃ³n cruzada, etc. con caret
###############################################################################################

library(caret)

## validaciÃ³n cruzada con gbm

train.control <- trainControl(method = "cv", number = 10)

# compara todos los valoes con todos (ES UNA MALLA) caret aplica validación cruzada en cada punto de la malla
# Esto se hace para ver cual es el número de arboles y profundidad óptimos de la validación (lo que hicimos la semana pasada a mano)
# Los que más se tocan es n.trees y profundidad)

grid <- expand.grid(n.trees = 1000 * 1:4,
                    interaction.depth = 2 * (1:4),
                    shrinkage = 0.001,
                    n.minobsinnode = 10)
model.gbm <- train(as.character(class) ~ ., data = billetes, 
               trControl = train.control, 
               tuneGrid = grid,
               method = "gbm")
# summarize results
print(model.gbm)
plot(model.gbm)    # Podría seguir un poco más en número de arboles pues el mínimo no está en el rango definido (CUIDADO PORQUE PUEDO CAPTURAR RUIDO)

table(billetes$class, predict(model.gbm, billetes))  # Perfecto a priori


# con svm


train.control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(degree = 1:5, 
                    scale = 1:3,
                    C = 10)
model.svm <- train(as.character(class) ~ ., data = billetes, 
               trControl = train.control, 
               tuneGrid = grid,
               method = "svmPoly")
# summarize results
print(model.svm)
plot(model.svm)

table(billetes$class, predict(model.svm, billetes))

# Ejercicio: ver ?models para ver quÃ© modelos hay y quÃ© parÃ¡metros admiten

?models

# Ejercicios: crear modelos para predecir las variables objetivo de alguno de estos
#   conjuntos de datos:
#   - https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset (regresiÃ³n)
#   - https://archive.ics.uci.edu/ml/datasets/Bank+Marketing (clasificaciÃ³n)
#   - https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients (clasificaciÃ³n)
#   - https://archive.ics.uci.edu/ml/datasets/Wine+Quality (regresiÃ³n)

# datos brutos (los dejo siempre en memoria "tal cual")

cards <- read.csv("default of credit card clients.csv", header = T, sep=";")

cards <- head(cards,200)
cards <- (tail(cards,199))
cards$Y <- factor(cards$Y)

cards$Y <- as.character(cards$Y)

summary(cards)
colnames(cards)

cards$Y <- factor(cards$Y)

# inspecciÃ³n rÃ¡pida
summary(cards)

mod.ctree <- ctree(Y ~ ., data = cards)

hplot(mod.ctree)

