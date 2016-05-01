#############################################################################
# Ciencia de datos - R - Parte 09: regresión lineal y logística
# cgb@datanalytics.com, 2016-02-12
#
# El objetivo de esta sesión es familiarizarse con las regresiones lineal y 
#   logística como ejemplos de lo que vendrá posteriormente
#############################################################################

#----------------------------------------------------------------------------
# modelos para pruebas de hipótesis
#----------------------------------------------------------------------------

tmp <- sleep
tmp$group <- factor(tmp$group)

# mediante pruebas de hipótesis
t.test(extra ~ group, data = tmp)
boxplot(extra ~ group, data = tmp) # como de efectivo es el tratamiento, cuanto duermen mas los tratados 

# mediante un modelo lineal
summary(lm(extra ~ group, data = tmp))

# otro ejemplo: http://www.msssi.gob.es/ssi/igualdadOportunidades/iEmpleo/Igualdad_salarial/Brecha_salarial_III.pdf
# véanse las páginas 159 y siguientes

#----------------------------------------------------------------------------
# modelo lineal
#----------------------------------------------------------------------------

extro <- read.table("lmm_data.txt", header = T, sep = ",")

modelo.0 <- lm(extro ~ open + agree + social, data = extro)

hist(residuals(modelo.0))

summary(modelo.0)

modelo.1 <- lm(extro ~ school + open + agree + social - 1, data = extro)
# modelo.1 <- lm(extro ~ school + agree - 1, data = extro) 
summary(modelo.1)

# Ejercicio: modelar el precio de la gasolina por provincia. 

df <- read.csv("carburantes_20160121.csv", header = TRUE, dec = ",", sep=";")
modelo.9 <- lm(Precio..Gasoleo.A ~ Provincia -1, data = df)
head (df)
# modelo.9 <- lm(Precio..Gasoleo.A ~ Latitud..WGS84.-1, data = df) # sale r-squared =1, pero no vale porque no es NORMAL!!!!!! (ceuta, canarias...)
summary(modelo.9)

# Ejercicio: ¿es relevante la provincia como variable predictora?

modelo.2 <- lm(extro ~ school:class + open + agree + social - 1, data = extro)
summary(modelo.2)

# Ejercicio: compara los modelos 1 y 2.

modelo.3 <- lm(extro ~ school * agree + open + social - 1, data = extro)
summary(modelo.3)

## Términos independientes variantes (varying intercepts)

n <- 1000
a <- sample(1:3, n, replace = T)
x <- runif(n)

y <- a/2 - x + rnorm(n, 0, .1)

datos <- data.frame(y = y, a = factor(a), x = x)

summary(lm(y ~ ., data = datos))

library(ggplot2)

ggplot(datos, aes(x = x, y = y)) + geom_point() + facet_grid(~ a) + 
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x)   # caso de libro, con pendientes paralelas en funci�n de variable categorica


# pendientes cambiantes

n <- 1000
a <- sample(1:3, n, replace = T)
x <- runif(n)

y <- a/2 + (a-1) * x + rnorm(n, 0, .2)

datos <- data.frame(y = y, a = factor(a), x = x)

mod.0 <- lm(y ~ ., data = datos)
mod.1 <- lm(y ~ a*x, data = datos)

anova(mod.0, mod.1)

summary(mod.1)

ggplot(datos, aes(x = x, y = y)) + geom_point() + facet_grid(~ a) + 
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) # tenemos una interaccion = pendientes diferentes segun la categorica


## Estudio de los efectos

install.packages("effects")

library(effects)

efecto <- effect("a*x", mod.1, xlevels = 10)
plot(efecto)


#----------------------------------------------------------------------------
# regresión logística
#----------------------------------------------------------------------------

admitidos <- read.table("admitidos.csv", header = T, sep = "\t")
admitidos$rank <- factor(admitidos$rank)

modelo.logistico <- glm(admit ~ .-1, data = admitidos, family = binomial)
summary(modelo.logistico)

# predicciones

boxplot(predict(modelo.logistico, type = "response") ~ admitidos$admit)

efecto <- effect("gre", modelo.logistico, xlevels = 10)   # probabilidad de admisi�n en funci�n de gpe (con una variabilidad dependiente del rsto de variables como rank, gpa)
plot(efecto)

efecto <- effect("rank", modelo.logistico, xlevels = 10)
plot(efecto)

efecto <- effect("gpa", modelo.logistico, xlevels = 10)
plot(efecto)

modelo.logistico.2 <- glm(admit ~ gpa + gre * rank, data = admitidos, family = binomial)
summary(modelo.logistico.2)

efecto <- effect("gre * rank", modelo.logistico.2, xlevels = 10)
plot(efecto)


# Ejercicio: crear unos datos en los que el efecto cruzado de una variable categórica con otra continua 
#   tiene un efecto discernible

library(plyr)

ucba <- as.data.frame(UCBAdmissions)

ucba <- ddply(ucba, .(Admit, Gender, Dept), function(x) x[rep(1, x$Freq),])
ucba$Freq <- NULL

# Ejercicio: modelar "Admit" en función de
#   1) El sexo

ucba$target <- ucba$Admit == "Admitted"
head(ucba)
modelo.logistico <- glm(target ~ Gender, data = ucba, family = binomial)
summary(modelo.logistico)

efecto <- effect("Gender", modelo.logistico)
plot(efecto)

#   2) Sexo + departamento

modelo.logistico <- glm(target ~ Gender + Dept, data = ucba, family = binomial)
summary(modelo.logistico)

efecto <- effect("Gender", modelo.logistico)
plot(efecto)

#   3) Sexo * departamento

modelo.logistico <- glm(target ~ Gender * Dept, data = ucba, family = binomial)
summary(modelo.logistico)

efecto <- effect("Gender*Dept", modelo.logistico)
plot(efecto)

# ¿Cuál es el mejor modelo? ¿Se leen en ellos historias distintas?

# Ejercicio: crear un modelo logístico para el conjunto de datos de los indios Pima
#   https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes

# Ejercicio: usar rstan para crear un modelo logístico (con alguno de los datos anteriores)

