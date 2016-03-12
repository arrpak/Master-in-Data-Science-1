
library(reshape2)
library(plyr)

library(ggplot2)
library(ggmap)

setwd("~/00_dsc/Repository/Master-in-Data-Science/Data")

# Ejercicio: haz un diagrama de cajas de las temperaturas en NY por mes (sin facetas)

head(airquality)
boxplot(airquality$Temp ~ airquality$Month)

ggplot(airquality, aes(y=Temp, x=factor(Month))) + geom_boxplot(aes(fill=factor(Month)))
ggplot(airquality, aes(y=Temp, x=factor(Month))) + geom_jitter(aes(colour = Temp),width = 0.5)

# Ejercicio: haz un histograma de las temperaturas en NY por mes (con facetas)

ggplot(airquality, aes(x=Temp)) + geom_histogram(fill='blue', col='blue') + facet_grid(Month~.)


# Ejercicio: prueba con los gráficos de violín (que son una mezcla de los dos anteriores)

ggplot(airquality, aes(y=Temp, x=factor(Month))) + geom_violin(col='blue',fill='blue')


# Ejercicio: superpón las distribuciones de las temperaturas de NY por mes como en 
#   http://www.datanalytics.com/2015/07/09/son-normales-las-alturas-de-los-individuos/

ggplot(airquality, aes(Temp,fill=factor(Month))) + geom_density(alpha=0.5)
