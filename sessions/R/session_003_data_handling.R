#############################################################################
# Ciencia de datos - R - Parte 03: manipulaci�n avanzada de datos con R
# cgb@datanalytics.com, 2015-06-017
#
# El objetivo de esta sesi�n es recorrer aprender a manipular datos usando dos
# paquetes importantes de R: reshape2 y plyr
#############################################################################

#############################################################################
# reshape2
#############################################################################

# Instalaci�n:
install.packages("reshape2")
install.packages("plyr")

# Nota: tambi�n puedes usar los men�s de RStudio para instalar paquetes (Tools...)

# Carga:
library(reshape2)
library(plyr)

#----------------------------------------------------------------------------
# formato largo (melted)
#----------------------------------------------------------------------------

pob.aragon.2014 <- read.table("pob_aragon_2014.csv", header = T, sep = "\t")
pob.aragon.2014

melt(pob.aragon.2014)          # mismos datos en otro formato... �formato largo!

# Ejercicio: pasa el tiempo que consideres necesario para entender muy bien:
#   - c�mo se ha transformado pob.aragon.2014
#   - que la informaci�n contenida en ambos conjuntos de datos es la misma

pob.aragon <- read.table("pob_aragon.csv", header = T, sep = "\t")
pob.aragon

melt(pob.aragon)               # ¡horrible!
melt(pob.aragon, id.vars = c("Provincia", "Periodo"))   # Ahora s�

# Ejercicio: �qu� pasa si alteras el orden de provincia y periodo?

melt(pob.aragon, id.vars = c("Periodo","Provincia"))   # Ahora s�

pob.aragon.long <- melt(pob.aragon, id.vars = c("Provincia", "Periodo")) 

# Una peque�a digresi�n:
arrange(pob.aragon.long, Periodo, Provincia)     # �te gusta m�s ordenar as�?

# Nota: la funci�n arrange est� en el paquete plyr...

# Ejercicio: busca en ?arrange c�mo ordenar descendentemente

# Ejercicio: toma el conjunto de datos airquality y disponlo en formato largo

summary(airquality)
melt(airquality, id.vars=c("Month","Day"))
  
# Ejercicio: calcula el valor mediano (median) de las variables de long.airquality

df <- melt(airquality, id.vars=c("Month","Day"))
cbind(tapply(df$value, df$variable, mean, na.rm = TRUE))

#----------------------------------------------------------------------------
# Formato ancho (cast)
#----------------------------------------------------------------------------

pob.aragon.2014.largo <- melt(pob.aragon.2014)
pob.aragon.2014.largo

# a partir del formato largo se puede pasar a distintos tipos de formatos anchos:

dcast(pob.aragon.2014.largo, Provincia ~ variable) # provincia en columna - variable en columnas
dcast(pob.aragon.2014.largo, variable ~ Provincia) # al rev�s

# Agregaciones

iris.long <- melt(iris)
head(iris.long)

dcast(iris.long, Species ~ variable) # 50 quiere decir que hay un vector de 50 valores

# Ejercicio: �qu� ha pasado?

dcast(iris.long, Species ~ variable, fun.aggregate = mean) 

dcast(iris.long, Species ~ variable, value.var = "value", fun.aggregate = mean)  # en el anterior comando ha adivinado el valor

# Nota: generalmente, no hay que especificar "value.var": dcast la adivina. Pero a veces se
#   equivoca, por lo que...


paro <- read.table("paro.csv", header = T, sep = "\t")

# vamos a arreglar un poco los datos (los detalles, m�s adelante)
paro$Periodo <- gsub("IV",  "4", paro$Periodo)
paro$Periodo <- gsub("III", "3", paro$Periodo)
paro$Periodo <- gsub("II",  "2", paro$Periodo)
paro$Periodo <- gsub("I",   "1", paro$Periodo)

paro$Situation <- as.character(paro$Situation)

paro$Situation[paro$Situation == "Active population"]   <- "active"
paro$Situation[paro$Situation == "Inactive persons"]    <- "inactive"
paro$Situation[paro$Situation == "Unemployed persons"]  <- "unemployed"
paro$Situation[paro$Situation == "Employed persons"]    <- "employed"
paro$Situation[paro$Situation == "Parados que buscan primer empleo"]    <- "never_employed"

paro$Situation <- factor(paro$Situation)

# paro est� en formato largo, pero...
head(paro)
paro.alt <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
head(paro.alt)

# Ejercicio: a�ade a paro.alt una columna adicional con la tasa de paro (desempleados entre
#   poblaci�n activa)

head(paro.alt)
# paro.alt$unemp_rate <- NULL
paro.alt$unemployed_rate <- round(paro.alt$unemployed / paro.alt$active,2) # mucho m�s f�cil que sin dcast
head(paro.alt)
sum(paro.alt$unemployed) / sum(paro.alt$active)

# Nota: este ejercicio demuestra que en ocasiones es bueno crear un determinado tipo de formato
#   largo para crear nuevas variables f�cilmente.

# Ejercicio: agrega los datos del paro para toda Espa�a usando dcast y fun.aggregate = sum.
#   Pista: si ignoras la provincia en dcast se producen "duplicados"

head(paro.alt)
paro.alt <- dcast(paro, Periodo ~ Situation, fun.aggregate = sum, na.rm=TRUE) # Evoluci�n en el tiempo
paro.alt$unemployed_rate <- paro.alt$unemployed / paro.alt$active*100

plot(paro.alt$unemployed_rate,type='l')

# Ejercicio: identifica las provincias y periodos en los que la tasa de paro masculina es
#   mayor que la femenina (nota: la tasa de paro es "unemployed" dividido por "active")


#----------------------------------------------------------------------------
# plyr: procesamiento de tablas por trozos
#----------------------------------------------------------------------------

# la expresi�n fundamental: (pensado para formato largo)

head(paro)
res <- ddply(paro, .(Gender, Periodo, Situation), summarize, total = sum(value))
head(res)


# elementos de la expresi�n anterior:
# ddply: transforma una tabla en otra tabla
# paro: un dataframe
# .(...): variables de la tabla de entrada por las que se parte 
# summarize: cualquier funci�n que opera sobre tablas
# total = ...: argumentos de la funci�n

# Ejercicio: pon airquality en formato largo y saca la media y la mediana de cada variable por mes

# primero pongo en formato largo

head(airquality)
paro.long <- melt(airquality, id.vars=c("Month","Day"), na.rm = TRUE)

head(paro.long)
ddply(paro.long, .(variable), summarize, mean = mean(value))
ddply(paro.long, .(variable), summarize, median = median(value))
ddply(paro.long, .(variable), summarize, mean = mean(value), median = median(value))

ddply(paro.long, .(Month,variable), summarize, mean = mean(value), median = median(value))

# mirar ddply(paro.long, .(variable), function(x) summarize(x,median = median(value)))

# Ejercicio: sacar media y mediana de la longitud del petalo por especie

head(iris)
ddply(iris, .(Species), summarize, mean = mean(Petal.Length), median = median(Petal.Length))

geo_mean <- function(x) exp(mean(log(x)))

# Ahora la media geometrica (exp(mean(log)))
ddply(iris, .(Species), summarize, geo_mean(Petal.Length))


# otras funciones que se pueden usar en ddply:
foo <- function(x) lm(Temp ~ Solar.R, data = x)$coefficients
ddply(airquality, .(Month), foo)

# En general, insisto, la funci�n puede ser cualquiera que admita como argumento una tabla
# Los dem�s argumentos de la funci�n (arbitraria) se pasan a trav�s de ddply (detr�s de la llamada a
#   la funci�n)

# variantes de la f�rmula anterior: dlply
res <- dlply(airquality, .(Month), function(x) lm(Temp ~ Solar.R, data = x))  # una lista!
lapply(res, coefficients)
ldply(res, coefficients)

# existen tambi�n llply, laply, alply... e incluso d_ply

# ejercicio: completa la funci�n siguiente y �sala para guardar un gr�fico de la relaci�n entre la temperatura
# y la irradiaci�n solar en cada mes

foo <- function(x){
  nombre.fichero <- paste0(unique(x$Month), ".png")
  png(nombre.fichero)
    plot(x$Solar.R, x$Temp, main = "...", xlab = "...", ylab = "...")
    abline(lm(Temp ~ Solar.R, data = x), col = "red")     
  dev.off()
}

# transformaciones por trozos

tasa.paro <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
tasa.paro <- transform(tasa.paro, rate = unemployed / active)
tasa.paro <- tasa.paro[, c("Gender", "Provinces", "Periodo", "rate")]
head(tasa.paro)
# Para seleccionar el perido de mayor tasa de paro en cada provincia y sexo, con plyr,
tmp <- ddply(tasa.paro, .(Gender, Provinces), transform, rank = rank(-tasa.paro, ties = "random"))
res <- tmp[tmp$rank == 1,]

ddply(tasa.paro, .(Gender,Provinces), transform, rango =rank(rate))
# Ejercicio: selecciona en cada provincia el periodo en el que fue m�ximo el n�mero total (hombres + mujeres) de parados


# Un ejemplo de regresiones por trozos y asignar el valor predicho.
# Usamos data de http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt
dat <- read.table("lmm_data.txt", header = T, sep = ",")

dat.preds <- ddply(dat, .(school), transform,    
                   pred = predict(lm(extro ~ open + agree + social + class))) # en este caso transform a�ade columna

# Todos los modelos tiene un predict

nrow(dat.preds)
nrow(dat)
# Alternativamente:

foo <- function(x){
  modelo <- lm(extro ~ open + agree + social + class, data = x)
  res <- x
  res$preds <- predict(modelo, new.data = x)
  res
}

dat.preds <- ddply(dat, .(school), function(x) foo(x))
dat.preds <- ddply(dat, .(school), foo)

# Practicar con las gasolineras, precio por provincia, por distribuidor

gasdf <- read.csv("carburantes_20050222.csv", sep="\t", header=TRUE, dec=",")
head(gasdf)
summary(gasdf)

sddf <- ddply(gasdf, .(Provincia), summarize, sd = sd(Precio.Gasoleo.A, na.rm = TRUE))

sddf[order(sddf$sd),]
