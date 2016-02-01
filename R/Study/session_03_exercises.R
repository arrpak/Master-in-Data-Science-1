
# Ejercicio: toma el conjunto de datos airquality y disponlo en formato largo

head(airquality)
df01<- melt(airquality, id.vars = c("Day","Month"), na.rm = TRUE)
head(df01)

# Ejercicio: calcula el valor mediano (median) de las variables de long.airquality

tapply(df01$value,df01$variable,median)

# Ejercicio: añade a paro.alt una columna adicional con la tasa de paro (desempleados entre
#   población activa)

df2 <- read.csv("paro.csv",header = TRUE, sep='\t', encoding = 'latin1')
df3 <- dcast(df2, Gender + Provinces + Periodo ~ Situation)
colnames(df3) <- c("Gender","Provinces","Period","Active","Employed","Inactive","First","Unemployed")
head(df3)
df3$Rate <- (df3$Unemployed / df3$Active * 100)

# Ejercicio: agrega los datos del paro para toda España usando dcast y fun.aggregate = sum y su evolución en el tiempo
#   Pista: si ignoras la provincia en dcast se producen "duplicados"

df2 <- read.csv("paro.csv",header = TRUE, sep='\t', encoding = 'latin1')
df4 <- dcast(df2, Periodo ~ Situation, fun.aggregate = sum, na.rm = TRUE)
colnames(df4) <- c("Period","Active","Employed","Inactive","First","Unemployed")
df4$Rate = df4$Unemployed / df4$Active *100

plot(df4$Rate,type='l')

# Ejercicio: identifica las provincias y periodos en los que la tasa de paro masculina es
#   mayor que la femenina (nota: la tasa de paro es "unemployed" dividido por "active")

# By Periods

df2 <- read.csv("paro.csv",header = TRUE, sep='\t', encoding = 'latin1')
df5 <- dcast(df2, Gender + Periodo ~ Situation, fun.aggregate = sum, na.rm = TRUE)
df5$Rate = df5$Unemployed / df5$Active *100
df6 <- dcast(df5[,c(1,2,8)], Periodo ~ Gender)

df6[df6$Males > df6$Females,]
barplot(df6$Females-df6$Males, ylim=c(-0.5,3), col='blue', density=30, main="Female - Male\nUnemployment Rate")
grid()

# By Provinces

df7 <- dcast(df2, Gender + Provinces ~ Situation, fun.aggregate = sum, na.rm = TRUE)
df7$Rate = df7$Unemployed / df7$Active *100
df8 <- dcast(df7[,c(1,2,8)], Provinces ~ Gender)

df8[df8$Males > df8$Females,]
barplot(df8$Females-df8$Males, ylim=c(-5,15), col='blue', density=30, main="Female - Male\nUnemployment Rate")
grid()

# Ejercicio: sacar media y mediana de la longitud del petalo por especie using ddply

head(iris)
ddply(iris, .(Species), summarize, pl_mean = mean(Petal.Length), pl_median = median(Petal.Length))

# Ejercicio: selecciona en cada provincia el periodo en el que fue máximo 
# el número total (hombres + mujeres) de parados

df2 <- read.csv("paro.csv",header = TRUE, sep='\t', encoding='latin1')

df2$Situation <- as.character(df2$Situation)
df2$Situation[df2$Situation == "Employed persons"]    <- "employed"

df9 <- dcast(df2, Provinces + Periodo ~ Situation, fun.aggregate = sum, na.rm=TRUE)
colnames(df9)=c("Provinces","Period","Active","Employed","Inactive","First","Unemployed")
df10 <- ddply(df9, .(Provinces), transform, unemp_rank=rank(-Unemployed))
df10[df10$unemp_rank == 1,][,c(1,2,7)]

# Ejercicio:Practicar con fichero de gasolineras

library(reshape2)
library(plyr)

library(ggplot2)
library(ggmap)

setwd("~/00_dsc/Repository/Master-in-Data-Science/Data")

gasdf <- read.csv("carburantes_20160121.csv", header=TRUE, sep=";", dec=',')
head(gasdf)

# Precio medio Gasoleo A por provincia

df20 <- ddply(gasdf, .(Provincia), summarize, mean_GA = round(mean(Precio..Gasoleo.A, na.rm=TRUE),3),
              ds_GA = round(sqrt(var(Precio..Gasoleo.A, na.rm=TRUE)),3))

arrange(df20,-mean_GA) # More expensive provincies
arrange(df20,-ds_GA)   # Provincies with more competence

# Gasolineras con el precio menor de Gasoleo A de cada provincia

df21 <- gasdf[,c(1,3,8,9,11)]
colnames(df21) <- c("provincia","municipio","longitud","latitud","precio_gasoleo_a")
head(df21)

df23 <- (ddply(df21, .(provincia), transform, rango_gasoleo_a = rank(precio_gasoleo_a, ties='random')))
df23 <- df23[df23$rango_gasoleo_a == 1,][,-6]

df24 <- (ddply(df21, .(provincia), transform, rango_gasoleo_a = rank(-precio_gasoleo_a, ties='random')))
df24 <- df24[df24$rango_gasoleo_a == 1,][,-6]


central.loc <- get_map( location = c(-3.686667,40.28311),
                        color = "color",
                        maptype = "roadmap",
                        scale = 1,
                        zoom = 6)

# lo represento
ggmap(central.loc) 

# le añado puntos
ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                               data = df23, colour = 'green',
                               size = 2)
ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                               data = df24, colour = 'red',
                               size = 2)

df25 <- df21[df21$precio_gasoleo_a > mean(df21$precio_gasoleo_a,na.rm=TRUE),]
df26 <- df21[df21$precio_gasoleo_a < mean(df21$precio_gasoleo_a,na.rm=TRUE),]

ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                               data = df25, colour = 'red',
                               size = 2)

ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                               data = df26, colour = 'green',
                               size = 2)

df27 <- df21[df21$precio_gasoleo_a == max(df21$precio_gasoleo_a, na.rm = TRUE),]

ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                                data = df27, colour = 'red',
                                size = 12)

df28 <- df21[df21$precio_gasoleo_a == min(df21$precio_gasoleo_a, na.rm = TRUE),]

ggmap(central.loc) + geom_point(aes(x = longitud, y = latitud),
                                data = df28, colour = 'red',
                                size = 12)

