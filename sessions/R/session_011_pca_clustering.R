#############################################################################
# Ciencia de datos - R - Parte 11: PCA y clústering
# cgb@datanalytics.com, 2016-02-14
#
# El objetivo de esta sesión es familiarizarse con el PCA y los métodos de
#   clústering
#############################################################################

###############################################################################################
# pca
###############################################################################################

res <- princomp(USArrests)

comp.2 <- res$scores[,1:2]       # dos columnas en lugar de 4

# como comp = m * loadings... hay que invertir la matriz de atribuciones
# y sumar la media 

tmp <- solve(res$loadings)
reconstruccion <- comp.2 %*% tmp[1:2,]
reconstruccion <- t(t(reconstruccion) + res$center)

head(reconstruccion)
head(USArrests)

plot(USArrests[,1], reconstruccion[,1])
plot(USArrests[,2], reconstruccion[,2])


# ¿Veis los efectos de la escala de las variables?

res <- princomp(USArrests, cor = TRUE)

# ahora hay menos varianza en la primera componente que antes
plot(res)

comp.2 <- res$scores[,1:2]       # dos columnas en lugar de 4

# como comp = m * loadings... hay que invertir la matriz de atribuciones
# y sumar la media y dividir por la sd

tmp <- solve(res$loadings)
reconstruccion <- comp.2 %*% tmp[1:2,]
reconstruccion <- t(t(reconstruccion) * res$scale + res$center)

head(reconstruccion)
head(USArrests)

plot(USArrests[,1], reconstruccion[,1])
plot(USArrests[,2], reconstruccion[,2])


#----------------------------------------------------------------------------
# Biplots
#----------------------------------------------------------------------------

biplot(res)


###############################################################################################
# clústering
###############################################################################################

#----------------------------------------------------------------------------
# Distancias: problemas de escala
#----------------------------------------------------------------------------

foo <- function(dat, cluster){
  plot(dat[,1], dat[,2], col = cluster$cluster, asp = 1)
}

# vértices de un cuadrado:
data <- matrix(c(10, 11, 10, 11, 21, 20, 20, 21), 4, 2)

data.rescale <- data
data.rescale[,2] <- 3 * data.rescale[,2]
cluster <- kmeans(data.rescale, 2)
foo(data.rescale, cluster)

data.rescale <- data
data.rescale[,1] <- 3 * data.rescale[,1]
cluster <- kmeans(data.rescale, 2)
foo(data.rescale, cluster)

# ¿reescalar?
fivenum(scale(c(1, rep(0, 100))))  # el 1 se convierte en su propio cluster (MAL en general, porque quieres hacer clusteres en funci�n de varias dimensiones, variables)

# Ejercicio: ¿qué ha sucedido? ¿Cómo varía el rango de la variable 
#   según el porcentaje de unos (vs ceros)?

# Ejercicio: ¿qué efecto tienen las variables 0-1 con pocos unos en k-means?

# Ejercicio: reescala x <- rcauchy(100) en el intervalo [0,1] 
# Ejercicio: crea una función que admita un vector numérico y lo reescale en [0,1]

normaliza <- function(x) (x - min(x)) / (max(x) - min(x))

fivenum(normaliza(rcauchy(100))) # Es parecido al tema de los Goya o si tengo como cliente a Bot�n y miro por sueldo (moraleja: hay que pensar y no hacer churrera con los escalados, escalados)


# Ejercicio: ¿qué problemas puede crear reescalar en [0,1]?


#----------------------------------------------------------------------------
# Una digresión: spectral clústering
#----------------------------------------------------------------------------

library(kernlab)

data(spirals)
plot(spirals)

sc <- specc(spirals, centers=2)

plot(spirals, col=sc)


#----------------------------------------------------------------------------
# k-medias
#----------------------------------------------------------------------------

library(jpeg)
library(cluster)

# leo una foto usando readJpeg de jpeg
# el objeto devuelto es un array mxnx3 dimensional
# la última dimensión es el rgb de cada pixel

tmp <- tempfile()
# download.file("http://www.frasesypoemas.com/sites/www.frasesypoemas.com/files/imagecache/normal/amelie.jpg", tmp)
download.file("file:///C:/Users/IBM_ADMIN/Downloads/45187-sotillo-de-la-adrada-canto-de-los-pollitos.jpg", tmp)


x <- readJPEG(tmp)

# si quieres mostrar la foto como un gráfico:
plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "")
rasterImage(x, 0, 0, 100, 100)

# convertimos el array 3D nxmx3 en uno 2D (nm)x3
# luego buscamos 5 clústers
# esencialmente, buscamos 5 "píxels representativos"
d   <- dim(x)
res <- kmeans(array(x, dim = c(d[1] * d[2], d[3])), 5)

# reemplazamos cada rgb de cada cluster por su 
# "píxel representativo" (medioide) correspondiente
rgb.clusters <- res$centers[res$cluster,]   # sustituyo cada pixel por el protoruivo o representante

# convertimos la matriz resultante en un array 3D 
# (invirtiendo la transformación anterior)
# y representamos gráficamente
plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "")
rasterImage(array(rgb.clusters, dim = d), 0, 0, 100, 100)

# Ejercicio: investigar el contendio de res y entender sus componentes.

# Ejercicio: crear distintos agrupaciones con distintos número de componentes y ver cómo evoluciona el
#   "withinss"

# clústers de series temporales

install.packages("tseries")
install.packages("reshape")
library(tseries)
library(zoo)
library(XML)
library(reshape)
library(ggplot2)

foo  <- function( simbolo, final = Sys.time(), profundidad = 30 * 24 * 3600 ){
  precios <- get.hist.quote(instrument= simbolo, start = final - profundidad,
                            end = final, quote=c("AdjClose"),
                            provider="yahoo", origin="1970-01-01",
                            compression="d", retclass="zoo")
  colnames(precios) <- simbolo
  return(precios)
}

# lista de símbolos del ibex

tmp <- readHTMLTable("http://finance.yahoo.com/q/cp?s=%5EIBEX+Components")[[5]]
tmp <- as.character(tmp$V1[-(1:6)])
tmp <- gsub("-P", "", tmp)
simbolos <- tmp[tmp != "ABG.MC"]

ibex <- do.call(merge, sapply(simbolos, foo, simplify = F))

ibex.scaled <- scale(ibex)

ibex.df <- data.frame(ibex.scaled, fecha = index(ibex.scaled))
ibex.df <- melt(ibex.df, id.vars = "fecha")
ibex.df <- ibex.df[ order(ibex.df$fecha, ibex.df$variable), ]
ibex.df$cluster <- kmeans(data.frame(t(ibex.scaled)), 4)$cluster

ggplot(ibex.df, aes(x=fecha, y=value, group=variable)) + 
  geom_line() + facet_wrap(~cluster)


# Ejercicio: comprender el código anterior

# Ejercicio: entender el efecto de scale en los datos. ¿Es adecuado?

# Ejercicio: identifica el nombre de las acciones de cada grupo

# Usa el within-between para tratar de averiguar si 4 es el número adecuado de grupos. 
#   ¿Cuántos usarías?



#----------------------------------------------------------------------------
# alternativas a k-medias (pam y clara)
#----------------------------------------------------------------------------

# Ejercicio: usa clara o pam en lugar de kmeans en alguno de los ejercicios de la bolsa o las fotos.

# Ejercicio: consulta ?pam y ?clara para averiguar más sobre su funcionamiento.

# Ejercicio (para casa): lee http://www.jstatsoft.org/v01/i04/paper para averiguar más sobre estos 
#   algoritmos.

#----------------------------------------------------------------------------
# Una aplicación "real" (ejercicio) MUY IMPORTANTE HACEE - LLEVAR� TIEMPO CUIDADo
#----------------------------------------------------------------------------

# busca en las páginas del INE los microdatos de la encuesta de estructura laboral, EES, del 2010.
# luego,

library(MicroDatosEs)
ees <- ees2010("/tmp/EES10_WEB")       # reemplaza esta ruta por aquella donde estén tus datos
ees <- as.data.frame(ees) 

# prueba con kmeans, pam, etc.
# trata de comprender los resultados
# p.e., res <- pam(ees[1:100,], k = 3)
# nota: trata de ejecutar tus algoritmos sobre subconjuntos de filas primero (¡la tabla es grande!)
# nota: ¿qué variables son relevantes y cuáles no?





#----------------------------------------------------------------------------
# clústering jerárquico
#----------------------------------------------------------------------------

# Ejercicio: Usa hclust para realizar un clústering jerárquico de iris (por las 4 primeras variables).
# Nota: busca en ?hclust (mejor en los ejemplos) cómo aplicar la función (con las opciones más simples, 
#   por defecto).

# Nota: en iris hay tres grupos claros (las tres especies). ¿Puedes identificarlas en el dendrograma?

# Ejercicio: Haz lo mismo con agnes (de cluster)

# Ejercicio: lo mismo con diana y mona (de cluster)

# Ejercicio: usa rect.hclusto cutree para "cortar" el dendrograma (creado por hclust) a la 
#   altura deseada.

# Ejercicio: construye un heatmap con las cuatro primeras columnas de iris; ¿deberías normalizar? 



#----------------------------------------------------------------------------
# calidad de los clústers
#----------------------------------------------------------------------------

# Ejercicio: crea un clúster con pam (foto o series temporales) y aplícale la función silhouette
#   (y plot-silhouette).

# Ejercicio: haz clústers (pam, clara, kmeans) de las cuatro primeras columnas de iris y estudia la 
#   pureza de los clústers resultantes (en función de Species).

install.packages("cluster")
library("cluster")
newiris <- iris
newiris$Species <- NULL
(kc <- kmeans(newiris, 2)) 

# plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
# points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:4, pch=16, cex=2)

plot(newiris[c("Sepal.Length", "Petal.Length")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Petal.Length")], col=1:2, pch=16, cex=4)

#----------------------------------------------------------------------------
# descripción de clústers
#----------------------------------------------------------------------------

# solo dos variables (por ejemplo)
my.vars <- iris[, 1:2]
my.cluster <- kmeans(my.vars, 3)

# ¿cómo distribuye el clúster el resto de las variables?
dat <- iris
dat$cluster <- my.cluster$cluster

tmp <- dat
tmp$Species <- NULL
tmp <- melt(tmp, id.vars = "cluster")

ggplot(tmp, aes(x = value)) + geom_histogram() + facet_grid(cluster ~ variable)
ggplot(dat, aes(x = Species)) + geom_bar() + facet_grid(cluster ~ .)

# Ejercicio: crear un "informe" que consista en una diapositiva por clúster que ayude a describirlo.

# Ejercicio: ¿cómo nos ayuda este "informe" a entender la calidad del clúster?

# Ejercicio: haz el ejercicio anterior mal hecho: muestra en un "informe" el valor medio de cada 
#   variable dentro de cada clúster. La función MASS:parcoord puede ayudarte.
