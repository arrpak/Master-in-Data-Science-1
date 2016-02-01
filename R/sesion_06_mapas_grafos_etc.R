#############################################################################
# Ciencia de datos - R - Parte 05: mapas, grafos, series temporales, etc.
# cgb@datanalytics.com, 2016-01-22
#
# La sesión tiene como objetivo aprender a manejar información menos habitual:
#   mapas, grafos/redes sociales, texto y series temporales
#############################################################################

#----------------------------------------------------------------------------
# ggmap
#----------------------------------------------------------------------------

library(ggmap)

# geocode descarga las coordenadas de una ubicación
unizar <- geocode('Universidad de Zaragoza, Zaragoza, España', source = "google")

unizar         # coordenadas de la universidad de Zaragoza

# ahora descargamos un mapa de la zona:

map.unizar <- get_map(location = as.numeric(unizar),
                      color = "color",
                      maptype = "roadmap",
                      scale = 2,
                      zoom = 16)

# Ejercicio: prueba con otras ubicaciones, otros zums, otros tipos de mapas (?get_map)

# Puedes pintar puntos encima:

ggmap(map.unizar) + geom_point(aes(x = lon, y = lat),
                               data = unizar, colour = 'red',
                               size = 4)


# Ejercicio: prueba a pintar las gasolineras en el mapa de España. 

# También puedes pintar rutas:

mapa <- get_map("Madrid", source = "stamen", maptype = "toner", zoom = 12)
ruta <- route(from = "Puerta del Sol, Madrid", to = "Plaza de Castilla, Madrid")
ggmap(mapa) + 
  geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
            colour = "red", size = 2, data = ruta)


# Ejercicio y referencia: lee https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf y mira qué 
#   otras cosas (además de puntos) pueden representarse sobre un mapa


#----------------------------------------------------------------------------
# shapefiles
#----------------------------------------------------------------------------

library(rgdal)

sport <- readOGR(dsn = "data/london_sport", "london_sport")

class(sport)

# un objeto de la clase S4 tiene "slots", que son las variables del objeto 
slotNames(sport)

# la más importante de este tipo de clases es "data"
sport@data

# en efecto, un objeto de la clase "SpatialPolygonsDataFrame" es un dataframe más una lista de "polígonos" asociada
# a cada fila que permite representarlos en un mapa:

sport@data[sport$Partic_Per < 15, ]

plot(sport[sport$Partic_Per > 25, ]) 

plot(sport)
plot(sport[sport$Partic_Per > 25, ], col = "blue", add = TRUE)


plot(sport, col = sport$Pop_2001)

summary(sport)

# ejercicio:
# añadir una columna al objeto sport y representarla gráficamente
# con la intensidad según el valor asociado
# pista: visitar http://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf y buscar el mapa de interés

# ejercicio avanzado:
#   descargar los shapefiles de provincias del INE
#   obtener datos de algún tipo de estadistica por provincias (¿paro? ¿población?)
#   crear un "SpatialPolygonsDataFrame" con esa información
#   representarla gráficamente

# ejercicio: ver si se pueden combinar shapefiles con ggmap


#----------------------------------------------------------------------------
# grafos
#----------------------------------------------------------------------------

library(XML)
library(plyr)
library(igraph)
library(ggmap)
library(popgraph)

# bajamos y procesamos un XML
tmp <- readLines("https://guifi.net/en/guifi/cnml/2435/detail")
tmp <- xmlParse(tmp)

# extraemos los "nodos" usando xpath
nodos <- xpathApply(tmp, "//*/node")

# extraemos los atributos de los nodos
lista.nodos <- rbind.fill(lapply(
  nodos, function(x) data.frame(t(xmlAttrs(x)), 
                                stringsAsFactors = FALSE)))

head(lista.nodos)

# ahora los enlaces
lista.links <- lapply(nodos, function(x){
  from <- xmlAttrs(x)["id"]
  to   <- xpathApply(x, ".//link", xmlAttrs)
  
  if (length(to) == 0)
    return(NULL)
  
  to <- sapply(to, function(x) x["linked_node_id"])
  data.frame(from = from, to = to, stringsAsFactors = FALSE)
})

# lo anterior es una lista de tablas; ahora las apilamos todas
lista.links <- do.call(rbind, lista.links)

# fitramos cosas que "sobran"
lista.links <- lista.links[lista.links$from %in% lista.nodos$id,]
lista.links <- lista.links[lista.links$to   %in% lista.nodos$id,]
lista.links <- lista.links[lista.links$to != lista.links$from,]

# ¡creamos el grafo!
g <- graph.data.frame(lista.links, directed = F, lista.nodos)
g.working <- subgraph(g, V(g)$status %in% c("Working"))

# representación gráfica
my_layout <- layout.fruchterman.reingold(g.working)
plot(g.working, layout = my_layout, vertex.label = NA, vertex.size = 0.3)

# mayor componente conexa
kk <- clusters(g.working)

# Ejercicio: estudia el objeto kk

g.wc <- subgraph(g.working, kk$membership == which.max(kk$csize))
g.wc <- set.edge.attribute(g.wc, name = "betweenness", E(g.wc), 
                           edge.betweenness(g.wc))
g.wc <- set.vertex.attribute(g.wc, name = "btw", 
                             V(g.wc), betweenness(g.wc))


my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, 
     vertex.size = (1 + V(g.wc)$btw) / 3000)

geom.layout <- cbind(as.numeric(V(g.wc)$lon),
                     as.numeric(V(g.wc)$lat))
plot(g.wc, layout = geom.layout, vertex.label = NA, 
     vertex.size = (1 + V(g.wc)$btw) / 3000)

# aún más, los pintamos sobre el mapa de Barcelona

map.bcn <- get_map("Barcelona", maptype="satellite", zoom = 12)
tmp <- as.popgraph(g.wc)
tmp <- set.vertex.attribute(tmp, name = "Longitude", V(tmp), 
                            value = as.numeric(V(tmp)$lon))
tmp <- set.vertex.attribute(tmp, name = "Latitude", V(tmp), 
                            value = as.numeric(V(tmp)$lat))
p <- ggmap(map.bcn) 
p <- p + geom_edgeset(aes(x = Longitude, y = Latitude), tmp, color="white" )
p <- p + geom_nodeset(aes(x = Longitude, y = Latitude, size = btw), tmp)
p



#----------------------------------------------------------------------------
# grafos
#----------------------------------------------------------------------------

library(tseries)
library(rvest)
library(reshape2)
library(ggplot2)
library(zoo)



foo  <- function( simbolo, final = Sys.time(), profundidad = 30 * 24 * 3600 ){
  precios <- get.hist.quote(instrument= simbolo, start = final - profundidad,
                            end = final, quote=c("AdjClose"),
                            provider="yahoo", origin="1970-01-01",
                            compression="d", retclass="zoo")
  colnames(precios) <- simbolo
  return(precios)
}

# lista de símbolos del ibex

tmp <- read_html("http://finance.yahoo.com/q/cp?s=%5EIBEX+Components")
tmp <- html_nodes(tmp, "table")

tmp <- html_table(tmp[[9]])
simbolos <- tmp$Symbol

ibex <- do.call(merge, sapply(simbolos, foo, simplify = F))

head(ibex)
class(ibex)

ibex.scaled <- scale(ibex)

# clustering de series temporales
ibex.df <- data.frame(ibex.scaled, fecha = index(ibex.scaled))
ibex.df <- melt(ibex.df, id.vars = "fecha")
ibex.df <- ibex.df[ order(ibex.df$fecha, ibex.df$variable), ]
ibex.df$cluster <- kmeans(data.frame(t(ibex.scaled)), 4)$cluster

ggplot(ibex.df, aes(x=fecha, y=value, group=variable)) + 
  geom_line() + facet_wrap(~cluster)

# operaciones en series temporales
plot(ibex[,1:4])
plot(ibex[,1:4], plot.type = "single")

# diferencias entre los valores de un día y el siguiente
plot(diff(ibex[,1:4]))

# applicar una función en ventanas
plot(rollapply(ibex, 10, sd)[,1:4])

# inicio y fin
start(ibex)
end(ibex)

# una subselección
window(ibex, start = Sys.Date() - 7, end = Sys.Date())


library(xts)

ibex.xts <- as.xts(ibex)

# seleccionar solo enero
ibex.xts["2016-01"]

first(ibex.xts, "1 week")
last(ibex.xts, "1 week")

# ejercicio: ver https://cran.r-project.org/web/packages/xts/vignettes/xts.pdf y practicar 
#   las funciones que aparecen ahí


#----------------------------------------------------------------------------
# texto
#----------------------------------------------------------------------------

quijote <- readLines("http://www.gutenberg.org/cache/epub/2000/pg2000.txt")

# Ejercicio 1: encuentra las palabras más frecuentes en la obra. Ten en cuenta mayúsculas, etc. Además, que 
#   hay texto en inglés al principio y al final,...

# Ejercicio 2: encuentra los bigramas del Quijote, es decir, parejas de palabras consecutivas (y no separadas 
#   por puntos, comas u otros separadores). Los bigramas y los trigramas son fundamentales para el análisis de 
#   texto natural.