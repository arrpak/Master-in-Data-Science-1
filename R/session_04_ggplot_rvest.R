#############################################################################
# Ciencia de datos - R - Parte 04: ggplot2, "web scraping" y "varios"
# cgb@datanalytics.com, 2015-06-17
#
# El objetivo de esta sesiÃ³n es aprender a crear grÃ¡ficos avanzados con ggplot2,
#   y descargar y limpiar datos bajados de internet
#############################################################################

#----------------------------------------------------------------------------
# ggplot2
#----------------------------------------------------------------------------



library(ggplot2)

paro <- read.table("paro.csv", header = T, sep = "\t")

# vamos a arreglar un poco los datos (los detalles, mÃ¡s adelante)
paro$Periodo <- gsub("QIV",  "-12-31", paro$Periodo)
paro$Periodo <- gsub("QIII", "-09-30", paro$Periodo)
paro$Periodo <- gsub("QII",  "-06-30", paro$Periodo)
paro$Periodo <- gsub("QI",   "-03-31", paro$Periodo)

paro$Periodo <- as.Date(paro$Periodo)

paro$Situation <- as.character(paro$Situation)

paro$Situation[paro$Situation == "Active population"]   <- "active"
paro$Situation[paro$Situation == "Inactive persons"]    <- "inactive"
paro$Situation[paro$Situation == "Unemployed persons"]  <- "unemployed"
paro$Situation[paro$Situation == "Employed persons"]    <- "employed"
paro$Situation[paro$Situation == "Parados que buscan primer empleo"]    <- "never_employed"

paro$Situation <- factor(paro$Situation)

paro <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
paro <- transform(paro, tasa.paro = 100 * unemployed / active)

ggplot(paro, aes(x = Periodo, y = tasa.paro, col = Gender)) +
  geom_point() + geom_smooth() + facet_wrap(~ Provinces)             # Se van aÃ±adiendo capas (geom) y formas (facet)

# Otra manera de representar las facetas

tmp <- paro[paro$Provinces %in% c("50 Zaragoza", "22 Huesca", "44 Teruel"),]
ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_point() + geom_smooth() + facet_grid(Provinces~Gender) # MÃ­nimo una capa (no es estrictamente necesario facet)

# EstÃ©ticas: cosas que se pueden pintar:
#   x e y
#   tamaÃ±o
#   trasparencia (alpha)
#   Forma (cÃ­rculos, cuadrados, etc.)
#   ...

# GeometrÃ­as: forma de los grÃ¡ficos: puntos, rectas, histogramas, densidades, etc.

# Facetas: parten un grÃ¡fico en sublienzos preservando las escalas (pequeÃ±os mÃºltiplos)

# Pista: usa http://docs.ggplot2.org/current/ como fuente de documentaciÃ³n para tus grÃ¡ficos

# Ejercicio: haz un diagrama de cajas de las temperaturas en NY por mes (sin facetas)

ggplot(airquality, aes(x=factor(Month),y=Temp)) +  geom_boxplot() # IMP!!!! boxplot necesita Month categÃ³rica y no numerica
                                                                  # Mirar caso anterior periodo paro que serÃ­a contÃ­nua y no categorico

# Puedo con mas capas

ggplot(airquality, aes(x=factor(Month),y=Temp)) +  geom_boxplot() + geom_jitter(alpha=0.3) 

# Ejercicio: haz un histograma de las temperaturas en NY por mes (con facetas)

ggplot(airquality, aes(Temp)) + geom_histogram(col='blue') + facet_wrap(~ Month) 
ggplot(airquality, aes(Temp)) + geom_histogram(fill='blue') + facet_grid(Month~ .) # En horizontal puedo comparar Temp por mes

# Ejercicio: prueba con los grÃ¡ficos de violÃ­n (que son una mezcla de los dos anteriores)

ggplot(airquality, aes(x=factor(Month),y=Temp)) + geom_violin(fill='blue',alpha=.3)
  
# Ejercicio: superpÃ³n las distribuciones de las temperaturas de NY por mes como en 
#   http://www.datanalytics.com/2015/07/09/son-normales-las-alturas-de-los-individuos/

ggplot(airquality, aes(x=Temp, fill=factor(Month))) + geom_density(alpha=0.5)

# Ejercicio: haz grÃ¡ficos con tus propios datos. Tienes un rato para ello.


gasdf <- read.csv("carburantes_20050222.csv", sep="\t", header=TRUE, dec=",")

gasdf <- gasdf[gasdf$Precio.Gasoleo.A >1,]
colnames(gasdf)

ggplot (gasdf, aes(x=Longitud..WGS84., y=Latitud..WGS84.)) + geom_point(aes(colour = Precio.Gasoleo.A))+ scale_colour_gradient(low = "blue")


library(ggmap)

# ubico mi alma mater
unizar <- geocode("Puerta del Sol Madrid")
class(unizar)
# obtengo un mapa
map.unizar <- get_map( location = as.numeric(unizar),
                       color = "color",
                       maptype = "roadmap",
                       scale = 1,
                       zoom = 20)

# lo represento
ggmap(map.unizar) 

# le añado puntos
ggmap(map.unizar) + geom_point(aes(x = lon, y = lat),
                               data = unizar, colour = 'red',
                               size = 2)



#----------------------------------------------------------------------------
# merge
#----------------------------------------------------------------------------

# left joins, etc.

clientes <- data.frame(id = 1:3, nombre = c("Carlos", "Sara", "Raquel"))
ventas <- data.frame(fecha = c(1, 1, 1, 2, 2, 3, 3, 3, 4), id = c(1,2,3,2,3, 1, 2, 3, 3), total = 100 * runif(9))


merge(clientes, ventas)
merge(clientes, ventas, all.x = TRUE)

ventas.2 <- ventas[ventas$fecha == 2,]
merge(clientes, ventas.2)
merge(clientes, ventas.2, all.x = T)

# Ejercicio: �y si las variables de cruce no se llaman igual? �Y si no quieres cruzar por todas ellas?

# Nota: puede interesarte esto: https://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html

# Nota: tambiÃ©n puedes usar el paquete data.table.
#   - ventaja: es el mÃ¡s rÃ¡pido con datos grandes
#   - inconveniente: tiene una sintaxis particular y algo extraÃ±a

#----------------------------------------------------------------------------
# web scraping (y mÃ¡s)
#----------------------------------------------------------------------------

install.packages("rvest")
library(rvest)

# vamos a descargar las cotizaciones del IBEX 35 en tiempo "real"
url.ibex <- "http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000"

tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")    

class(tmp)
length(tmp)
# Es una lista de nodos
sapply(tmp, class)

# Convertimos nodos en dataframes

sapply(tmp,html_table,fill=TRUE) # La tabla 5 es la que quiero con las cotizacionoes

ibex <- html_table(tmp[[5]])


# Ejercicio: examinar los objetos anteriores

# Ejercicio: Â¿cuÃ¡l es la tabla de interÃ©s?
# Cuando lo averigues, cambia en el cÃ³digo siguiente el interrogante por el valor adecuado y ejecuÃºtalo

# ibex <- html_table(tmp[[??]])

# Ejercicio: pon nombres "razonables" a las columnas (?colnames)

# Ejercicio: los nÃºmeros no son nÃºmeros sino texto... para convertirlos en nÃºmeros "de verdad" vamos a transformarlos
#   adecuadamente:
#     1.- usa gsub para cambiar "." por "" (i.e., nada) en las columnas de interÃ©s
#     2.- usa gsub para cambiar "," por "." en las columnas de interÃ©s
#     3.- usa as.numeric para cambiar texto por nÃºmeros
#     4.- Â¿te atreves a usar as.Date para cambiar texto por fechas?

# Primer arreglo nom bres columnas (no acentos,signos,blancos)

colnames(ibex) <- c("Nombre","Ultimo","Diferencia","Maximo","Minimo","Volumen","Efectivo","Fecha","Hora")

head(ibex)

clean_num <- function(x)
{
  tmp <- gsub("\\.","",x)
  tmp <- gsub(",",".",tmp)
  as.numeric(tmp)
}

for (i in 2:7) ibex[,i] <- clean_num(ibex[,i])
ibex$Fecha <- as.Date(ibex$Fecha,"%d/%m/%Y")

for (i in 1:ncol(ibex)) print(class(ibex[,i]))


# la otra "gran" funciÃ³n para manejar texto: paste

paste("A", 1:6, sep = ",")

paste("Hoy es ", date(), " y tengo clase de R", sep = "")

paste("A", 1:6, collapse = ",")

# finalmente, strsplit

strsplit("Hoy es martes", split = " ")

strsplit(c("hoy es martes", "ma�ana es miÃ©rcoles"), split = " ")

# MÃ¡s: el paquete stringr, ...

# Ejercicio: examina la documentaciÃ³n del paquete rvest, http://cran.r-project.org/web/packages/rvest/rvest.pdf,
#   y busca aplicaciones a sus funciones

# Ejercicio: consulta http://www.gis-blog.com/data-management-with-r-tidyr-part-1/ y Ã©chale un vistazo a las 
#   funciones separate y unite