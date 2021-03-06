#############################################################################
# Ciencia de datos - R - Procesamiento del lenguaje natural
# cgb@datanalytics.com, 2016-07-07
#
# Introducción al NLP
#############################################################################

#----------------------------------------------------------------------------
# Preprocesamiento con el paquete tm
# Basado en https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
#----------------------------------------------------------------------------

library(tm)

# cargamos los datos que hemos bajado de internet usando thyssen_scraping.R
# se trata de las descripciones de los cuadros del mueseo en cuestión

load("thyssen.Rdata")     # carga thyssen

# comentarios contiene blablablá sobre los cuadros
# los metadatos contienen el autor, el título y la fecha

comentarios <- sapply(thyssen, function(x) x$texto)
metadatos   <- lapply(thyssen, function(x) x$ficha)

length(comentarios)
hist(nchar(comentarios))


## Creación de un "corpus"

comentarios <- VCorpus(VectorSource(comentarios), readerControl = list(language = "es"))

# VCorpus tiene otras "sources": directorios, etc. 

# en tm, para "ver" objetos, se usa inspect:

inspect(comentarios[1:3])

# Un Corpus es una lista de documentos; cada documento tiene el texto y 
# un conjunto de metadatos (que no usaremos)

str(comentarios[1])
comentarios[[1]]$content


## transformaciones: tm_map aplica una función a cada documento

comentarios <- tm_map(comentarios, stripWhitespace)
comentarios <- tm_map(comentarios, content_transformer(tolower))

comentarios <- tm_map(comentarios, removeWords, stopwords("spanish"))

# wordcloud: sigue las instrucciones de https://georeferenced.wordpress.com/2013/01/15/rwordcloud/
# y crea una. Ten cuidado con max.words (comienza conservadoramente).

library(wordcloud)
wordcloud(comentarios, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.25, use.r.layout=FALSE, colors=brewer.pal(1, "Dark2"))

## ¿lematización? (cuidado: es lenta)

prueba <- tm_map(comentarios[1:10], stemDocument)
prueba[[1]]$content

# ejercicio: haz un wordcloud de prueba


## Matrices de palabras

dcm.thyssen <- DocumentTermMatrix(comentarios)

# filtra los términos "sparse": que aparecen en pocos documentos
dcm.thyssen.small <- removeSparseTerms(dcm.thyssen, sparse= 0.8)

inspect(dcm.thyssen.small[1:10, 1:10])

# ejercicio: lee ?DocumentTermMatrix para ver otras opciones

findFreqTerms(dcm.thyssen, 100)

# nota: aparecen comas... ¡hay que volver atrás y quitarlas! ¿Cómo?

findAssocs(dcm.thyssen, "dalí", 0.6)
findAssocs(dcm.thyssen, "barroco", 0.5)
findAssocs(dcm.thyssen, "cubista", 0.6)
findAssocs(dcm.thyssen, "beaulieu", 0.6)

## TF-IDF

# cambia la frecuencia del término por su TF-IDF, i.e., quita peso a las palabras
# que aparecen en muchos documentos

dcm.norm <- weightTfIdf(dcm.thyssen)

# ejercicio: inspecciona el objeto anterior y cerciórate de que es así

inspect(dcm.norm)[1:10,1:10]

## Modelización usando documentos de frecuencias de palabras

# matriz a DF (para clasificación, etc.)

res <- as.data.frame(inspect(dcm.thyssen.small))

# vamos a tratar de predecir la fecha de un cuadro según las palabras con que se describe

fechas <- sapply(metadatos, function(x) x[3])
fechas <- as.numeric(fechas)

dat <- data.frame(fecha = fechas, res)
dat <- dat[!is.na(dat$fecha),]

library(party)

modelo <- ctree(fecha ~ ., data = dat)

plot(modelo)

## Latent Dirichlet allocation


library(topicmodels)

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 5

freqs <- apply(dcm.thyssen.small, 1, sum)
m     <- dcm.thyssen.small[freqs > 0, ]

ldaOut <-LDA(m, k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, 
                          burnin = burnin, iter = iter, thin=thin))

topics(ldaOut)

terms(ldaOut)
wordassignments(ldaOut)

# Ejercicio: aplicar el contenido de
# https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
# al caso en cuestión

## Factorización no negativa de matrices

library(NMF)

m <- as.matrix(m)

res <- nmf(m, 5)

basismap(res)
coefmap(res)


