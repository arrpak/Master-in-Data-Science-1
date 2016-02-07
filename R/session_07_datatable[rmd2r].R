#' ---	
#' title: "Ciencia de datos - R - Parte 07: Manejo de datos con data.table (y dplyr)"	
#' author: "Olivier Nuñez"	
#' date: "5 de febrero de 2016"	
#' output: html_document	
#' ---	
#' 	
#' 	
#' ### Introducción: Porqué utilizar data.table?	
#' 	
#' > Rapidez de cálculo	
#' 	
#' > Gestión óptima de la memoria	
#' 	
#' > Sencillez de su gramática	
#' 	
#' Un _Quick Tour_ con un sencillo ejemplo:	
#' 	
install.packages("data.table")
require(data.table) 	
n=1e6	
periodos=2007:2016	
DT <- data.table(fecha=rep(periodos,n)) #creacion de un data table	
DT[,precio:=rnorm(n*10,100,10)] #generación de precios	
#' 	
#' 	
#' No hay que tener miedo a escribir...	
#' 	
DT	
#' 	
#' 	
#' 	
#' Agregación:	
#' 	
DT[,.(precio.max=max(precio),precio.min=min(precio)),by=fecha]	
#' 	
#' 	
#' Un data.table es un tipo especial de data.frame	
#' 	
is.data.frame(DT)	
tables()	
rm(DT) #eliminamos DT de la memoria porque pesa mucho!	
#' 	
#' 	
#' ### Importación de una base de datos	
#' Importamos los datos de paro	
#' 	
paro <- read.table("data/paro.csv", header = TRUE, sep = "\t",encoding="UTF-8")	
paro.dt <- fread("data/paro.csv",encoding="UTF-8") #con data.table	
#' 	
#' 	
#' 	
#' 	
paro.dt	
tables()	
#' 	
#' 	
#' ### Indexar/ordenar la base: setkey	
#' 	
#' Ordenación de la base por genero y luego por provincias	
#' 	
setkey(paro.dt,Gender,Provinces)	
paro.dt	
# ahora las tablas tienen "key", están indexadas	
# nota: en data.table, la indexación es la ordenación física por la clave	
tables()	
#' 	
#' 	
#' ### Recodificación de variables categoricas 	
#' A menudo, las variables de la base han de ser recodificadas. Así, en nuestro ejemplo, nos puede interesar expresar el sexo en castellano:	
#' 	
#' 	
paro.dt[,Genero:=ifelse(Gender=="Males","Hombres","Mujeres")]	
paro.dt	

paro.dt[,Genero2:=factor(Gender, levels=c("Females","Males"), labels=c("Mujeres","Hombres"))]
paro.dt	

#' 	
#' 	
#' Una manera más general de hacerlo:	
#' 	
paro.dt[,Genero:=NULL] #eliminamos la columna anteriormente creada	
paro.dt[,Genero2:=NULL] #eliminamos la columna anteriormente creada	
paro.dt	
#Ahora, definimos la tabla de cambio de una codificación a la otra	
gender2genero=data.table(Gender=c("Males","Females"),Genero=c("Hombres","Mujeres"))	
gender2genero	
setkey(paro.dt,Gender) #La base de datos ha de ser indexada por Gender 	
paro.dt<-paro.dt[gender2genero,]	
paro.dt	
#' 	
#' 	
#' __Ejercicio__ : Simplificar la variable "Situation".	
#' variable Situación:	
#' 	
levels(factor(paro.dt$Situation)) #codificación original de Situation	
Situation.corto=c("Active","Employed","Inactive","Never_employed","Unemployed") #Nueva codificación	
#' 	
#' 	
#' 	
paro.dt[,Situation:=factor(Situation,labels=Situation.corto)]	
paro.dt

# Otra forma

Situation2shortsituation = data.table(Situation=c("Active population","Employed persons","Inactive persons",
                                                  "Parados que buscan primer empleo","Unemployed persons"),
                                      NewSituation=c("Active","Employed","Inactive","Never_employed","Unemployed"))
setkey(paro.dt,Situation) 
paro.dt <- paro.dt[Situation2shortsituation,]

table(paro.dt$Situation,paro.dt$NewSituation)


#' 	
#' 	
#' Otro tipo de recodificación consiste en generar dos variables categoricas a partir de una: de la variable "Periodo" sacamos el año y el cuatrimestre:	
#' 	
paro.dt[,Year:=substr(Periodo,1,4)] #año	
paro.dt[,Cuat:=substring(Periodo,6)] #cuatrimestre 	
paro.dt[,Cuat:=as.roman(Cuat)] #cuatrimestre (formato romano, ahora reconocido)	
subset(paro.dt,select=c(Periodo,Year,Cuat))	
#' 	
#'   	
#' ### Resumir/agregar los datos	
#' 	
#' #####----------------------------------------------------------------------------	
#' #### Tablas cruzadas: dcast	
#' ######----------------------------------------------------------------------------	
#' Podemos agregar los datos mediante tablas cruzadas. Por ejemplo, calcular el numero total de individuos en cada _Situation_ por año y cuatrimestre:	
#' 	
dcast(paro.dt,Situation+Year~Cuat,fun=sum) 	
#Ejercicio: buscar el NA; añadir la opción na.rm=TRUE para quitarlo.	
#' 	
setkey(paro.dt,Situation)
paro.dt[is.na(value),]	

dcast(paro.dt,Situation+Year~Cuat,fun=sum,na.rm=TRUE) 	
paro.dt[is.na(value),]	

#' 	
#' 	
#' Otro aplicación de dcast en este contexto, es el calculo de la tasa de paro (tasa.paro=Unemployed/Active):	
#' 	
tmp=dcast(paro.dt, Gender + Provinces + Periodo ~ Situation)	
tmp[,tasa.paro:=Unemployed/Active]	
tasas=subset(tmp,select=c(Gender,Provinces, Periodo,tasa.paro))	
tasas	
# Cálculo del periodo de mayor tasa de paro en cada provincia y sexo (Es un ejercicio de la sesión 03)
tasas[,.(Periodo.maxparo=Periodo[which.max(tasa.paro)]),by=.(Gender,Provinces)]	

which.max(tmp$tasa.paro)
tmp$tasa.paro[807]
# Ejercicio: hacer lo mismo con dplyr	
#' 	
#' 	
#' 	
#' ######----------------------------------------------------------------------------	
#' #### Agregación condicionada con data.table	
#' ######----------------------------------------------------------------------------	
#' 	
paro.dt[, .(total = sum(value)), by = .(Gender,Periodo,Situation)]	# no creamos una nueva columna
#Ejercicio: buscar el NA en la base	
#' 	
#' 	
#' Alternativamente	
#' 	
setkey(paro.dt,Gender,Periodo,Situation)	
#hace falta indexar 	
tables()	
paro.dt[, list(total = sum(value)), by = key(paro.dt)]	
# Variantes:	
paro.dt[, .(total = sum(value), max.value = max(value)), by = key(paro.dt)]	
#' 	
#' ######----------------------------------------------------------------------------	
#' #### Agregación condicionada con dplyr	
#' ######----------------------------------------------------------------------------	
#' 	
install.packages("dplyr")
require(dplyr)	
#' 	
#' 	
#' 	
tmp <- group_by(paro, Gender, Periodo, Situation)	
summarise(tmp, total = sum(value), max.value = max(value))	
#' 	
#' 	
#' Pero "nadie" lo hace así. Mucha gente usa "tuberías" que favorece la interactividad en el análisis:	
#' 	
#' 	
res <- paro %>% 	
  group_by(Gender, Periodo, Situation) %>%	
  summarise(total = sum(value), max.value = max(value))	
res
#' 	
#' 	
#' Nota sobre el operador %>% (tubería o "pipe")	
#' 	
foo <- function(x) x + 2	
5 %>% foo	
#' 	
#' 	
#' 	
# Ejercicio: En dplyr existe la funcion "filter". Úsala para obtener el agregado	
# anterior solo para las mujeres utilizando tuberías	
#' 	
res <- paro %>%
  filter(Gender=='Females') %>%
  group_by(Gender, Periodo, Situation) %>%	
  summarise(total = sum(value), max.value = max(value))	
res
res <- paro %>%
  filter(Gender=='Males') %>%
  group_by(Gender, Periodo, Situation) %>%	
  summarise(total = sum(value), max.value = max(value))	
res

# mejor

res %>% filter(Gender=='Females')
res %>% filter(Gender=='Males')
res %>% filter(Situation=="Active population")


res <- paro %>%
  filter(Provinces=='28 Madrid') %>%                          # filtrar antes es más óptimo si sólo es una provincia
  group_by(Gender, Periodo, Situation) %>%	
  summarise(total = sum(value), max.value = max(value))	
res



#' 	
#' ### Transformación condicionada	
#' ######----------------------------------------------------------------------------	
#' #### Un ejemplo: calcular la proporción (en porcentaje) de hombres y mujeres entre los parados 	
#' ######----------------------------------------------------------------------------	
#' Con data.table:	
#' 	
parados <- subset(paro.dt,Situation == "Unemployed")	
res.dt <- parados[, pct := 100 * value / sum(value), by = .(Periodo,Provinces)]	   # MUY USADO!!!!!
res.dt                                                                             # mismo número de filas que parados
#' 	
#' 	
#' Con dplyr:	
#' 	
res.dplyr <- parados %>% group_by(Periodo, Provinces) %>% mutate(pct = 100 * value / sum(value))	
res.dplyr	
#' 	
#' 	
#' __Ejercicio__: En cada periodo, calcular el porcentaje de parados en cada provincia (la suma de todos los parados de todas las provincias suman 100% en cada periodo). Hacerlo para el total por sexos (hombres + mujeres). Usar data.table por un lado y dplyr por el otro.	
#' 	

parados <- subset(paro.dt,Situation == "Unemployed")
tmp=parados[,(value=sum(value)),by=.(Provinces,Periodo)]
tmp2 <- tmp[,pct:=100*V1/sum(V1), by=.(Periodo)]

sum(subset(tmp2,Periodo=='2014QIV')$pct)     # Compruebo que para un periodo sum=100
tmp2[,.(total.pct=sum(pct)),by=.(Periodo)]    # Compruebo todos los periodos


#Ejercicio, el anterior pero sólo paa mujeres
paradas <- subset(parados,Gender == "Females")
tmp=paradas[,(value=sum(value)),by=.(Provinces,Periodo)]
tmp2 <- tmp[,pct:=100*V1/sum(V1), by=.(Periodo)]

tmp2[,.(total.pct=sum(pct)),by=.(Periodo)]

# con dplyr

tmp <- parados %>% group_by(Periodo,Provinces) %>% summarise(value=sum(value))
res <- tmp %>% group_by(Periodo) %>% mutate(value=100*value/sum(value))

res %>% group_by(Periodo) %>% summarise(total.pct=sum(value)) # Compruebo

#' 	
#' ### Juntar bases de datos	
#' Para ilustrar este aspecto simulamos un ejemplo en finanzas:	
#' 	
n <- 3e6 #número de contratos	
n.clientes <- 1e6 #número de clientes	

#Suponemos que tenemos dos bases de datos: una de clientes y una de contratos donde, 	
contracts <- data.table(contract=1:n,amount=1000 * exp(runif(n))) #cada contrato tiene un cierto "amount"	
contracts	
#cada cliente puede tener uno (o más) contrato(s).	
customers <- data.table(customer  = sample(n.clientes, n, replace = TRUE),contract=1:n,key="customer") 	
customers	
#' 	
#' 	
#' Ahora vamos a juntar las bases de acuerdo con el contrato:	
#' 	
#----------------------------------------------------------------------------	
# merges con data.table	
#----------------------------------------------------------------------------	
tmp <- merge(contracts,customers,by="contract")	

# Ejercicio: Clientes con un sólo contrato. El ejercicio busca pensar teniendo en cuenta 3M lineas

#primero con dplyr

system.time(res <- customers %>% group_by(customer) %>% summarise(cuantos=length(contract)))
res %>% filter(cuantos==1) %>% nrow()

# con data.table

system.time(res <- customers[,.(cuantos=length(contract)),by=customer])
res[cuantos==1,.N]
res[cuantos==1,length((customer))] # otra opción

#Ejercicio: comprueba utilizando system.time() que si las bases están previamente indexadas (por "contract"),	
#el merge es más rapido. Nota: si previa indexación, la opción "by" sobra.	
#' 	
#' 	
#' 	
#----------------------------------------------------------------------------	
# merges con plyr, dplyr	
#----------------------------------------------------------------------------	
require(plyr)

system.time(join(contracts, customers,by="contract")) #mucho más lento!	
system.time(merge(contracts,customers,by="contract"))

# Ejercicio: ¿por qué campos se realiza el cruce si se omite "by"?	
#Para más información sobre join y sus "hermanas" leer:	
#https://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html	
#' 	
#' 	
#' 	
#' ### Otros recursos: Cheat-sheet para data.table, dplyr y .... R-markdown	
#' Para tener una visión sintetica de estos paquetes y de sus posibilidades:	
#' 	
#' > Cheat-sheet para [dplyr](http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)	
#' 	
#' > Cheat-sheet para [data.table](https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf)	
#' 	
#' > Cheat-sheet para [R-markdown](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)	
#' 
M=matrix(c(1,1,0,2),2,2,byrow=TRUE)
M=matrix(c(1,2), nrow=1)
M
layout(M)
plot(c(1,2,3,4))
hist(c(1,2,3,4,5,5,5))
  
#' 	
