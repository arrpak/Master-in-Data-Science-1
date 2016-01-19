# Ayuda de head y sacar las diez primeras filas de iris

?head
head(iris,10)
head(iris, n=10L)

# Crear una versión de iris ordenada por especie y dentro de cada especie por Petal.Length

?order
iris[order(iris$Species, iris$Petal.Length), ]

# Ejercicio: estudia el conjunto de datos "airquality" (información meteorológica de 
#   cierto año en Nueva York) En particular, responde a:
#   - ¿Cuál es la temperatura media de esos días?
#   - ¿Cuál es la tempreatura media en mayo?
#   - ¿Cuál fue el día más ventoso?

summary(airquality)                                                 # Temp Average        = 77.88
summary(airquality[airquality$Month == 5, ])                        # Temp Average in May = 65.55
airquality[order(airquality$Wind, decreasing = TRUE), ][1:1,5:6]    # Windiest day        = 17th, June

# Ejercicio: crea una tabla adicional seleccionando todas las columnas menos mes y día; luego
#   haz un plot de ella y trata de encontrar relaciones (cualitativas) entre la temperatura y el viento
#   o el ozono,...

my_airquality <- airquality[,1:4]
plot(my_airquality)
plot(my_airquality$Temp,my_airquality$Ozone)
plot(my_airquality$Wind,my_airquality$Ozone)

# Ejercicio: lee el fichero paro.csv usando la función read.table. Comprueba que
#   está correctamente leído usando head, tail, nrow, summary, etc.

?read.table
paro_df <- read.table("/home/dsc/Repositories/Master-in-Data-Science/Data/R/paro.csv", header=TRUE)

head(paro_df)
tail(paro_df)
nrow(paro_df)
summary(paro_df)

# Ejercicio: representa gráficamente la anchura del sépalo contra su longitud (en iris)
# Ejercicio: usa main, xlab e ylab para añadir etiquetas (ver ?plot)

?plot
plot(iris$Sepal.Width,iris$Sepal.Length, main="Iris Sepal", xlab="sepal.width", ylab="sepal.length", col='blue')

# Ejercicio: estudia la distribución de las temperaturas en Nueva York (durante los meses en cuestión)
# Ejercicio: usa "col" para mejorar el aspecto del gráfico.
# Ejercicio: usa abline para dibujar una línea vertical roja en la media de la distribución.

head(airquality)
hist(airquality$Temp,main="Histogram of NY Temps",xlab="Temp",
                     border=NA, col='blue', density=50)
abline(v=mean(airquality$Temp),col='red')
