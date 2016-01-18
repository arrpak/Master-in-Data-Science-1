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
