#############################################################################
# Ciencia de datos - R - Parte 02: estructuras de datos y programación
# cgb@datanalytics.com, 2016-01-07
#
# El objetivo de esta sesión es aprender a manejar vectores y listas. Incluye
# una introducción a la programación vectorizada. Termina con una introducción 
# a la estadística con R.
#############################################################################

#----------------------------------------------------------------------------
# Vectores
#----------------------------------------------------------------------------

x <- 1:10
y <- iris$Species

# Ejercicio: crea el vector que numera las filas de iris (es decir, los números del 1 hasta
# el número de filas de iris)

v <- 1:nrow(iris)


## inspección de vectores
length(x)
table(y)         # ¡muy importante!
summary(y)
summary(x)

## funciones
fivenum(x)   # los "cinco números característicos" de un vector 
mean(x) 
max(x) 
median(x) 
sum(x) 
prod(x)    
# ...         y miles más


## selección
# se usa el corchete; ahora sin comas porque los vectores son unidimensionales
x <- x^2
x[1:3] 
x[x > 25] 
x[3:1] 
x[-(1:2)] 
x[-length(x)]

## ejercicio: seleccionar todos menos los dos últimos
x

x[1:(length(x)-2)]
x[-2]

# cambiar parte de los componentes de un vector
z <- x 
z[z < 5] <- 100 
z

# Nota: ¡seleccionar y reemplazar es una técnica muy poderosa!

## muestrear un vector
x
sample(x, 4) 
sample(x, 100)                  # ?falla (hay menos valores en el vector)
sample(x, 100, replace = TRUE)  # manera correcta

# Ejercicio: Muestrea iris, es decir, extrae (p.e., 30) filas al azar de dicha tabla.
#   Pista: recuerda que "ordenar" era "seleccionar ordenadamente"; de igual manera, 
#   en una tabla, muestrear será...

iris[sample(1:nrow(iris),30),]

iris[(1:(nrow(iris)/2))*2,]

iris[1:(nrow(iris)/2)*2,]

# Nota: el muestreo es importante en distintos ámbitos. Por ejemplo, a la hora de hacer tests A/B: 
# ¿qué observaciones van al grupo A y cuáles al B?

# Ejercicio: parte iris en dos partes iguales (75 observaciones cada uno) con las filas elegidas 
# al azar (¡y complementarias!)

muestra = sample(1:nrow(iris),75)
iris[muestra,]
iris[-muestra,]

## Generación de vectores

1:5
5:1
c(1:5, 5:1) 
c(1, 5, -1, 4)

# Importante: la función c() (de concatenar) sirve para generar vectores "a mano".
#   El operador ":" crea rangos

# En realidad, ":" es una abreviatura para seq()

seq(1, 4)

seq(1,40, by=2)


# Y está estrechamente emparentada con la función rep()

rep(1:4, 4)
rep(1:4, each = 4)

# Ejercicio: Crea el patrón 1, 1.1, 1.2,..., 2. (Nota: hay varias respuestas).
#   Pista: consulta qué hace el argumento "by" de seq.

seq(1,2,by=0.1)

# Los siguientes ejemplos ilustran cómo crear patrones más complejos usando rep()

rep(1:4, 2) 
rep(1:4, each = 2)
rep(1:4, c(2,2,2,2))

rep(1:4, times = 4:1)

rep(1:4, c(2,1,2,1)) 
rep(1:4, each = 2, len = 4) 
rep(1:4, each = 2, len = 10) 
rep(1:4, each = 2, times = 3) 


## generación de vectores con una determinada distribución estadística
x.uniforme <- runif(10)
x.normal   <- rnorm(13)
hist(rnorm(1000))
hist(runif(1000))
hist(rpois(1000, 5))

# Ejercicio: consulta la ayuda de rnorm, de runif, de rpois... ¿qué tienen en común?
# Ejercicio: busca cómo muestrear la distribución gamma

?rnorm
?runif
?rpois

# Soluciones = van a cuatro funciones similares (d,p,q,r)
# uniform distribution (google images)
# normal distribution ()
# pois
# r - muestrea al azar
# d - mirar función de densidad (probabilidad)
# p - función de distribucuón (suma de probabilidad o integral hasta el punto) o probabilidad de sacar un valor menor que
# q - quantil (realmente es la inversa de la p)

# Mirar diferencias entre normal y t (esta incluye muestras con catastrofes en bolsas)

hist(rt(1000,df=4))

hist(rgamma(1000,2,100))
## ordenación de vectores

x <- c(4, 5, 3, 2, 1, 2)
sort(x)           # ordena los elementos del vector

order(x)
x[order(x)]       # equivale a sort(x)

# Nota: ¿recuerdas cómo ordenábamos tablas?
# Recordad que no había etrictamente una sort de tablas
# Ahora tiene sentido como se ordenaban tablas

# La función rank está relacionada con las anteriores

x
rank(x)
rank(x, ties = "first")

# Ejercicio: ¿qué otros tipos de "ties" existe? ¿Qué hacen?
# Ejercicio: Comprobar que rank(x, ties = 'first') es equivalente a order(order(x)).
# Ejercicio: Comprobar que order(order(order(x))) es equivalente a order(x).



## Operaciones matemáticas

2+2
x <- 4*(3+5)^2           # el resultado de una operación matemática puede asignarse a una variable
x / 10

# Una propiedad interesantes de R es que se puede operar sobre vectores igual que se opera sobre
#   números. De hecho, en R, un número ¡es un vector numérico de longitud 1!

length(2)
length(x)

# calculadora sobre vectores: 
x <- 1:10
2*x 
2*x + 1
x^2 
x * rev(x) 
sum(x) 
prod(x) 
cumsum(x)


# suma de los términos de una progresión geométrica
# una progresión geométrica es: a, ab, ab^2, ab^3,...

n <- 100
a <- 3
b <- 1.1

res <- 1:n
res <- b^res
res <- a * res
sum(res)

# ejercicio: simulación de la evolución del mercado de valores (de acuerdo con cierta gente)
# si el precio hoy es a, mañana es a * exp(lambda), donde lambda tiene distribución N(a,b)
# pista: genera las lambdas y luego cumprod para obtener cotizaciones a lo lardo de n sesiones 

tmp <- rnorm(200,0.001,0.02)
res <- cumprod(exp(tmp))
plot(res,type='l')

#incluyendo catastrofes 

tmp <- (0.02*rt(200,df=4))+0.001
res <- cumprod(exp(tmp))
plot(res,type='l')

# un ejercicio más sofisticado: calculadora de cuotas de hipotecas:

# 1000 euros al 3% valen
1000 * (1 + 3 / 100)^4
# al cabo de 4 años;
# 1000 euros dentro de 4 años al 3% valen
1000 / (1 + 3 / 100)^4
# ahora.

# si pagas 500 euros al mes durante treinta años y tu hipoteca está al 3%, su valor actual es:
interes.mensual <- 3 / 12 / 100
meses <- 1:(12*30)
deflactor <- (1 +  interes.mensual)^meses    # un vector
deflactor <- 1 / deflactor                   # otro vector
valor.actual <- sum(500 * deflactor)


# ahora vamos a hacer lo contrario: dado un capital, un plazo y un interés, calcular la cuota

capital <- 100000 
anyos <- 20 
interes <- 3

interes.mensual <- interes / 12 / 100 
meses <- 1:(anyos*12)

cuota <- capital / sum(1 / (1+interes.mensual)^meses)

# Nota: en realidad estamos sumando los términos de una "progresión geométrica"

# Nota: para entender la última línea, ejecuta las siguientes:

1+interes.mensual                               # un número
(1+interes.mensual)^meses                       # un vector de longitud anyos * 12
1 / (1+interes.mensual)^meses                   # el vector de sus recíprocos
sum(1 / (1+interes.mensual)^meses)              # la suma de los recíprocos
capital / sum(1 / (1+interes.mensual)^meses)

# Nota: lo que hacemos es sumar el valor presente neto de una cantidad desconocida (cuota) usando el interés mensual dado

# Ejercicio: suma un millón de términos de la fórmula de Leibniz (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
#   para aproximar pi

# Ejercicio: calcular el valor medio de la longitud de los pétalos de iris usando mean()
# Ejercicio: lo mismo, usando sum() y length()

# Ejercicio: haz mean(sample(iris$Sepal.Length, replace = T)) varias veces. Salen números que
#   se parecen a la media de iris$Sepal.Lenght... ¿Te suena a algo?

#----------------------------------------------------------------------------
# La función tapply
#----------------------------------------------------------------------------

tapply(iris$Petal.Length, iris$Species, mean)

# La función tapply aplica una función (mean) a un vector (longitud del pétalo) en
#   cada uno de los trozos definidos por iris$Species (¿como un group by?)

# Ejercicio: calcula el valor medio de la temperatura en cada mes de Nueva York (usa airquality)

tapply(airquality$Temp, airquality$Month, mean)
tapply(airquality$Temp, airquality$Month, fivenum)
tapply(airquality$Temp, airquality$Month, median)

tapply(airquality$Month, airquality$Temp, mean)
       
#----------------------------------------------------------------------------
# Listas
#----------------------------------------------------------------------------

# Ya las conocemos:
is.list(iris)

# Son coleccciones heterogéneas de objetos de longitud determinada:
length(iris)

# Son útiles como "contenedores" de información:

is.list(modelo.con.dept)

length(modelo.con.dept)
names(modelo.con.dept)
str(modelo.con.dept)

names(modelo.con.dept)

# Los extractores $ y [[]] que vimos son propios de listas:
modelo.con.dept$coefficients
modelo.con.dept[["coefficients"]]

# Creación de listas:
mi.lista <- list(a = 1:3, b = c("hola", "caracola"))
mi.lista

mi.lista$z <- matrix(1:4, 2, 2)
mi.lista

# Usos avanzados: los ficheros XML o JSON se "parsean" como listas en R. (MIRAR JSON.ORG)

# Ejercicio: ¿qué función serviría para concatenar dos listas?

mi.otro.lista <- list(hola="Esta es otra lista")

c(mi.lista,mi.otro.lista)

list(mi.lista,mi.otro.lista)

# Ejercicio: ¿cómo borrarías un elemento de una lista?
# Ejercicio: ¿qué crees que pasaría si haces mi.lista[1:2]?

mi.lista[[1:2]]
mi.lista[1:2]


#############################################################################
# Programación
#############################################################################

## Variables (ya las hemos visto)

mi.iris <- iris[1:10,]

# Recordad:

ls()
rm(iris_2)
ls()

# ¿de qué tipo son?

mi.iris <- iris[1:10,]
class(mi.iris)
is.data.frame(mi.iris)

x <- 1:10
is.vector(x)
class(x)
typeof(x)


## Funciones

calcular.cuota.hipoteca <- function(capital, anyos, interes){
  interes.mensual <- interes / 12 / 100 
  meses <- 1:(anyos*12)
  return(capital / sum(1 / (1+interes.mensual)^meses))    # Si no pones return, se devuelve el último valor analizado
}

calcular.cuota.hipoteca(100000, 20, 3)

calculadora.hipotecas <- calcular.cuota.hipoteca
calculadora.hipotecas(100000, 20, 3)
ls()
rm(calculadora.hipotecas)
ls()

# Ejercicio: crear una función que dado un número n calcule la suma de los n primeros términos de la serie de
#   Leibniz para calcular pi


tmp <- 0:1000
tmp <- (-1)^tmp / (2 * tmp + 1)
sum(tmp)*4



## Expresiones condicionales

xln <- function(x){
  return(-x * log(x))
}

x <- 1:10000 / 10000
plot(x, xln(x), type = "l", xlab = "", ylab = "", 
     main = "Función -x * log(x)")


xln(0)       # Nan cuando queremos cero!


xln <- function(x){
  if (x == 0)
    return(0)
  return(-x * log(x))
}

xln(0)

# Ejercicio: modificar la función anterior para que dé un error cuando x sea menor que 0 o mayor que 1.
#   Pista: la función stop() lanza un error. El argumento de stop es el texto que aparece en el mensaje.

# con elses:

xln <- function(x){
  if (x == 0)
    return(0)
  else
    return(-x * log(x))
}

# Nota: cuando lo que sigue a if es una única línea se pueden ignorar las {}. Si lo que lo sigue
#   es un bloque de código, hay que encerrarlo en llaves {}

## Bucles

# for(var in vector){
# 	# expresión que se repite
# }


mi.factorial <- function(n){
  factorial <- 1
  for(i in 1:n){
    factorial <- factorial * i
  }
  return(factorial)
}

mi.factorial(7)

# Ejercicio: crea una función para simular cotizaciones bursátiles usando bucles for en lugar de la
# función cumprod.


# con while

while(condicion){
  # expresión que se repite
}


mi.factorial <- function(n){
  factorial <- n
  while(n > 1){
    n <- n - 1
    factorial <- factorial * n
  }
  return(factorial)
}

mi.factorial(7)


## Vectorización de código

x <- 1:10
sqrt(x)
sum(x)

# programación no vectorizada:
x <- 1:10
suma.x <- 0
for (i in x){
  suma.x <- suma.x + i
}
suma.x

#----------------------------------------------------------------------------
# funciones sapply, lapply, apply, replicate...
#----------------------------------------------------------------------------

cuadrado <- function(x) x^2
sapply(1:10, cuadrado)
lapply(1:10, cuadrado)

### MUY UTIL:
sapply(1:10, function(x) x^2)

lapply(iris, class)
sapply(iris, length)

# Ejercicio: crea el vector de nombres de ficheros de data usando dir; luego, aplícale una función
#   que lea las líneas (readLines) y las cuente.

# Ejercicio: usa nchar para contar el número de caracteres de esos ficheros.

# Ejercicio: usa replicate para obtener la distribución de los posibles valores de una acción al cabo de un año
# Pistas: crea una función que dependa de 3 parámetros: número de días y los parámetros de la normal;
#   luego usa replicate para generar un vector de precios finales

# Un ejemplo (asegúrate de que tu directorio de trabajo es "data")

ficheros <- dir()
ficheros

foo <- function(fichero){
  tmp <- readLines(fichero)
  length(tmp)
}

sapply(ficheros, foo)

# Ejercicio: ¿qué hace el código anterior?

# Ejercicio: ¿se te ocurren aplicaciones?

# Ejercicio: haz lo mismo calculando (usa la función nchar) el número de caracteres de cada fichero.





