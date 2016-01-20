# Ejercicio: crea el vector que numera las filas de iris (es decir, los números del 1 hasta
# el número de filas de iris)

iris_idx <- 1:nrow(iris)

## Ejercicio: seleccionar todos los valores de un vector excepto los dos últimos

iris_idx[1:(length(iris_idx)-2)]

# Ejercicio: Muestrea iris, es decir, extrae (p.e., 30) filas al azar de dicha tabla.
#   Pista: recuerda que "ordenar" era "seleccionar ordenadamente"; de igual manera, 
#   en una tabla, muestrear será...

iris[sample(1:nrow(iris),30), ]

# Ejercicio: parte iris en dos partes iguales (75 observaciones cada uno) con las filas elegidas 
# al azar (¡y complementarias!)

s_idx <- sample(1:nrow(iris),nrow(iris)/2)
iris_1 <- iris[s_idx, ]
iris_2 <- iris[-(s_idx), ]
iris_1
iris_2

# Ejercicio: simulación de la evolución del mercado de valores (de acuerdo con cierta gente)
# si el precio hoy es a, mañana es a * exp(lambda), donde lambda tiene distribución N(a,b)
# pista: genera las lambdas y luego cumprod para obtener cotizaciones a lo lardo de n sesiones 

plot(8.90*cumprod(exp(rnorm(200,0,0.01))), type='l') # sd=1% diary, first value=8.90€ (today TEF value)

# Ejercicio: suma un millón de términos de la fórmula de Leibniz (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
# para aproximar pi

x <- (0:1e6)
sum(((-1)^x)/(2*x+1))*4

# Ejercicio: calcular el valor medio de la longitud de los pétalos de iris usando mean()
# Ejercicio: lo mismo, usando sum() y length()

mean(iris$Petal.Length) # avarage Petal.Length = 3.758

# Ejercicio: haz mean(sample(iris$Sepal.Length, replace = T)) varias veces. Salen números que
#   se parecen a la media de iris$Sepal.Lenght... ¿Te suena a algo?

mean(iris$Sepal.Length)
mean(sample(iris$Sepal.Length, replace = T))

# Ejercicio: calcula el valor medio de la temperatura en cada mes de Nueva York (usa airquality)

barplot(round(tapply(airquality$Temp, airquality$Month, mean),2),col='green',density=20, ylim=c(0,100))

# Ejercicio: crear una función que dado un número n calcule la suma de los n primeros términos de la serie de
#   Leibniz para calcular pi

pi_cal <- function(n)
{
  x <- 0:n
  return(sum(((-1)^x)/(2 * x + 1))*4)
}

pi_cal(3000)

# Ejercicio: modificar la función siguiente para que dé un error cuando x sea menor que 0 o mayor que 1.
#   Pista: la función stop() lanza un error. El argumento de stop es el texto que aparece en el mensaje.

xln <- function(x)
{
  if (x == 0)
    return(0)
  else
    if (x < 0 | x > 1)
      stop("input value out of range")
    else
      return(-x*log(x))
}

xln(c(0.1,0.2,0.3))
xln(.5)
xln(2)
xln(-3)

r <- seq(0.1,0.9,by=0.01)
plot(r,xln(r),type='l',col='blue',lty=4)

# Ejercicio: crea una función para simular cotizaciones bursátiles usando bucles for en lugar de la
# función cumprod.

plot(8.90*cumprod(exp(rnorm(200,0,0.01))), type='l') # sd=1% diary, first value=8.90€ (today TEF value)

stock_prices_simulator <- function(stock_price)
{
  tmp <- 1
  result <- NULL
  for (i in exp(rnorm(200,0,0.01)))
  {
    tmp <- tmp * i
    result <- c(result,tmp)
  }
  return(result*stock_price)
}

plot(stock_prices(8.9),type='l')

# Ejercicio: crea el vector de nombres de ficheros de data usando dir; luego, aplícale una función
#   que lea las líneas (readLines) y las cuente.

read_lines_dir <- function(v)
{
  result <- 0
  for (i in v) { result <- result + length(readLines(i,warn=FALSE)) }
  return(result)
}

read_lines_dir(dir(".")) # lines = 21061

# Ejercicio: usa nchar para contar el número de caracteres de esos ficheros.

totalchar <- 0
for (j in dir("."))
{
  for (i in readLines(j,warn=FALSE))
  {
    totalchar <- totalchar + nchar(i)
  }
}

totalchar # characters = 2326698

# Ejercicio: usa replicate para obtener la distribución de los posibles valores de una acción al cabo de un año
# Pistas: crea una función que dependa de 3 parámetros: número de días y los parámetros de la normal;
#   luego usa replicate para generar un vector de precios finales

# Un ejemplo (asegúrate de que tu directorio de trabajo es "data")
