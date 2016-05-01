#############################################################################
# Ciencia de datos - R - Parte 09: estadística (cont.)
# cgb@datanalytics.com, 2016-02-05
#
# El objetivo de esta sesión es familiarizarse con la estadística bayesiana y
#   la modelización estadística 
#############################################################################

#----------------------------------------------------------------------------
# A/B testing bayesiano
#----------------------------------------------------------------------------

# Datos:

set.seed(1234)                # fijamos semilla y ahora todos tenemos los vismos datos simulados probar a hacer laz simulaciones varias veces
 
N.A <- rpois(1, 10000)        # sujetos que ven pagina a (poisson simula muy bien esto)
N.B <- rpois(1, 10000)

p.a <- 0.02                   # probabilidad te�rica de conversion en p�gina a (que compran, o clican por ejemplo)
p.b <- 0.025

n.a <- rbinom(1, N.A, p.a)    # exitos en p�gina a (son probabilidades teoricas)
n.b <- rbinom(1, N.B, p.b)


# test de hipótesis "frecuentista": 

prop.test(c(n.a, n.b), c(N.A, N.B))     # la hip�tesis es que ambas p�ginas son iguales

# realmente p.a sale 0.017 y p.b=0.024 y no las te�ricas (confirma el test es mejor la p�gina b, conforme sube el 10000 (mas tiempo) el resultado es mejor hacer con 1000 en vez de 10000, se acercan las prob)


# test de hipótesis basado en remuestreos (cambios de etiquetas): (imaginemos que el test anterior no existe, lo hacemos nosotros:)

diferencia.props.real <- n.b / N.B - n.a / N.A

mascara    <- rep(c("a", "b"), times = c(N.A, N.B))      # independientes porque son aleatorias, luego comparo con la real. Si son similares ser�a independiente a y B (dar�a igual una que otra)
resultados <- rep(0, length(mascara))
resultados[1:(n.a + n.b)] <- 1

res <- replicate(10000, tapply(sample(resultados), mascara, mean)) # sample= al azar
res <- t(res)
hist(res[,2] - res[,1], col = "gray")
abline(v = diferencia.props.real, col = "red") # la real est� fuera de rango, luego no son independientes a y b (tiene valor) pensar en placebo


# test de hipótesis bayesiano (analítico): véase http://www.evanmiller.org/bayesian-ab-testing.html


# test de hipótesis bayesiano (MCMC)

install.packages("rstan")

library(rstan)

stanmodelcode <- '
data {
  int A;
  int B;
  int a;
  int b;
}
 
parameters {
  real<lower=0, upper = 1> pa;
  real<lower=0, upper = 1> pb;
}
 
model {
  // priori no informativa
  pa ~ beta(1,1); 
  pb ~ beta(1,1); 

  // verosimilitud
  a ~ binomial(A, pa);                      # n�mero de conversiones
  b ~ binomial(B, pb);
}
'

fit <- stan(model_code = stanmodelcode, 
            data = list(A = N.A, B = N.B, a = n.a, b = n.b),
            iter = 12000, warmup = 2000, 
            chains = 4, thin = 10)

res <- as.data.frame(fit)

res$delta <- res$pb - res$pa

hist(res$delta, col = "gray", breaks = 30)  # resultado: mejora la tasa de conversi�n de pag b respecta a A entre 0,30% y 1%. En realidad no se hace as�, sin oque se hace por sexo, edad, origen, pais


# Ejercicio: implementar un t-test bayesiano equivalente a t.test(extra ~ group, data = sleep)
# Pistas: hay dos vectores de datos con distribución normal de parámetros (mu, sigma) y (mu + delta, sigma)
#   y el objetivo es ver si "delta" es distinta de cero.

boxplot(extra ~ group, data = sleep)



#----------------------------------------------------------------------------
# Máxima verosimilitud
#----------------------------------------------------------------------------

curve(dbinom(60, 100, x), from = 0, to = 1,
      xlab = "p", ylab = "", main = "Función de verosimilitud")

res <- optimize(function(p) dbinom(60, 100, p), lower = 0, upper = 1, maximum = TRUE)
abline(v = res$maximum, col = "red")


## duración de unas bombillas por máxima verosimilitud

duraciones <- rexp(100, 1/1000)

log.verosimilitud <- function(lambda){
  length(duraciones) * log(lambda) - lambda * sum(duraciones)
}

curve(log.verosimilitud, from = 0, to = 0.005)

res <- optimize(log.verosimilitud, lower = 0, upper = 0.005, maximum = TRUE)
abline(v = res$maximum, col = "red")

# comparaci�n de resultado anal�tico y aproximado:
res$maximum
1 / mean(duraciones)

# Ejercicio: para el conjunto de datos, crea la función de pérdida cuadrática (del modelo lineal que 
#   predice dist en función de speed). Es una función que depende de dos parámetros. Usa optim para 
#   encontrar esos parámetros. Compáralos con los que salen con lm.


#----------------------------------------------------------------------------
# Modelización bayesiana
#----------------------------------------------------------------------------

# Regresión lineal

stanmodelcode <- '
data {
  int N;
  vector[N] speed;
  vector[N] dist;
}

parameters {
  real a0;
  real a1;
  real<lower = 0, upper = 100> sigma;
}

model {
  // prioris
  a0 ~ cauchy(0, 100);
  a1 ~ cauchy(0, 100);
  sigma ~ cauchy(0, 5);

  // verosimilitud
  for (i in 1:N) 
  dist[i] ~ normal(a0 + a1 * speed[i], sigma);
}
'

fit <- stan(model_code = stanmodelcode, 
            data = list(N = nrow(cars), speed = cars$speed, dist = cars$dist),
            iter=12000, warmup=2000, 
            chains=4, thin=10)

tmp <- as.data.frame(fit)$a1
hist(tmp, breaks = 30, col = "gray", xlab = "", freq = FALSE, main = "coef speed: posteriori (hist) vs lm (red)")


# Series temporales

# Voy a crear unos datos

n <- 100
delta <- 0.9
sigma <- 0.5
a0 <- 0
saltos <- rnorm(n-1, 0, sigma)
a <- Reduce(function(a,b) delta * a + b, saltos, a0, accumulate = TRUE)
plot(a, type = "l")

codigo <- '
data {
  int n;
  vector[n] x;
}

parameters {
  real delta;
  real<lower = 0, upper = 100> sigma;
}

model {
  // prioris
  delta ~ uniform(0, 2);
  //sigma ~ cauchy(0, 5);

  // verosimilitud
  for (i in 2:n) 
    x[i] ~ normal(delta * x[i-1], sigma);
}
'

fit <- stan(model_code = codigo, 
            data = list(n = n, x = a),
            iter=12000, warmup=2000, 
            chains=4, thin=10)

tmp <- as.data.frame(fit)

hist(tmp$delta)


# Ejercicio: estimar parámetros bursátiles:
#   1.- Baja datos de una acción (unas 100 observaciones)
#   2.- Construye los incrementos delta(t) = log(p(t+1)/p(t))
#   3.- Modelízalos como delta(t) ~ normal(a0 + a1 * t, sigma)
#   4.- Estudia los resultados obtenidos
#   5.- Simula una serie con esos parámetros

# Ejercicio: repite el ejercicio anterior pero usando una distribución t en lugar de una normal


