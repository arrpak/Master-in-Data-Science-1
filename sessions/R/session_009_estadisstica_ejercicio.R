# ejercicio sesion 9


library(rstan)

a <- sleep$extra[sleep$group =="1"] # me pasan estos dos vectores y me piden que compare sus medias
b <- sleep$extra[sleep$group =="2"]

stanmodelcode <- '
data {
int Nx;
int Ny;
vector[Nx] x;
vector[Ny] y;
}

parameters {
real mux;
real muy;
real<lower=0> sigma;
}

model {
// priori no informativa
#pa ~ beta(1,1); 
#pb ~ beta(1,1); 


// verosimilitud
x ~ normal(mux, sigma);   # distintas medias con varianzas iguales                   
y ~ normal(muy, sigma);
}
'

fit <- stan(model_code = stanmodelcode, 
            data = list(x = a, y = b, Nx=length(a), Ny=length(b)),
            iter = 12000, warmup = 2000, 
            chains = 4, thin = 10)

res <- as.data.frame(fit)
  
head(res)

hist(res$muy ~ res$mux, breaks=30, col="gray")


# Segundo ejercicio

perdida.cuadratica <- function(a0,a1)
{
  # Pseudo codigo: suma de (y_i ~ a0+a1*x_i)^2
  sum(cars$dist - (a0 + a1*cars$speed))^2
}

res <- optim(c(-1,1), function(x) perdida.cuadratica(x[1],x[2]))

# comparamos con lm

lm(dist ~ speed, data ~ cars)


# Tercer ejercicio

stanmodelcode <- '
data {
int N;
vector[N] speed;
vector[N] dist;
}

parameters {
real delta;
real<lower = 0, upper = 100> sigma;
}

model {
// prioris
delta ~ uniffor(0,2);
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
