# ----------------------
#  Intro to vectors
# ----------------------

1:10
iris_sp <- iris$Sepal.Length

# Inspecting vectors

summary(iris_sp)
length(iris_sp)
table(iris_sp)
barplot(table(iris_sp))

# Functions

fivenum(iris_sp)
mean(iris_sp)
median(iris_sp)
sum(iris_sp)
max(iris_sp)

# Selecting values

iris_sp
iris_sp[4]
iris_sp[1:4]
iris_sp[c(1,4)]
iris_sp[iris_sp < 6]

iris_sp[-4]
iris_sp[-(1:2)]

sample(iris_sp,4)
sample(iris_sp,200,replace=TRUE) # If it is necessary more values

# Modifing values

iris_sp
iris_sp[4] <- 3
iris_sp
iris_sp[iris_sp >7] <- Inf
iris_sp

# Generating vectors

1:10
10:1
c(1,2,4,12)
c(1:10,20:30)

seq(1,10)
seq(1,10, by=2)
seq(0.5,2.5, by=.25)

rep(1:4, times=4)
rep(1:4, times=c(1,2,1,2))
rep(1:4, times=1:4)
rep(1:4, times=4:1)

rep(1:4, each=4)
rep(1:4, each=4, len=12)
rep(1:4, each=4, times=2)

# Generating vectors as statistical distribution

runif(100)                    # Uniform distribution
runif(100, min=1, max=10)

rbinom(100,4,0.5)             # Binomial distribution

rnorm(100)                   # Normal distribution
rnorm(100,0,1)                    # mean=0, sd=1, = rnorm(100)
rnorm(100,2,1)

rpois(100,2)                  # Poisson distribution
rgamma(100,1)                 # Gamma distribution
rt(100,10)                    # Student't Distribution

# Ordering vectors

iris_sp
sort(iris_sp)
order(iris_sp)
iris_sp[order(iris_sp)]       # the same than sort(iris_df)

rank(iris_sp)
rank(iris_sp, ties.method = 'first')

order(iris_sp)

# -------------------
# Maths with vectors
# -------------------

2+2
x <- 4*(3+5)^2  # result of operation is assigned to x
x
x / 10

length(2)
length(x)       # In fact, a variable a vector with length = 1

x <- 1:10
2*x 
2*x + 1
x^2 

rev(x) 
sum(x) 
prod(x) 
cumsum(x)
cumprod(x)

# Example (it is useful to think in terms of excel) 
# Sum of geometric progression ab, ab^2, ab^3 

res <- 1:100
res <- 1.1^res # To think about order of ^
res <- 3 * res
sum(res)

sum(3*(1.1^(1:100)))

# ---------------------------------------
# tapply function (equivalent to groupby)
# ---------------------------------------

tapply(iris$Petal.Length, iris$Species, mean) # applies mean() function to Petal.Length gruoped by Species
tapply(iris$Petal.Length, iris$Species, sum)
tapply(iris$Petal.Length, iris$Species, max)
tapply(iris$Petal.Length, iris$Species, fivenum)

# ---------------------------------------
# R Programming
# ---------------------------------------

# Types of objects

class(iris)
is.data.frame(iris)

class(1:10)
class(c(1.2,1.5))
class(iris$Petal.Width)
is.vector(1:10)
is.integer(1:10)
is.numeric(c(1.2,1.5))

# Functions

monthly_pay_loan <- function(amount, years, rate)
{
  monthly_rate <- rate / 12 / 100 
  months_num <- 1:(years*12)
  return(amount / sum(1 / (1+monthly_rate)^months_num))
}

monthly_pay_loan(217000,20,0.8)

# Conditional statements

xln <- function(x)
{
   if (x == 0)
     return(0)
   else
     return(-x*log(x))
}

xln(20)

# Loops

my_factorial <- function(n)
{
  result <- 1
  for (i in 1:n) { result <- result * i }
  return(result)
}

my_factorial(4)

# apply functions

cuadrado <- function(x) x^2
sapply(1:10, cuadrado)
lapply(1:10, cuadrado)
replicate(2, cuadrado)

replicate(10,mean(rnorm(1000,0,0.3)))
          