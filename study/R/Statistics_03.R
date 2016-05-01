?sample

df1 <- sample(0:1,100000,replace=TRUE,prob=c(0.9,0.1)) # sample(1000,100)

df1 <- rep(0:1, times=c(900,100))
v2<-replicate(100000,sum(sample(df1,100))) > 10
sum(v2)/length(v2)

res <- replicate(10000,sum(sample(1000,100) < 101 )) # por qué?
sum(res > 10)

curve(dbeta(x,4,6), from=0,to=1)
curve(dbeta(x,40,6), from=0,to=1)
curve(dbeta(x,4000,6000), from=0,to=1)

# mal mezclar dos rormales

v1 <- rnorm(1000,1,6)
v2 <- rnorm(1000,1,16)

m1 <- rbinom(1000,1,0.3)
m2 <- -(m1-1)

hist((m1*v1+m2*v2),breaks=30)

m1+m2
# bien mezclar dos normales

foo <- function()
{
  de.cual <- rbinom(1,1,0.7)
  if (de.cual ==1)
  {
    rnorm(1,6)
  }
  else
  {  rnorm(1,16) }
}
muestra <- replicate(1000,foo())
hist(muestra, breaks =30);


1 - pbinom(59.5, 100, 0.5)

binom.test(60, 100, 0.5, "greater")