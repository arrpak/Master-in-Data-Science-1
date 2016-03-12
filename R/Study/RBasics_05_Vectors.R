# Vectors and assignment
# ----------------------

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
1/x
x
y <- c(x, 0, x)
y


# Vector arithmetic
# -----------------

v <- 2*x + y +1
v

# Arithmetic operators = + - * / ^
# Arithmetic functions log,exp,sin,cos,tan,sqrt.
# More arithmetics functions max,min,range,length,sort
# Statistical functions mean,var
# Two vectors functions pmax,pmin

range(x)
sort(x, decreasing = TRUE)

z <-2*y
pmax(x,z)

# Generating regular sequences
# ----------------------------

1:10
seq(1,10)
2:8
seq(2,8)
7:3
seq(7,3)

seq(5, 50, by=5)
seq(5, by=5, length=10)

seq(1, length(x))

rep(x,times=5)
rep(x,each=5)

# colon operator has high priority within an expressio
1:10-1
1:(10-1)

# Logical vectors
# ---------------

# Logical operators <,<=,>,>=,==,!=
# More logical operators &,|,!

temp <- x > 9 & x <10.6
temp

# Missing values
# --------------

# na = not available
z <- c(1, 3, NA)
is.na(z)

# nan = not a number
0/0
Inf-Inf
z <- c(1,3,4,NaN,NA)
is.nan(z)
is.na(z)

# String vectors
# --------------

s <- c("J", "A", "V", "I", "E", "R")
s
paste("col",1:10)
paste("col",1:10, sep="")

# Index vectors
# --------------

# logical vector
x
x[x > 10]
z
z[!is.na(z)]

# positive vector
x[c(1,3,5)]
x[c(1,2,1,2)]
x[1:2]
x[1:10]
c("x","y")[rep(c(1,2,2,1), times=4)]

# negative vector
x
x[-(1:3)]
x[-c(1,3,5)]

# it can receive end of assignment
z[is.na(z)] <- 0
z
z[z != 0] <- 1
z
