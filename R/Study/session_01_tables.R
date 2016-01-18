# ----------------------
# Inspecting a Dataframe
# ----------------------

# R package contains dataframe iris (with sepal and petal information about iris flower)

iris
print(iris)

# First steps to inspect a dataframe

plot(iris)
summary(iris)
str(iris)
colnames(iris)

# Inspecting size of dataframes

dim(iris)
nrow(iris)
ncol(iris)

# Selecting rows and columns
# --------------------------

# Output is a vector

iris[[4]]
iris[,4:4]
iris[["Petal.Width"]]
iris[,"Petal.Width"]
iris$Petal.Width

# Output is a dataframe

head(iris,10)
tail(iris,10)

iris[2:4,]
iris[,2:4]
iris[2:4,2:4]
iris[c(1,3),]
iris[1:4,c(1,3)]

iris[4]
iris["Petal.Width"]
subset(iris, select=Petal.Width)

iris[1:10,-(1:2)]
subset(iris, select=-Petal.Width)[1:10,]

iris[iris$Sepal.Width >= 3,]

# ----------------------
# Dataframes usage
# ----------------------

my_iris <- iris

# Ordering by columns (strictly, there is not a sort dataframe function)

sort(iris$Petal.Length)
order(iris$Petal.Length) # Actually, it sorts index
iris[order(iris$Petal.Length),]
iris[order(iris$Petal.Length,decreasing = TRUE),]
iris[order(-iris$Petal.Length),]

# Adding and deleting columns

my_iris$Area <- my_iris$Sepal.Length * my_iris$Sepal.Width
head(my_iris)

my_iris$Area <- NULL # to delete
head(my_iris)

transform(my_iris, Petal.Area = Petal.Width*Petal.Length, Sepal.Area = Sepal.Width*Sepal.Length)




