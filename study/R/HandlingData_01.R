# ---------------------------
#  Packages: rshape2 and plyr
# ---------------------------

library(reshape2)
library(plyr)

# Other way to order dataframes included into plyr package

arrange(iris,Sepal.Length,Petal.Length)
arrange(iris,Sepal.Length+Petal.Length) 

# Long format: melt

df1 <- read.table("pob_aragon_2014.csv",header=TRUE)
melt(df1)

df2 <- read.table("pob_aragon.csv",header=TRUE)
df2

melt(df2) # no useful
melt(df2,id.var=c("Provincia","Periodo"))
melt(df2,id.var=c("Periodo","Provincia"))

# width format: dcast

long_df3 <- melt(df1)
long_df3
dcast(long_df3, Provincia ~ variable)
dcast(long_df3, variable ~ Provincia)

# Aggregations

head(iris)
long_df4 <- melt(iris)
head(long_df4)

dcast(long_df4,Species ~ variable) # There are 50 Sepal.Width values for specie=setosa
dcast(long_df4,Species ~ variable, fun.aggregate = var)

df5 <- read.csv("carburantes_20160121.csv", header=TRUE, sep=";",dec=',')
colnames(df5) 
df6 <- melt(df5[,c(1,10,11)], na.rm=TRUE)

arrange(dcast(df6, Provincia ~ variable, fun.aggregate = mean),Precio..Gasoleo.A)
# vs
tapply(df5$Precio..Gasoleo.A,df5$Provincia,var,na.rm=TRUE)

# Processing by parts (rows)

head(airquality)
airquality_long <- melt(airquality, id.vars=c("Month","Day"), na.rm = TRUE)
head(airquality_long)

ddply(airquality_long, .(variable), summarize, avg = mean(value))          # grouping by variable Output is a dataframe
cbind(tapply(airquality_long$value, airquality_long$variable, mean))       # grouping by variable Output is a matrix

# Using my functions

geo_mean <- function(x) exp(mean(log(x)))

ddply(iris, .(Species), summarize, pl_geo_mean = geo_mean(Petal.Length))

