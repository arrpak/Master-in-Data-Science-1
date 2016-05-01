# LOADING DATA

dfTrain <- read.csv('data/train.csv', header = TRUE, sep = ',', na.strings = c("","NA"))
dim(dfTrain)

# Eliminate columns with one different valor

dfTrain <- dfTrain[,colSums(dfTrain) != 0]
dim(dfTrain)

# Initial review od data

name <- names(dfTrain)[1:336]
type <- rep(0,336)
i <- 0
for (col in 1:336){
  i <- i+1
  type[col] <- typeof(dfTrain[,col])
}
numvalors <- rep(0,336)
i <- 0
for (col in 1:336){
  i <- i+1
  numvalors[col] <- length(unique(dfTrain[,col]))
}
dfUniques <- data.frame(name,type,numvalors)
dfUniques[order(dfUniques$name),]

# To Convert to categorical columns beginning by 'ind'

attach(dfTrain)
col.cat <- name[grepl("ind",name)]
for (col in col.cat){
    dfTrain[[col]] <- as.factor(dfTrain[[col]])
}

cor(dfTrain$var3,dfTrain$var15, method="kendall", use="pairwise") 


# LOGISTIC REGRESSION

name <- names(dfTrain)[2:336]

type <- rep(0,335)
i <- 0
for (col in 1:335){
  i <- i+1
  type[col] <- typeof(dfTrain[,col])
}

numvalors <- rep(0,335)
i <- 0
for (col in 1:335){
  i <- i+1
  numvalors[col] <- length(unique(dfTrain[,col]))
}

tstat <- rep(0,335)
pvalue <- rep(0,335)
dfPredictors <- data.frame(name,type,numvalors,tstat,pvalue)

i <- 0
for (col in names(dfTrain)[2:336]){
  i <- i+1
  glm.fit <- glm(substitute(TARGET ~ i, list(i = as.name(col))), data = dfTrain)
  dfPredictors[i,4] <- summary(glm.fit)$coef[2,3]
  dfPredictors[i,5] <- summary(glm.fit)$coef[2,4]
}

dfPredictors <- dfPredictors[order(dfPredictors$pvalue),]
head(dfPredictors,50)


glm.fit <- glm(TARGET~ind_var30+num_var30+num_var30, data = dfTrain, family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,type = "response")
summary(glm.probs)


glm.pred <- rep(0,nrow(dfTrain))
glm.pred[glm.probs < median(glm.probs)] <- 1
table(glm.pred,dfTrain$TARGET)                          # Confusion matrix
mean(glm.pred == dfTrain$TARGET)


require(glmnet)
x <- subset(dfTrain, select=-TARGET)
x <- subset(x,select=-ID)
y <- dfTrain$TARGET
glmmod<-glmnet(as.matrix(x),as.factor(y),alpha=1,family='binomial')
summary(glmmod)

cvlasso <- cv.glmnet(as.matrix(x),y)
plot(cvlasso)
coef(cvlasso,s='lambda.min')
