library(xgboost)

setwd("~/00_dsc/kschool_master_data_science/Master-in-Data-Science/final_project/R")

df_train <- read.csv('../data/train_reduced.csv', header = TRUE, sep = ',', na.strings = c("","NA"))
df_target <- df_train$TARGET
df_train$ID <- NULL
df_train$TARGET <- NULL

X <- as.matrix(df_train)
y <- df_target

nrounds <- 50
xgb_param <- expand.grid(eta = c(0.01,0.015,0.02),
                      max.depth = 5,
                      colsample_bytree = c(0.25,0.5,0.75),
                      subsample = c(0.25,0.5,0.75),
                      gamma = 0,
                      min_child_weight = 1)

result_cv <- data.frame(id=1:50)

for (n in 1:nrow(xgb_param)){
  set.seed(1234)
  xgb_row <- xgb.cv(data = X,
                    label = y,
                    nrounds = nrounds,
                    objective = 'binary:logistic',
                    eta = xgb_param[n,'eta'],
                    max.depth = xgb_param[n,'max.depth'],
                    gamma = xgb_param[n,'gamma'],
                    min_child_weight = xgb_param[n,'min_child_weight'],
                    colsample_bytree = xgb_param[n,'colsample_bytree'],
                    subsample = xgb_param[n,'subsample'],
                    missing = NA,
                    nfold = 5,
                    metrics = list('auc','logloss','error'),
                    early.stop.round = 150,
                    maximize=F,
                    verbose=0)
  result_cv[[paste0(n)]] <- xgb_row$test.auc.mean
}

# 451 for i=1

write.csv(result_cv, '../data/result_cv.csv')
