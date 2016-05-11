library(xgboost)

setwd("~/00_dsc/kschool_master_data_science/Master-in-Data-Science/final_project/R")

df_train <- read.csv('../data/train_reduced.csv', header = TRUE, sep = ',', na.strings = c("","NA"))
df_target <- df_train$TARGET
df_train$ID <- NULL
df_train$TARGET <- NULL

X <- as.matrix(df_train)
y <- df_target

nrounds <- 360
xgb_param <- expand.grid(eta = c(0.01,0.02,0.03),
                         colsample_bytree = c(0.25,0.5,0.75),
                         subsample = c(0.25,0.5,0.75))

result_cv <- data.frame()

for (n in 1:nrow(xgb_param)){
  set.seed(1234)
  xgb_row <- xgb.cv(data = X,
                    label = y,
                    nrounds = nrounds,
                    objective = 'binary:logistic',
                    max.depth = 5,
                    gamma = 0,
                    min_child_weight = 1,
                    missing = NA,
                    nfold = 5,
                    metrics = list('auc','logloss','error'),
                    early.stop.round = 150,
                    maximize=F,
                    verbose=0,
                    eta = xgb_param[n,'eta'],
                    colsample_bytree = xgb_param[n,'colsample_bytree'],
                    subsample = xgb_param[n,'subsample'])
  
  result_cv <- rbind(result_cv,t(data.frame(xgb_row$test.auc.mean)))
  print(n)
}

result_cv <- cbind(xgb_param,result_cv)

write.csv(result_cv, '../data/result_cv360.csv')

which_round_max_auc <- which.max(t(apply(result_cv[,4:363],2,max)))
which_param_max_auc <- which.max(result_cv[,which_round_max_auc])

result_cv[which_param_max_auc,c(1,2,3,3+which_round_max_auc)]

nrounds <- 720
xgb_row <- xgb.cv(data = X,
                  label = y,
                  nrounds = nrounds,
                  objective = 'binary:logistic',
                  max.depth = 5,
                  gamma = 0,
                  min_child_weight = 1,
                  missing = NA,
                  nfold = 5,
                  metrics = list('auc','logloss','error'),
                  early.stop.round = 150,
                  maximize=F,
                  verbose=0,
                  eta = 0.03,
                  colsample_bytree = 0.5,
                  subsample = xgb_param = 0.75)

