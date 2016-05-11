library(xgboost)

setwd("~/00_dsc/kschool_master_data_science/Master-in-Data-Science/final_project/R")

df_train <- read.csv('../data/train_reduced.csv', header = TRUE, sep = ',', na.strings = c("","NA"))
df_target <- df_train$TARGET
df_train$ID <- NULL
df_train$TARGET <- NULL

xgb_data <- xgb.DMatrix(data.matrix(df_train), label = as.numeric(df_target))

all_lines <- NULL
general_param <- list(objective = "binary:logistic",
                      booster = "gbtree",
                      eval_metric = "auc")

for (n in 1:300){

    set.seed(1967+n)

    tree_param <- list(eta = runif(1, 0.010, 0.04),
                       max.depth = sample(5:8, 1),
                       max_delta_step = sample(0:3, 1),
                       subsample = runif(1, 0.7, 0.99),
                       colsample_bytree = runif(1, 0.5, 0.99))
              
    xgb_param <- append(general_param,tree_param)              
              
    model_cv <- xgb.cv(param = xgb_param,
                       data = xgb_data,
                       nrounds = 1e4,
                       nfold = 10,
                       early.stop.round = 100,
                       print.every.n = 1000L)

    new_line <- data.frame( eta = tree_param$eta,
                            max.depth = tree_param$max.depth,
                            max_delta_step = tree_param$max_delta_step,
                            subsample = tree_param$subsample,
                            colsample_bytree = tree_param$colsample_bytree,
                            best_itr = which.max(model_cv$test.auc.mean),
                            best_auc = max(model_cv$test.auc.mean))
    print(new_line)

    all_lines <- rbind(all_lines, new_line)
}

write.csv(all_lines,"../data/result_cv_03.csv", row.names = FALSE)

