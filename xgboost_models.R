sp_all_xgboost <- filter(sp_all, populacao >= 60000)
sp_all_index <- createDataPartition(sp_all_xgboost$furto_outros, p = 0.7, times = 1, list = FALSE)
sp_all_train <- sp_all_xgboost[sp_all_index, ]
sp_all_test <- sp_all_xgboost[-sp_all_index, ]
sp_all_train_label <- sp_all_train$furto_outros 
sp_all_test_label <- sp_all_test$furto_outros
sp_all_train <- select(sp_all_train, -(cidade:tentativa_homicidio))
sp_all_test <- select(sp_all_test, -(cidade:tentativa_homicidio))

params <-
  list(
    objective = "reg:linear",
    eval_metric = "rmse",
    max.depth = 2,
    eta = 0.3,
    silent = 1,
    min_child_weight = 2
  )
bst <-
  xgboost(
    params = params,
    nfold = 5,
    booster = "gbtree",
    data = data.matrix(sp_all_train),
    label = sp_all_train_label,
    nrounds = 2000,
    early_stopping_rounds = 3,
    maximize = FALSE
  )

xgb.importance(names(sp_all_train), model = bst)

roubos_pred <- predict(bst, data.matrix(sp_all_test))
plot(roubos_pred,sp_all_test_label)
abline(c(0,1))
mean(sp_all$furto_outros)
erros <- roubos_pred - sp_all_test_label
sqrt(mean(erros^2))

