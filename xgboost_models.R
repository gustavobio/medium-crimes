sp_all <- sp_all %>%
  mutate(furto_total = furto_outros + furto_veiculos)

sp_all %>%
  mutate(tamanho_cidade = ifelse(populacao >= 60000, "maior que 60000", "menor que 60000")) %>%
  ggplot(aes(x = furto_total, fill = tamanho_cidade)) + 
  geom_density(alpha = 0.5)

sp_all %>%
  ggplot() + 
  geom_histogram(aes(x = log(pib)), bins = 50) + 
  theme_bw()

sp_all %>%
  mutate(tamanho_cidade = ifelse(populacao >= 60000, "maior que 60000", "menor que 60000")) %>%
  ggplot() + 
  geom_density(aes(x = pessoas_por_casa, fill = tamanho_cidade), alpha = 0.5) + 
  theme_bw()

sp_all %>%
  mutate(tamanho_cidade = ifelse(populacao >= 60000, "maior que 60000", "menor que 60000")) %>%
  ggplot(aes(x = log(pib), fill = tamanho_cidade)) +
  geom_density(alpha = 0.5) + 
  theme_bw()

sp_all_xgboost <- sp_all

set.seed(10)
sp_all_index <- createDataPartition(sp_all_xgboost$furto_outros, p = 0.7, times = 1, list = FALSE)
sp_all_train <- sp_all_xgboost[sp_all_index, ]
sp_all_test <- sp_all_xgboost[-sp_all_index, ]
sp_all_train_label <- sp_all_train$furto_outros 
sp_all_test_label <- sp_all_test$furto_outros
sp_all_train <- select(sp_all_train, -(cidade:tentativa_homicidio), -furto_total, -motoneta, populacao)
sp_all_test <- select(sp_all_test, -(cidade:tentativa_homicidio), -furto_total, -motoneta, populacao)

params <-
  list(
    objective = "reg:linear",
    eval_metric = "rmse",
    max.depth = 2,
    eta = 0.35,
    silent = 1,
    min_child_weight = 0.3
  )
bst <-
  xgboost(
    params = params,
    nfold = 10,
    booster = "gbtree",
    data = data.matrix(sp_all_train),
    label = sp_all_train_label,
    nrounds = 3000,
    early_stopping_rounds = 3,
    maximize = FALSE
  )

bst$evaluation_log %>%
  ggplot() + 
  geom_line(aes(x = iter, y = train_rmse_mean), col = "red") + 
  geom_line(aes(x = iter, y = test_rmse_mean), col = "green")

xgb.importance(names(sp_all_train), model = bst)

roubos_pred <- predict(bst, data.matrix(sp_all_test))
plot(roubos_pred, sp_all_test_label)
abline(c(0,1))
mean(sp_all$furto_outros)
erros <- roubos_pred - sp_all_test_label
sqrt(mean(erros^2))

