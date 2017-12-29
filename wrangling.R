source("helper_functions.R")
require(rvest)
require(stringr)
require(tidyr)
require(purrr)
require(ggplot2)
require(corrplot)
require(caret)
require(tidyverse)

sp_cities <- get_ids()

sp_education <- map(sp_cities$id, get_education)

sp_pib <- map(sp_cities$id, get_pib_per_capita)

sp_pop <- map(sp_cities$id, get_population)

sp_jobs <- map(sp_cities$id, get_jobs)

sp_voting <- map(sp_cities$id, get_presidential_voting)

sp_census <- map(sp_cities$id, get_census_2010)

sp_vehicles <- map(sp_cities$id, get_vehicles)

sp_education_df <- bind_rows(sp_education)

sp_pib_df <- bind_rows(sp_pib)

sp_pop_df <- bind_rows(sp_pop)

sp_jobs_df <- bind_rows(sp_jobs)

sp_voting_df <- bind_rows(sp_voting)

sp_census_df <- bind_rows(sp_census)

sp_vehicles_df <- bind_rows(sp_vehicles)

ssp_mun <- get_ssp_mun()

crime_data <- map(ssp_mun$ssp_id[-1], get_crime_data)

all(unlist(lapply(crime_data, function(x) attributes(x)$cidade)) %in% ssp_mun$cidade)

names(crime_data) <- ssp_mun$ssp_id[-1]

crime_data_df <- bind_rows(lapply(crime_data, function(x) {
  city <- attributes(x)$cidade
  x$Total <- gsub("\\.", "", as.character(x$Total))
  x$Total <- as.numeric(gsub(",", ".", x$Total))
  res <- select(x, Natureza, Total) %>%
    spread(Natureza, Total)
  names(res) <- c("estupro", 
                  "furto_outros", 
                  "furto_veiculos", 
                  "homicidio_culposo_outros",
                  "homicidio_culposo_acidente",
                  "homicidio_doloso",
                  "homicidio_doloso_acidente",
                  "latrocinio",
                  "lesao_culposa_outros",
                  "lesao_culposa_acidente",
                  "lesao_dolosa",
                  "n_vitimas_homicidio_doloso",
                  "n_vitimas_homicidio_doloso_acidente",
                  "n_vitimas_latrocinio",
                  "roubos_outros",
                  "roubo_banco",
                  "roubo_carga",
                  "roubo_veiculo",
                  "tentativa_homicidio")
  res$cidade <- city
  select(res, cidade, everything())
}))

crime_data_df <- crime_data_df %>%
  mutate(cidade = match_city(cidade, sp_cities$cidade)$y)

sp_sf <- filter(brmap_municipio, sigla == "SP")
sp_sf <- sp_sf %>%
  mutate(municipio = match_city(municipio, sp_cities$cidade)$y)
  
sp_all <- left_join(sp_education_df, sp_cities) %>%
  left_join(sp_pib_df) %>%
  left_join(sp_pop_df) %>%
  left_join(sp_jobs_df) %>%
  left_join(sp_voting_df) %>%
  left_join(sp_census_df) %>%
  left_join(sp_vehicles_df) %>%
  left_join(crime_data_df) %>%
  rename_all(remove_accents)

sp_all <- sp_all %>%
  select(
    cidade,
    populacao = `populacao_residente`,
    estupro:tentativa_homicidio,
    pib,
    populacao_urbana = `populacao_residente_urbana`,
    docentes_fundamental = doc_ens_fun,
    pessoal_ocupado = `pessoal_ocupado_total`,
    n_pessoas_sem_instrucao = `pessoas_de_10_anos_ou_mais_de_idade_sem_instrucao_e_fundamental_incompleto_-_total`,
    numero_empresas = `numero_de_empresas_atuantes`,
    salario_medio = `salario_medio_mensal`,
    pessoas_por_casa = `media_de_moradores_em_domicilios_particulares_ocupados`,
    motocicleta,
    motoneta,
    automovel
  )

sp_all <- sp_all %>%
  mutate_at(vars(estupro:tentativa_homicidio), funs(./populacao*1000)) %>%
  mutate_at(vars(docentes_fundamental:numero_empresas), funs(./populacao*1000)) %>%
  mutate(populacao_urbana = populacao_urbana/populacao) %>%
  mutate_at(vars(motocicleta:automovel), funs(./populacao*1000)) %>%
  select(cidade, populacao, everything())

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

