source("helper_functions.R")
require(rvest)
require(stringr)
require(tidyr)
require(purrr)
require(ggplot2)
require(corrplot)
require(caret)
require(tidyverse)

# Get IBGE city names and ids
sp_cities <- get_ids()

# Get education data for all cities
sp_education <- map(sp_cities$id, get_education)

# Get pib for all cities
sp_pib <- map(sp_cities$id, get_pib_per_capita)

# Population
sp_pop <- map(sp_cities$id, get_population)

# Jobs and salaries
sp_jobs <- map(sp_cities$id, get_jobs)

# Presidential voting
sp_voting <- map(sp_cities$id, get_presidential_voting)

# Census
sp_census <- map(sp_cities$id, get_census_2010)

# Number of vehicles
sp_vehicles <- map(sp_cities$id, get_vehicles)

# Bind all data
sp_education_df <- bind_rows(sp_education)

sp_pib_df <- bind_rows(sp_pib)

sp_pop_df <- bind_rows(sp_pop)

sp_jobs_df <- bind_rows(sp_jobs)

sp_voting_df <- bind_rows(sp_voting)

sp_census_df <- bind_rows(sp_census)

sp_vehicles_df <- bind_rows(sp_vehicles)

ssp_mun <- get_ssp_mun()

crime_data <- map(ssp_mun$ssp_id[-1], get_crime_data)

# Do all city names match?
all(unlist(lapply(crime_data, function(x) attributes(x)$cidade)) %in% ssp_mun$cidade)

names(crime_data) <- ssp_mun$ssp_id[-1]

# Bind crime data
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

# Polygons for all cities except Ilhabela
sp_sf <- filter(brmap_municipio, sigla == "SP")
sp_sf <- sp_sf %>%
  mutate(municipio = match_city(municipio, sp_cities$cidade)$y)
  
# Join all data frames
sp_all <- left_join(sp_education_df, sp_cities) %>%
  left_join(sp_pib_df) %>%
  left_join(sp_pop_df) %>%
  left_join(sp_jobs_df) %>%
  left_join(sp_voting_df) %>%
  left_join(sp_census_df) %>%
  left_join(sp_vehicles_df) %>%
  left_join(crime_data_df) %>%
  rename_all(remove_accents)

# Keep only the most relevant variables
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
    automovel,
    caminhao,
    total_de_veiculos,
    salarios_total = `salarios_e_outras_remuneracões`, 
    numero_de_unidades_locais,
    domicilios_coletivos
  )

# Transform absolute numbers into per thousand people
sp_all <- sp_all %>%
  mutate_at(vars(estupro:tentativa_homicidio), funs(./populacao*1000)) %>%
  mutate_at(vars(docentes_fundamental:numero_empresas), funs(./populacao*1000)) %>%
  mutate(populacao_urbana = populacao_urbana/populacao) %>%
  mutate_at(vars(motocicleta:automovel), funs(./populacao*1000)) %>%
  select(cidade, populacao, everything())
