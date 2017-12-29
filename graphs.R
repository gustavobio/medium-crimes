require(ggplot2)
require(corrplot)
require(sf)
require(viridis)

corrplot(cor(select(sp_all, -id, -cidade)))

sp_all %>%
  filter(cidade != "São Paulo") %>%
  ggplot(aes(x = populacao, y = n_pessoas_sem_instrucao, group = 1)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("População") + 
  ylab("Número de pessoas sem instrução por 1000 habitantes") +
  theme_bw()

sp_all %>%
  select(cidade, populacao, n_pessoas_sem_instrucao) %>%
  mutate(pct_sem_instrucao = n_pessoas_sem_instrucao/10) %>%
  select(-n_pessoas_sem_instrucao) %>%
  arrange(desc(pct_sem_instrucao)) %>%
  head(5)

sp_all %>%
  filter(populacao >= 150000) %>%
  select(-id, -cidade) %>%
  cor() %>%
  corrplot(tl.col = "black")

sp_all %>%
  filter(cidade != "São Paulo") %>%
  ggplot(aes(x = populacao, y = furto_outros)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("População") + 
  ylab("Número de furtos por 1000 habitantes") +
  theme_bw()

sp_all %>%
  select(cidade, populacao) %>%
  mutate(cidade = reorder(cidade, populacao)) %>%
  arrange(desc(populacao)) %>%
  top_n(20) %>%
  ggplot(aes(x = cidade, y = populacao)) + 
  geom_bar(stat = "identity") + 
  xlab("Município") + 
  ylab("População") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) + 
  coord_flip() + 
  theme_bw() 

sp_all %>%
  filter(cidade != "São Paulo") %>%
  ggplot(aes(x = populacao, y = roubos_outros)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("População") + 
  ylab("Número de roubos por 1000 habitantes") +
  theme_bw()

sp_all %>%
  filter(populacao >= 10^5) %>%
  select(cidade, populacao, furto_outros) %>%
  arrange(furto_outros)

sp_sf %>% 
  right_join(select(sp_all, cidade, n_pessoas_sem_instrucao), by = c("municipio" = "cidade")) %>%
  ggplot() + 
  geom_sf(aes(fill = n_pessoas_sem_instrucao/10), colour = "black", size = 0.2) + 
  theme_minimal() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c(option = "plasma", name = "% de habitantes com fundamental incompleto") + 
  guides(fill = guide_colorbar(barwidth = 10, title.position = "top")) + 
  theme(legend.position = "bottom") 

sp_sf %>% 
  right_join(select(sp_all, cidade, furto_outros), by = c("municipio" = "cidade")) %>%
  ggplot() + 
  geom_sf(aes(fill = furto_outros), colour = "black", size = 0.2) + 
  theme_minimal() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c(option = "plasma", name = "Furtos por 1000 habitantes") + 
  guides(fill = guide_colorbar(barwidth = 10, title.position = "top")) + 
  theme(legend.position = "bottom")

sp_sf %>% 
  right_join(select(sp_all, cidade, roubos_outros), by = c("municipio" = "cidade")) %>%
  ggplot() + 
  geom_sf(aes(fill = roubos_outros), colour = "black", size = 0.2) + 
  theme_minimal() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c(option = "plasma", name = "Roubos por 1000 habitantes") + 
  guides(fill = guide_colorbar(barwidth = 10, title.position = "top")) + 
  theme(legend.position = "bottom")

pca <- prcomp(select(sp_all, 
                     furto_outros,
                     furto_veiculos,
                     roubos_outros,
                     roubo_banco,
                     roubo_veiculo,
                     roubo_carga,
                     homicidio_doloso,
                     latrocinio))
pca_scores <- data.frame(pca$x[, 1:2])
pca_scores$cidade <- sp_all$cidade
pca_scores$populacao <- sp_all$populacao

highlights <- pca_scores %>%
  arrange(desc(PC1)) %>%
  head(6) %>%
  bind_rows(., head(arrange(pca_scores, PC2)))

ggplot(pca_scores, aes(x = PC1, y = PC2)) + 
  geom_point(aes(size = sqrt(populacao)), alpha = 0.3, col = "royalblue") + 
  geom_text(aes(x = PC1 + 2, y = PC2, label = cidade), data = highlights, check_overlap = T, nudge_x = -5) + 
  theme_bw() + 
  theme(legend.position = "none")

sp_all %>%
  ggplot(aes(x = roubos_outros, y = furto_outros)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("Roubos por 1000 habitantes") + 
  ylab("Furtos por 1000 habitantes") + 
  theme_bw()

sp_all %>%
  ggplot(aes(x = roubos_outros, y = furto_outros)) + 
  geom_point(alpha = 0.3, aes(size = populacao)) + 
  geom_smooth() + 
  xlab("Roubos por 1000 habitantes") + 
  ylab("Furtos por 1000 habitantes") + 
  theme_bw() + 
  theme(legend.position = "none")

sp_all %>%
  ggplot(aes(x = pib, y = furto_outros)) + 
  geom_point(alpha = 0.3, aes(size = populacao)) + 
  geom_smooth() + 
  xlab("PIB per capita (reais)") + 
  ylab("Furtos por 1000 habitantes") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(label = scales::unit_format("K", scale = 1e-3, sep = ""))

sp_all %>%
  ggplot(aes(x = pib, y = roubos_outros)) + 
  geom_point(alpha = 0.3, aes(size = populacao)) + 
  geom_smooth() + 
  xlab("PIB per capita (reais)") + 
  ylab("Roubos por 1000 habitantes") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(label = scales::unit_format("K", scale = 1e-3, sep = ""))

sp_all %>%
  ggplot(aes(x = salario_medio, y = furto_outros)) + 
  geom_point(alpha = 0.3, aes(size = populacao)) + 
  geom_smooth() + 
  xlab("Salário mensal médio (salários mínimos)") + 
  ylab("Furtos por 1000 habitantes") + 
  theme_bw() + 
  theme(legend.position = "none")

sp_all %>%
  ggplot(aes(x = salario_medio, y = roubos_outros)) + 
  geom_point(alpha = 0.3, aes(size = populacao)) + 
  geom_smooth() + 
  xlab("Salário mensal médio (salários mínimos)") + 
  ylab("Roubos por 1000 habitantes") + 
  theme_bw() + 
  theme(legend.position = "none")

sp_sf %>% 
  right_join(select(sp_all, cidade, pib), by = c("municipio" = "cidade")) %>%
  ggplot() + 
  geom_sf(aes(fill = pib), colour = "black", size = 0.2) + 
  theme_minimal() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c(label = scales::unit_format("K", scale = 1e-3, sep = ""), option = "plasma", name = "PIB per capita") + 
  guides(fill = guide_colorbar(barwidth = 10, title.position = "top")) + 
  theme(legend.position = "bottom")

sp_sf %>% 
  right_join(select(sp_all, cidade, salario_medio), by = c("municipio" = "cidade")) %>%
  ggplot() + 
  geom_sf(aes(fill = salario_medio), colour = "black", size = 0.2) + 
  theme_minimal() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c(option = "plasma", name = "Salário médio mensal (salários mínimos)") + 
  guides(fill = guide_colorbar(barwidth = 10, title.position = "top")) + 
  theme(legend.position = "bottom")

sp_all %>%
  select(cidade, pib) %>%
  mutate(cidade = reorder(cidade, pib)) %>%
  arrange(desc(pib)) %>%
  top_n(20) %>%
  ggplot(aes(x = cidade, y = pib)) + 
  geom_bar(stat = "identity") + 
  xlab("Município") + 
  ylab("PIB per capita") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) + 
  coord_flip() + 
  theme_bw() 

sp_all %>%
  select(cidade, salario_medio) %>%
  mutate(cidade = reorder(cidade, salario_medio)) %>%
  arrange(desc(salario_medio)) %>%
  top_n(20) %>%
  ggplot(aes(x = cidade, y = salario_medio)) + 
  geom_bar(stat = "identity") + 
  xlab("Município") + 
  ylab("Salário mensal médio (salários mínimos)") + 
  coord_flip() + 
  theme_bw() 

sp_all %>%
  mutate(furtos = furto_outros + furto_veiculos) %>%
  select(cidade, furtos) %>%
  mutate(cidade = reorder(cidade, furtos)) %>%
  arrange(desc(furtos)) %>%
  top_n(20) %>%
  ggplot(aes(x = cidade, y = furtos)) + 
  geom_bar(stat = "identity") + 
  xlab("Município") + 
  ylab("Número de furtos por 1000 habitantes") + 
  coord_flip() + 
  theme_bw() 

sp_all %>%
  mutate(roubos = roubos_outros + roubo_banco + roubo_carga + roubo_veiculo) %>%
  select(cidade, roubos) %>%
  mutate(cidade = reorder(cidade, roubos)) %>%
  arrange(desc(roubos)) %>%
  top_n(20) %>%
  ggplot(aes(x = cidade, y = roubos)) + 
  geom_bar(stat = "identity") + 
  xlab("Município") + 
  ylab("Número de roubos por 1000 habitantes") + 
  coord_flip() + 
  theme_bw() 
