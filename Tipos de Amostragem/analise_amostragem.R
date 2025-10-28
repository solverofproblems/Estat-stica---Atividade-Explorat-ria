# ============================================================================
# ANÁLISE DE AMOSTRAGEM PROBABILÍSTICA - DADOS DO CENSO AMERICANO 2017
# Fonte: American Community Survey (ACS) 2017 Census Tract Data
# ============================================================================

library(tidyverse)
library(ggplot2)

# ============================================================================
# CARREGAMENTO E LIMPEZA DOS DADOS
# ============================================================================
cat("=== CARREGANDO DADOS ===\n")

censo <- read.csv("Censo/acs2017_census_tract_data.csv", stringsAsFactors = FALSE)

censo <- censo %>% 
  filter(TotalPop > 0) %>%
  filter(!is.na(Income) & !is.na(Poverty) & !is.na(Unemployment)) %>%
  filter(Income > 0)

cat("Registros após limpeza:", nrow(censo), "\n")

# ============================================================================
# PARÂMETROS DE AMOSTRAGEM
# ============================================================================
N <- nrow(censo)
n <- 1000
k <- floor(N / n)

cat("Tamanho da população (N):", N, "\n")
cat("Tamanho da amostra (n):", n, "\n")
cat("Intervalo sistemático (k):", k, "\n\n")

# ============================================================================
# AMOSTRAGEM ALEATÓRIA SIMPLES
# ============================================================================
cat("=== AMOSTRAGEM ALEATÓRIA SIMPLES ===\n")

set.seed(123)
srs_amostra <- censo %>% sample_n(n, replace = FALSE)

cat("Amostra SRS:", nrow(srs_amostra), "registros\n")

# ============================================================================
# AMOSTRAGEM SISTEMÁTICA
# ============================================================================
cat("=== AMOSTRAGEM SISTEMÁTICA ===\n")

set.seed(123)
inicio <- sample(1:k, 1)
indices_sistematicos <- seq(inicio, N, by = k)
if(length(indices_sistematicos) > n) {
  indices_sistematicos <- indices_sistematicos[1:n]
}

sistematica_amostra <- censo[indices_sistematicos, ]

cat("Amostra Sistemática:", nrow(sistematica_amostra), "registros\n")

# ============================================================================
# AMOSTRAGEM ESTRATIFICADA
# ============================================================================
cat("=== AMOSTRAGEM ESTRATIFICADA ===\n")

set.seed(123)
estratificada_amostra <- censo %>%
  group_by(State) %>%
  sample_n(size = min(nrow(.), round(n * n() / N)), replace = FALSE) %>%
  ungroup()

if(nrow(estratificada_amostra) != n) {
  diff_n <- n - nrow(estratificada_amostra)
  set.seed(456)
  adicional <- censo %>% 
    anti_join(estratificada_amostra, by = "TractId") %>%
    sample_n(min(diff_n, nrow(.)))
  estratificada_amostra <- bind_rows(estratificada_amostra, adicional)
}

cat("Amostra Estratificada:", nrow(estratificada_amostra), "registros\n")

# ============================================================================
# AMOSTRAGEM POR CONGLOMERADO (CLUSTERS)
# ============================================================================
cat("=== AMOSTRAGEM POR CONGLOMERADO ===\n")

set.seed(123)
n_clusters <- ceiling(n / 50)
clusters_selecionados <- sample(unique(censo$County), min(n_clusters, length(unique(censo$County))))

cluster_amostra <- censo %>%
  filter(County %in% clusters_selecionados)

if(nrow(cluster_amostra) > n) {
  set.seed(789)
  cluster_amostra <- cluster_amostra %>% sample_n(n)
}

cat("Amostra por Clusters:", nrow(cluster_amostra), "registros\n\n")

# ============================================================================
# GRÁFICO 1: ALEATÓRIA SIMPLES - HISTOGRAMA DE INCOME
# ============================================================================
cat("=== GRÁFICO 1: ALEATÓRIA SIMPLES ===\n")

p1 <- ggplot(srs_amostra, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = mean(srs_amostra$Income), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "AMOSTRAGEM ALEATÓRIA SIMPLES",
    subtitle = "Histograma da Renda (Income) - Regularidade Salarial dos Indivíduos",
    x = "Renda (Income em USD)",
    y = "Frequência",
    caption = "Fonte: ACS 2017 Census Tract Data | Linha vermelha = Média"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkblue"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
  ) +
  scale_x_continuous(labels = scales::dollar_format())

print(p1)

cat("COMPORTAMENTO: Seleção completamente aleatória mostra a regularidade\n")
cat("salarial dos indivíduos na amostra. A distribuição representa bem\n")
cat("a população total com variabilidade natural.\n\n")

# ============================================================================
# GRÁFICO 2: SISTEMÁTICA - LINHA DE INCOME AO LONGO DO ÍNDICE
# ============================================================================
cat("=== GRÁFICO 2: SISTEMÁTICA ===\n")

sistematica_amostra <- sistematica_amostra %>% mutate(indice = 1:nrow(.))

p2 <- ggplot(sistematica_amostra, aes(x = indice, y = Income)) +
  geom_line(color = "darkgreen", size = 0.8) +
  geom_point(color = "darkgreen", alpha = 0.4, size = 1) +
  labs(
    title = "AMOSTRAGEM SISTEMÁTICA",
    subtitle = "Renda ao longo do Índice da População - Regularidade da Amostragem",
    x = "Índice da População",
    y = "Renda (Income em USD)",
    caption = "Fonte: ACS 2017 Census Tract Data"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", color = "darkgreen"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0)) +
  scale_y_continuous(labels = scales::dollar_format())

print(p2)
cat("COMPORTAMENTO: Amostragem sistemática mostra regularidade da seleção.\n\n")

# ============================================================================
# GRÁFICO 3: ESTRATIFICADA - MÉDIA DE INCOME POR RAÇA
# ============================================================================
cat("=== GRÁFICO 3: ESTRATIFICADA ===\n")

estratificada_amostra <- estratificada_amostra %>%
  mutate(RacaDominante = case_when(
    White > Black & White > Asian ~ "Branca",
    Black > White & Black > Asian ~ "Negra",
    Asian > White & Asian > Black ~ "Asiatica",
    TRUE ~ "Outras"
  )) %>% filter(!is.na(RacaDominante))

renda_por_raca <- estratificada_amostra %>%
  group_by(RacaDominante) %>%
  summarise(Media_Renda = mean(Income), .groups = 'drop') %>%
  arrange(Media_Renda)

p3 <- ggplot(renda_por_raca, aes(x = reorder(RacaDominante, Media_Renda), 
                                     y = Media_Renda, fill = RacaDominante)) +
  geom_col(alpha = 0.8) +
  labs(title = "AMOSTRAGEM ESTRATIFICADA",
       subtitle = "Media de Renda por Raca Dominante",
       x = "Raca Dominante",
       y = "Renda Media (Income em USD)",
       caption = "Fonte: ACS 2017 Census Tract Data") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", color = "darkorange"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
        legend.position = 'none') +
  scale_fill_brewer(palette = 'Set2') +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip()

print(p3)
cat("COMPORTAMENTO: Compara grupos homogeneos (racas).\n\n")

# ============================================================================
# GRÁFICO 4: CONGLOMERADO - INCOME MÉDIO POR STATE
# ============================================================================
cat("=== GRÁFICO 4: CONGLOMERADO ===\n")

income_por_state <- cluster_amostra %>%
  group_by(State) %>%
  summarise(Media_Income = mean(Income), .groups = 'drop') %>%
  arrange(Media_Income) %>%
  tail(15)

p4 <- ggplot(income_por_state, aes(x = reorder(State, Media_Income), 
                                     y = Media_Income, fill = State)) +
  geom_col(alpha = 1.0) +
  labs(title = "AMOSTRAGEM POR CONGLOMERADO",
       subtitle = "Renda Media por Estado",
       x = "Estado",
       y = "Renda Media (Income em USD)",
       caption = "Fonte: ACS 2017 Census Tract Data | Top 15 estados") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", color = "darkred"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
        legend.position = 'none') +
  scale_fill_viridis_d(option = "plasma", alpha = 1.0) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip()

print(p4)
cat("COMPORTAMENTO: Mostra variacoes geograficas por estado.\n\n")

cat("=== ANALISE FINALIZADA ===\n")
