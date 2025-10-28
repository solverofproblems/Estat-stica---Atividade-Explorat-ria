# ============================================================================
# ANÁLISE DOS DADOS DE FELICIDADE MUNDIAL 2019 - VERSÃO RESUMIDA
# Fonte: World Happiness Report 2019
# ============================================================================

library(tidyverse)
library(ggplot2)

# ============================================================================
# CARREGAMENTO DOS DADOS
# ============================================================================
cat("=== CARREGANDO DADOS ===\n")

happiness <- read.csv("Análise de Dados/2019.csv", stringsAsFactors = FALSE)

cat("Total de países:", nrow(happiness), "\n")
cat("Total de variáveis:", ncol(happiness), "\n\n")

# ============================================================================
# PREPARAÇÃO DOS DADOS
# ============================================================================
cat("=== PREPARANDO DADOS ===\n")

# Top 3 países com melhor ranking
top_3 <- happiness %>%
  arrange(Overall.rank) %>%
  head(3)

# Bottom 3 países com pior ranking
bottom_3 <- happiness %>%
  arrange(desc(Overall.rank)) %>%
  head(3)

cat("Top 3 países selecionados\n")
cat("Bottom 3 países selecionados\n\n")

# ============================================================================
# GRÁFICO 1: TOP 3 PAÍSES COM MELHOR RANKING
# ============================================================================
cat("=== GRÁFICO 1: TOP 3 PAÍSES ===\n")

# Usar a pontuação de felicidade (Score) - quanto maior, melhor
p1 <- ggplot(top_3, aes(x = reorder(Country.or.region, Score), y = Score)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste("Posição", Overall.rank)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "TOP 3 PAÍSES COM MELHOR RANKING DE FELICIDADE",
    subtitle = "Maior pontuação = Melhor país (Finland = maior barra)",
    x = "País",
    y = "Pontuação de Felicidade",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p1)
cat("Gráfico 1 criado: Top 3 países\n\n")

# ============================================================================
# GRÁFICO 2: BOTTOM 3 PAÍSES COM PIOR RANKING
# ============================================================================
cat("=== GRÁFICO 2: BOTTOM 3 PAÍSES ===\n")

# Usar a pontuação de felicidade (Score) - quanto menor, pior
p2 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Score), y = Score)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = paste("Posição", Overall.rank)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "BOTTOM 3 PAÍSES COM PIOR RANKING DE FELICIDADE",
    subtitle = "Menor pontuação = Pior país (South Sudan = menor barra)",
    x = "País",
    y = "Pontuação de Felicidade",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkred"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p2)
cat("Gráfico 2 criado: Bottom 3 países\n\n")

# ============================================================================
# GRÁFICO 3: PIB PER CAPITA - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 3: PIB PER CAPITA - TOP 3 ===\n")

p3 <- ggplot(top_3, aes(x = reorder(Country.or.region, GDP.per.capita), y = GDP.per.capita)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = round(GDP.per.capita, 2)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PIB PER CAPITA - TOP 3 PAÍSES MAIS FELIZES",
    subtitle = "Maior PIB per capita = Maior poder econômico",
    x = "País",
    y = "PIB per Capita",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p3)
cat("Gráfico 3 criado: PIB per capita - Top 3\n\n")

# ============================================================================
# GRÁFICO 4: GENEROSIDADE - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 4: GENEROSIDADE - TOP 3 ===\n")

p4 <- ggplot(top_3, aes(x = reorder(Country.or.region, Generosity), y = Generosity)) +
  geom_col(fill = "orange", alpha = 0.8) +
  geom_text(aes(label = round(Generosity, 2)), 
            hjust = -0.1, color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  labs(
    title = "GENEROSIDADE - TOP 3 PAÍSES MAIS FELIZES",
    subtitle = "Maior valor = Maior nível de generosidade",
    x = "País",
    y = "Generosidade",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "orange"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p4)
cat("Gráfico 4 criado: Generosidade - Top 3\n\n")

# ============================================================================
# GRÁFICO 5: PONTUAÇÃO GERAL - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 5: PONTUAÇÃO GERAL - TOP 3 ===\n")

p5 <- ggplot(top_3, aes(x = reorder(Country.or.region, Score), y = Score)) +
  geom_col(fill = "purple", alpha = 0.8) +
  geom_text(aes(label = round(Score, 2)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PONTUAÇÃO GERAL - TOP 3 PAÍSES MAIS FELIZES",
    subtitle = "Maior pontuação = Maior nível de felicidade",
    x = "País",
    y = "Pontuação Geral",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "purple"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p5)
cat("Gráfico 5 criado: Pontuação geral - Top 3\n\n")

# ============================================================================
# GRÁFICO 6: LIBERDADE PARA ESCOLHAS - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 6: LIBERDADE - TOP 3 ===\n")

p6 <- ggplot(top_3, aes(x = reorder(Country.or.region, Freedom.to.make.life.choices), 
                         y = Freedom.to.make.life.choices)) +
  geom_col(fill = "darkcyan", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "LIBERDADE PARA ESCOLHAS DE VIDA - TOP 3 PAÍSES",
    subtitle = "Nível de liberdade para tomar decisões",
    x = "País",
    y = "Liberdade para Escolhas",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkcyan"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p6)
cat("Gráfico 6 criado: Liberdade - Top 3\n\n")

# ============================================================================
# GRÁFICO 7: EXPECTATIVA DE VIDA SAUDÁVEL - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 7: EXPECTATIVA DE VIDA - TOP 3 ===\n")

p7 <- ggplot(top_3, aes(x = reorder(Country.or.region, Healthy.life.expectancy), 
                         y = Healthy.life.expectancy)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "EXPECTATIVA DE VIDA SAUDÁVEL - TOP 3 PAÍSES",
    subtitle = "Anos de vida saudável esperados",
    x = "País",
    y = "Expectativa de Vida Saudável",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkmagenta"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p7)
cat("Gráfico 7 criado: Expectativa de vida - Top 3\n\n")

# ============================================================================
# GRÁFICO 8: PIB PER CAPITA - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 8: PIB PER CAPITA - BOTTOM 3 ===\n")

p8 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, GDP.per.capita), y = GDP.per.capita)) +
  geom_col(fill = "red", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "PIB PER CAPITA - BOTTOM 3 PAÍSES MENOS FELIZES",
    subtitle = "Relação entre infelicidade e poder econômico",
    x = "País",
    y = "PIB per Capita",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "red"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p8)
cat("Gráfico 8 criado: PIB per capita - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 9: GENEROSIDADE - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 9: GENEROSIDADE - BOTTOM 3 ===\n")

p9 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Generosity), y = Generosity)) +
  geom_col(fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "GENEROSIDADE - BOTTOM 3 PAÍSES MENOS FELIZES",
    subtitle = "Nível de generosidade dos países menos felizes",
    x = "País",
    y = "Generosidade",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkorange"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p9)
cat("Gráfico 9 criado: Generosidade - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 10: PONTUAÇÃO GERAL - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 10: PONTUAÇÃO GERAL - BOTTOM 3 ===\n")

p10 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Score), y = Score)) +
  geom_col(fill = "maroon", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "PONTUAÇÃO GERAL - BOTTOM 3 PAÍSES MENOS FELIZES",
    subtitle = "Score total de felicidade",
    x = "País",
    y = "Pontuação Geral",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "maroon"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p10)
cat("Gráfico 10 criado: Pontuação geral - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 11: LIBERDADE PARA ESCOLHAS - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 11: LIBERDADE - BOTTOM 3 ===\n")

p11 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Freedom.to.make.life.choices), 
                             y = Freedom.to.make.life.choices)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "LIBERDADE PARA ESCOLHAS DE VIDA - BOTTOM 3 PAÍSES",
    subtitle = "Nível de liberdade para tomar decisões",
    x = "País",
    y = "Liberdade para Escolhas",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkred"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p11)
cat("Gráfico 11 criado: Liberdade - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 12: EXPECTATIVA DE VIDA SAUDÁVEL - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 12: EXPECTATIVA DE VIDA - BOTTOM 3 ===\n")

p12 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Healthy.life.expectancy), 
                             y = Healthy.life.expectancy)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "EXPECTATIVA DE VIDA SAUDÁVEL - BOTTOM 3 PAÍSES",
    subtitle = "Anos de vida saudável esperados",
    x = "País",
    y = "Expectativa de Vida Saudável",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "firebrick"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(p12)
cat("Gráfico 12 criado: Expectativa de vida - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 13: PERCEPÇÃO DE CORRUPÇÃO - TOP 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 13: PERCEPÇÃO DE CORRUPÇÃO - TOP 3 ===\n")

p13 <- ggplot(top_3, aes(x = reorder(Country.or.region, Perceptions.of.corruption), 
                         y = Perceptions.of.corruption)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = round(Perceptions.of.corruption, 2)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PERCEPÇÃO DE CORRUPÇÃO - TOP 3 PAÍSES MAIS FELIZES",
    subtitle = "Menor valor = Menos corrupção percebida (melhor)",
    x = "País",
    y = "Percepção de Corrupção",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p13)
cat("Gráfico 13 criado: Percepção de corrupção - Top 3\n\n")

# ============================================================================
# GRÁFICO 14: PERCEPÇÃO DE CORRUPÇÃO - BOTTOM 3 PAÍSES
# ============================================================================
cat("=== GRÁFICO 14: PERCEPÇÃO DE CORRUPÇÃO - BOTTOM 3 ===\n")

p14 <- ggplot(bottom_3, aes(x = reorder(Country.or.region, Perceptions.of.corruption), 
                            y = Perceptions.of.corruption)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = round(Perceptions.of.corruption, 2)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PERCEPÇÃO DE CORRUPÇÃO - BOTTOM 3 PAÍSES MENOS FELIZES",
    subtitle = "Maior valor = Mais corrupção percebida (pior)",
    x = "País",
    y = "Percepção de Corrupção",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkred"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p14)
cat("Gráfico 14 criado: Percepção de corrupção - Bottom 3\n\n")

# ============================================================================
# GRÁFICO 15: COMPARAÇÃO DE CORRUPÇÃO - MELHOR vs PIOR PAÍS
# ============================================================================
cat("=== GRÁFICO 15: COMPARAÇÃO MELHOR vs PIOR PAÍS ===\n")

# Melhor país (Finland) vs Pior país (South Sudan)
melhor_pais <- happiness %>% filter(Overall.rank == 1)
pior_pais <- happiness %>% filter(Overall.rank == max(happiness$Overall.rank))

comparacao <- bind_rows(
  melhor_pais %>% mutate(Tipo = "Melhor País"),
  pior_pais %>% mutate(Tipo = "Pior País")
)

p15 <- ggplot(comparacao, aes(x = Country.or.region, y = Perceptions.of.corruption, fill = Tipo)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Perceptions.of.corruption, 2)), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "COMPARAÇÃO DE PERCEPÇÃO DE CORRUPÇÃO",
    subtitle = "Finland (melhor) vs South Sudan (pior) - Menor valor = Menos corrupção",
    x = "País",
    y = "Percepção de Corrupção",
    fill = "Categoria",
    caption = "Fonte: World Happiness Report 2019"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("Melhor País" = "darkgreen", "Pior País" = "darkred"))

print(p15)
cat("Gráfico 15 criado: Comparação melhor vs pior país\n\n")

# ============================================================================
# RESUMO ESTATÍSTICO
# ============================================================================
cat("=== RESUMO ESTATÍSTICO ===\n")

cat("TOP 3 PAÍSES:\n")
print(top_3[, c("Country.or.region", "Overall.rank", "Score")])

cat("\nBOTTOM 3 PAÍSES:\n")
print(bottom_3[, c("Country.or.region", "Overall.rank", "Score")])

cat("\n=== ANÁLISE FINALIZADA ===\n")
cat("15 gráficos de barras criados para análise dos dados de felicidade.\n")
cat("Versão resumida com apenas os 3 melhores e 3 piores países.\n")