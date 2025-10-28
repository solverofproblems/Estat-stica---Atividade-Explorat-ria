# ============================================================================
# ANÁLISE DE PROBABILIDADE DE CHUVA
# Baseada em 3 parâmetros meteorológicos principais
# ============================================================================

# Carregar bibliotecas necessárias
library(tidyverse)
library(ggplot2)
library(dplyr)

cat("=== ANÁLISE DE PROBABILIDADE DE CHUVA ===\n")
cat("Parâmetros analisados:\n")
cat("1. Humidity3pm (Umidade relativa às 15h)\n")
cat("2. WindGustSpeed (Velocidade da rajada de vento)\n")
cat("3. Rainfall (Quantidade de chuva no dia atual)\n\n")

# ============================================================================
# CARREGAMENTO E LIMPEZA DOS DADOS
# ============================================================================
cat("Carregando dados meteorológicos...\n")

# Carregar dados
weather <- read.csv("probabilidade/weatherAUS.csv", stringsAsFactors = FALSE)

# Limpeza dos dados - remover valores ausentes
weather_clean <- weather %>%
  filter(!is.na(Humidity3pm) & !is.na(WindGustSpeed) & !is.na(Rainfall) & 
         !is.na(RainTomorrow) & Humidity3pm >= 0 & WindGustSpeed >= 0 & Rainfall >= 0)

cat("Dados carregados:", nrow(weather_clean), "registros válidos\n")
cat("Total de registros originais:", nrow(weather), "\n\n")

# ============================================================================
# ANÁLISE ESTATÍSTICA GERAL
# ============================================================================
cat("=== ESTATÍSTICAS GERAIS ===\n")

# Probabilidade geral de chover
prob_geral <- mean(weather_clean$RainTomorrow == "Yes") * 100
cat("Probabilidade geral de chover:", round(prob_geral, 2), "%\n")

# Estatísticas dos parâmetros
cat("\nEstatísticas dos parâmetros:\n")
cat("Humidity3pm - Média:", round(mean(weather_clean$Humidity3pm), 2), "%\n")
cat("WindGustSpeed - Média:", round(mean(weather_clean$WindGustSpeed), 2), "km/h\n")
cat("Rainfall - Média:", round(mean(weather_clean$Rainfall), 2), "mm\n\n")

# ============================================================================
# ANÁLISE DOS FATORES INDIVIDUAIS (Para referência)
# ============================================================================
cat("=== ANÁLISE DOS FATORES INDIVIDUAIS ===\n")

# Análise rápida dos fatores individuais
cat("Fator 1 - Umidade às 15h:\n")
cat("- Baixa (<30%):", round(mean(weather_clean$RainTomorrow[weather_clean$Humidity3pm < 30] == "Yes") * 100, 1), "%\n")
cat("- Alta (≥70%):", round(mean(weather_clean$RainTomorrow[weather_clean$Humidity3pm >= 70] == "Yes") * 100, 1), "%\n\n")

cat("Fator 2 - Velocidade do Vento:\n")
cat("- Calmo (<20 km/h):", round(mean(weather_clean$RainTomorrow[weather_clean$WindGustSpeed < 20] == "Yes") * 100, 1), "%\n")
cat("- Forte (≥60 km/h):", round(mean(weather_clean$RainTomorrow[weather_clean$WindGustSpeed >= 60] == "Yes") * 100, 1), "%\n\n")

cat("Fator 3 - Chuva Atual:\n")
cat("- Sem chuva (0 mm):", round(mean(weather_clean$RainTomorrow[weather_clean$Rainfall == 0] == "Yes") * 100, 1), "%\n")
cat("- Com chuva (>0 mm):", round(mean(weather_clean$RainTomorrow[weather_clean$Rainfall > 0] == "Yes") * 100, 1), "%\n\n")

# ============================================================================
# ANÁLISE REAL: UMIDADE vs VENTO E PROBABILIDADE DE CHUVA
# ============================================================================
cat("=== ANÁLISE REAL: UMIDADE vs VENTO ===\n")

# Criar categorias baseadas nos dados reais
weather_clean$Umidade_Cat <- case_when(
  weather_clean$Humidity3pm < 30 ~ "Baixa (<30%)",
  weather_clean$Humidity3pm < 50 ~ "Moderada (30-50%)",
  weather_clean$Humidity3pm < 70 ~ "Alta (50-70%)",
  TRUE ~ "Muito Alta (≥70%)"
)

weather_clean$Vento_Cat <- case_when(
  weather_clean$WindGustSpeed < 20 ~ "Calmo (<20 km/h)",
  weather_clean$WindGustSpeed < 40 ~ "Moderado (20-40 km/h)",
  weather_clean$WindGustSpeed < 60 ~ "Forte (40-60 km/h)",
  TRUE ~ "Muito Forte (≥60 km/h)"
)

# Calcular probabilidade real por combinação de umidade e vento
prob_combinada <- weather_clean %>%
  group_by(Umidade_Cat, Vento_Cat) %>%
  summarise(
    Probabilidade_Real = mean(RainTomorrow == "Yes") * 100,
    Total_Registros = n(),
    .groups = 'drop'
  ) %>%
  filter(Total_Registros >= 50) %>%  # Filtrar combinações com poucos dados
  arrange(desc(Probabilidade_Real))

print(prob_combinada)

# Gráfico: Top 8 combinações Umidade x Vento com maior probabilidade
prob_top8 <- prob_combinada %>%
  arrange(desc(Probabilidade_Real)) %>%
  head(8)

p_combinado <- ggplot(prob_top8, aes(x = reorder(paste(Umidade_Cat, "x", Vento_Cat), Probabilidade_Real), 
                                    y = Probabilidade_Real)) +
  geom_col(fill = "darkblue", alpha = 0.8) +
  geom_text(aes(label = paste(round(Probabilidade_Real, 1), "%")), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "TOP 8 COMBINAÇÕES UMIDADE x VENTO COM MAIOR PROBABILIDADE DE CHUVA",
    subtitle = "Combinações que mais indicam chance de chover",
    x = "Combinação: Umidade x Vento",
    y = "Probabilidade Real de Chuva (%)",
    caption = "Fonte: WeatherAUS Dataset | Top 8 combinações com maior probabilidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkblue"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

print(p_combinado)
cat("Gráfico criado: Top 8 combinações Umidade x Vento\n\n")

# ============================================================================
# ANÁLISE DETALHADA DOS FATORES
# ============================================================================
cat("=== ANÁLISE DETALHADA DOS FATORES ===\n")

# Análise individual de umidade
cat("ANÁLISE DE UMIDADE:\n")
umidade_analise <- weather_clean %>%
  group_by(Umidade_Cat) %>%
  summarise(
    Probabilidade_Chuva = mean(RainTomorrow == "Yes") * 100,
    Total_Registros = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Probabilidade_Chuva))

print(umidade_analise)

# Análise individual de vento
cat("\nANÁLISE DE VENTO:\n")
vento_analise <- weather_clean %>%
  group_by(Vento_Cat) %>%
  summarise(
    Probabilidade_Chuva = mean(RainTomorrow == "Yes") * 100,
    Total_Registros = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Probabilidade_Chuva))

print(vento_analise)

# Gráfico de umidade
p_umidade <- ggplot(umidade_analise, aes(x = reorder(Umidade_Cat, Probabilidade_Chuva), 
                                         y = Probabilidade_Chuva)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste(round(Probabilidade_Chuva, 1), "%")), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PROBABILIDADE DE CHUVA POR UMIDADE",
    subtitle = "Quanto maior a umidade, maior a chance de chover",
    x = "Categoria de Umidade",
    y = "Probabilidade de Chuva (%)",
    caption = "Fonte: WeatherAUS Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "steelblue"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_umidade)

# Gráfico de vento
p_vento <- ggplot(vento_analise, aes(x = reorder(Vento_Cat, Probabilidade_Chuva), 
                                    y = Probabilidade_Chuva)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste(round(Probabilidade_Chuva, 1), "%")), 
            vjust = -0.5, color = "white", fontface = "bold", size = 4) +
  labs(
    title = "PROBABILIDADE DE CHUVA POR VELOCIDADE DO VENTO",
    subtitle = "Rajadas mais intensas aumentam a chance de chover",
    x = "Categoria de Velocidade do Vento",
    y = "Probabilidade de Chuva (%)",
    caption = "Fonte: WeatherAUS Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_vento)
cat("Gráficos individuais criados: Umidade e Vento\n\n")

# ============================================================================
# RESUMO FINAL
# ============================================================================
cat("=== RESUMO FINAL ===\n")
cat("Probabilidade geral de chuva:", round(prob_geral, 2), "%\n")
cat("Análise baseada em dados reais de umidade e vento\n")
cat("Gráficos criados:\n")
cat("1. Combinação Umidade x Vento\n")
cat("2. Análise individual de Umidade\n")
cat("3. Análise individual de Vento\n\n")

cat("Análise estatística real concluída!\n")
cat("Percentuais baseados em dados históricos reais.\n")
