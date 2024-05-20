library(readr)
banco <- read_csv("banco_final.csv")
View(banco)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forcats)
library(dplyr)
library(scales)

#Padronização (cores e tema)----
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

estat_theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966",
                 "#999966", "#006606", "#008091", "#041835", "#666666")


theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

#1) Número de lançamentos a cada década por formato de lançamento;----

#gráfico de linhas

teste <- data.frame(banco$date_aired)
teste$ano <- year(teste$banco.date_aired)
teste$decada <- (teste$ano %/% 10)* 10
teste$formato <- (banco$format)
view(teste)
contados <- teste %>% 
  group_by(decada, formato) %>% 
  summarise(quantidades = n())
contados$formato[contados$formato == "Movie"] <-  "Filme"
contados$formato[contados$formato == "Serie"] <-  "Série"
view(contados)

ggplot(contados) +
  aes(x = decada, y = quantidades, group = formato, colour = formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Número de Lançamentos") +
  estat_theme()
ggsave("Linhas01.pdf", width = 158, height = 93, units = "mm")

#Introdução 
#Título
#Década(1960, 1970, 1980, 1990...)
#Melhorar a Análise Descritiva
#Função "\ref"


#2) Variação da nota IMDB por temporada dos episódios;----

temporadas_1234 <- banco %>% 
  filter(season %in% c("1", "2", "3", "4"))
view(temporadas_1234)

ggplot(temporadas_1234) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporada", y = "Nota IMDB") +
  estat_theme()
ggsave("BoxPlot01.pdf", width = 158, height = 93, units = "mm")

#Introdução da análise
#Quadro de medidas (6.2.1) TABELA
#Títulos Gráfico/Análise
#Rever análise(Menos Pessoal, Menos 1ºpessoa, )
#

#não entendi muito bem oque a função "\ref" faz, então deve continuar errada

#3) Top 3 terrenos mais frequentes pela ativação da armadilha;----

#Tradução----
banco$setting_terrain[banco$setting_terrain == "Air"] <- "Aereo"
banco$setting_terrain[banco$setting_terrain == "Cave"] <- "Cavernoso"
banco$setting_terrain[banco$setting_terrain == "Coast"] <- "Costeiro"
banco$setting_terrain[banco$setting_terrain == "Desert"] <- "Desertico"
banco$setting_terrain[banco$setting_terrain == "Forest"] <- "Florestal"
banco$setting_terrain[banco$setting_terrain == "Island"] <- "Insular"
banco$setting_terrain[banco$setting_terrain == "Jungle"] <- "Selvagem"
banco$setting_terrain[banco$setting_terrain == "Moon"] <- "Lunar"
banco$setting_terrain[banco$setting_terrain == "Mountain"] <- "Montanhoso"
banco$setting_terrain[banco$setting_terrain == "Ocean"] <- "Oceanico"
banco$setting_terrain[banco$setting_terrain == "Rural"] <- "Rural"
banco$setting_terrain[banco$setting_terrain == "Snow"] <- "Neve"
banco$setting_terrain[banco$setting_terrain == "Space"] <- "Espacial"
banco$setting_terrain[banco$setting_terrain == "Swamp"] <- "Pantanoso"
banco$setting_terrain[banco$setting_terrain == "Urban"] <- "Urbano"

#Gráfico de Ativação das Armadilhas
ativacao <- banco %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ativacao_urbano <- ativacao[ativacao$setting_terrain == "Urbano", ]
ativacao_rural <- ativacao[ativacao$setting_terrain == "Rural", ]
ativacao_florestal <- ativacao[ativacao$setting_terrain == "Florestal", ]

ativacao_top3 <- rbind(ativacao_urbano, ativacao_rural, ativacao_florestal)
ativacao_top3 <- ativacao_top3[complete.cases(ativacao_top3$trap_work_first),]
ativacao_top3$trap_work_first[ativacao_top3$trap_work_first == "TRUE"] <- "Verdadeiro"
ativacao_top3$trap_work_first[ativacao_top3$trap_work_first == "FALSE"] <- "Falso"

porcentagens <- str_c(ativacao_top3$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(ativacao_top3$freq, " (", porcentagens, ")"))

ggplot(ativacao_top3) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = trap_work_first, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência", fill = "Capturado de Primeira") +
  theme_estat()
ggsave("Colunas_Bi01.pdf", width = 158, height = 93, units = "mm")

#Não consegui entender qual o erro com a porcentagem nos gráficos, deixei assim mesmo.

#4) Relação entre as notas IMDB e engajamento:----
ggplot(banco) +
  aes(x = imdb, y = engagement) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
  labs(
    x = "Nota IMDB",
    y = "Engajamento"
  ) +
  theme_estat()
ggsave("Dispersão01.pdf", width = 158, height = 93, units = "mm")

#Coeficiente de Correlação de Pearson
x <- c(banco$imdb)
y <- c(banco$engagement)

cf_pearson <- cor(x, y)
print(cf_pearson)

#Não tinha padronização em relação ao "nível de transparência do alpha, tem um valor ideal?
