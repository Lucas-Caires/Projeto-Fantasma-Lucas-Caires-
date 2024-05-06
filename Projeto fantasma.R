library(readr)
banco <- read_csv("banco_final.csv")
View(banco)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forcats)
library(dplyr)

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

ggplot(temporadas_1234) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporada", y = "Nota") +
  estat_theme()
ggsave("BoxPlot01.pdf", width = 158, height = 93, units = "mm")

#não entendi muito bem oque a função "\ref" faz, então deve continuar errada

#3) Top 3 terrenos mais frequentes pela ativação da armadilha;----


