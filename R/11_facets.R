# 11 - trimestres - facets ----
# URL: https://github.com/ecodiegoale/Dataviz/blob/ggplot2/trimestres_facets.R
# Preparacion ========================================================

rm(list = ls())
library(siebanxicor)
library(tidyverse)
library(scales)
library(ggtext)

library(extrafont)
font_import()
y
loadfonts(device = "win")

#Si no tienes experiencia en el uso de la API de Banco de México te recomiendo visitar mi blog personal
#donde lo explico paso a paso con ejemplos
#https://www.rpubs.com/ecodiegoale/api_banxico

## Token para usar siebanxicor ======================================
token <- "TU TOKEN"

setToken(token)

## Tema para el gráfico =============================================
theme_da <- theme(panel.background = element_blank(),
                  text = element_text(family="Dubai"),
                  panel.border = element_blank(),
                  legend.position = "none",
                  axis.line = element_line(
                    colour = "black", 
                    linewidth= 0.8),
                  axis.ticks = element_blank(),
                  axis.text.y = element_text(
                    colour = "black",
                    size = 10),
                  axis.text.x = element_text(
                    colour = "black",
                    size = 10),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  legend.key = element_blank(), #quita el fondo gris de las leyendas
                  panel.grid.major.y =element_line(
                    color = '#D7D6D6',
                    linewidth = 0.8),
                  panel.grid.minor.y = element_line(
                    color = "#D7D6D6",
                    linewidth = 0.1,
                    linetype = 'dashed'),
                  panel.grid.major.x = element_blank(),
                  plot.title = element_text(
                    size = 14, hjust = 0.5,
                    face = "bold", colour = "black"),
                  plot.caption = element_text(
                    size = 8, hjust = 0.5,
                    colour = "black"),
                  plot.subtitle = ggtext::element_markdown(
                    size = 12, hjust = 0.5,
                    face = "bold", colour = "black"))

# Datos del PIB =========================================================

metadata <- getSeriesMetadata("SR17622")
hoy <- Sys.Date() #mas reciente

pib <- getSeriesData("SR17622", '2000-01-01', hoy)
pib <- getSerieDataFrame(pib, "SR17622")

## Tibble ===============================================================

pib <- pib %>%
  filter(date > "2018-10-01")
pib$year<- year(pib$date)
pib$trim<- quarter(pib$date)

pib <- pib %>%
  mutate(
    trim = as.character(trim),
    trim = case_when(
      trim == "1" ~ "I",
      trim == "2" ~ "II",
      trim == "3" ~ "III",
      trim == "4" ~ "IV"
    ),
    value = value/1000
  )

pib_plot <- ggplot(data = pib, aes(x=trim, y=value))+
  geom_bar(stat="identity",color ="#474F7A", fill="#474F7A")+
  scale_y_continuous(label=comma)+
  facet_wrap(~ year, nrow = 1,
             strip.position = "bottom")+
  theme_da+
  facet_wrap(~ year, nrow = 1,
             strip.position = "bottom")+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing.x = unit(0, "cm"),
        strip.text = element_text(colour = "black",
                                  size = 10))+
  labs(title = "PIB trimestral a precios constantes, base 2018",
       subtitle =  "Miles de millones de pesos",
       caption = "API, Banco de México | @ecodiegoale")
pib_plot

library(ggh4x)
pib_plot2 <- ggplot(data = pib)+
  geom_bar(aes(x=interaction(trim, year), 
               y=value, group = 1),
           stat="identity",color ="#474F7A", fill="#474F7A")+
  scale_y_continuous(label=comma)+
  scale_x_discrete(NULL, guide = "axis_nested") +
  theme_da+
  labs(title = "PIB trimestral a precios constantes, base 2018",
       subtitle =  "Miles de millones de pesos",
       caption = "API, Banco de México | @ecodiegoale")
pib_plot2

ggsave("pib.png", plot = pib_plot2, 
       width = 20, height = 10, units = "cm")
