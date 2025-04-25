library(ggplot2)
library(sf)
library(readr)
library(geobr)
library(dplyr)
library(lubridate)
library(png)
library(grid)
library(gridExtra)
library(magick)

## Banco
dados <- read_csv2("Tabelas/primeira_inv_pro MODIFI.csv")

estados <- read_state(code_state = "all", year = 2020)
dados_mapa <- merge(estados, dados, by = "abbrev_state")

names(dados_mapa)
names(estados)

#Mapa


# Definir a ordem específica para 'ano_mes' como fator
ordem_ano_mes <- c("Feb/14", "Apr/14", "jun/14", "Aug/14", 
                   "Sep/14", "Oct/14", "nov/14", "jan/15", "Feb/15")

# Converter 'ano_mes' para um fator com a ordem específica
dados_mapa$ano_mes <- factor(dados_mapa$ano_mes, levels = ordem_ano_mes)

# Ajustar a capitalização dos níveis de 'ano_mes'
levels(dados_mapa$ano_mes) <- sapply(levels(dados_mapa$ano_mes), function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
})

# Criação do mapa com a escala de cores viridis em degradê invertido
meu_mapa <- ggplot(data = dados_mapa) +
  geom_sf(aes(fill = ano_mes), color = "grey") +
  geom_sf_text(aes(label = abbrev_state), size = 3, color =   "black") +
  scale_fill_viridis_d(name = "Month/Year", direction = -1, guide = guide_legend(reverse = FALSE)) +  # Inverte a escala de cores
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",  # Posiciona a legenda na direita
        legend.direction = "horizontal") +  # Organiza a legenda verticalmente
  labs(fill = "Month/Year")

# Exibir o mapa
print(meu_mapa)

ggsave("Figuras/Timeline+mapa/mapa_brasil_timeline.png", plot = meu_mapa, width = 10, height = 8, dpi = 300)

#######################################################
### Mapa das 5 regiões ############################
#####################################################

dados_mapa <- dados_mapa %>%
  mutate(name_region_english = case_when(
    name_region == "Norte" ~ "North",
    name_region == "Nordeste" ~ "Northeast",
    name_region == "Centro Oeste" ~ "Central-East",
    name_region == "Sudeste" ~ "Southeast",
    name_region == "Sul" ~ "South",
    TRUE ~ name_region  # Mantém o nome original caso não seja uma das opções listadas
  ))


# Criar o mapa
meu_mapar <- ggplot(data = dados_mapa) +
  geom_sf(aes(fill = factor(name_region_english, levels = c("North", "Northeast", "Central-East", "Southeast", "South"))),
          color = "grey") + # Preencher as regiões com cores e contornos cinza
  geom_sf_text(aes(label = abbrev_state), size = 3, color = "black") + # Adicionar as siglas dos estados
  scale_fill_manual(values = c("darksalmon", "darkseagreen", "cornflowerblue", "darkgoldenrod1", "mediumpurple1"),
                    labels = c("North", "Northeast", "Central-East", "Southeast", "South")) +
  labs(title = , 
       fill = "Brazilian regions") +
  theme_minimal() + # Um tema minimalista
  theme(axis.title = element_blank(), # Remove os títulos dos eixos
        axis.text = element_blank(), # Remove os textos dos eixos
        axis.ticks = element_blank(), # Remove os ticks dos eixos
        panel.grid = element_blank(), # Remove a grade do fundo
        legend.position = "right") # Posiciona a legenda à direita


# Exibir o mapa
print(meu_mapar)

ggsave("Figuras/Timeline+mapa/mapa_brasil_regions.png", plot = meu_mapar, width = 10, height = 8, dpi = 300)




##################################################
######### Fazendo uma imagem timeline mapa ########
###################################################

library(png)
library(grid)
library(gridExtra)
library(ggplot2)
library(gtable)

# Carregar a imagem PNG (substitua pelo caminho correto do arquivo)
imagem <- readPNG("Figuras/Timeline+mapa/timeline_CHIKV.png")

# Converter a imagem para um grob para uso no grid.arrange
imagem_grob <- rasterGrob(imagem, interpolate = TRUE)

# Ajustar os mapas
# Supondo que `meu_mapa` e `meu_mapar` sejam ggplot objects já criados anteriormente
mapa_final <- meu_mapa + theme(legend.position = "none")  # Retirar a legenda

# Combinar a imagem e o mapa com a imagem à esquerda e o mapa à direita
# Ajustar o layout para duas colunas
plot_combinado <- grid.arrange(mapa_final, imagem_grob, ncol = 2)  # meu_mapar com legenda, imagem à direita

# Salvar o arquivo com as dimensões corretas
ggsave("Figuras/Timeline+mapa/mapa_brasil_timeline_final.png", plot = plot_combinado, width = 10.7, height = 8, dpi = 300)


