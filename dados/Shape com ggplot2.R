library(ggplot2)
library(sf)
library(dplyr)

# ---- Load sources
source("dados/src/rosadosventos.R")

# ----  Load shapefile
shapename <- read_sf('dados/shape_estado_pb/Municipios.shp')

# ---- Carregar dados
dados <- read.csv2("dados/exemplo_de_base.csv")

# ---- Criando dataframe com a ordem correta do shapefile
codigoshape <- data.frame(GEOCODIG_M = shapename$GEOCODIG_M)

# ---- Unindo o shapefile com os dados
dados_mapa <- shapename |> 
  left_join(dados, by = 'GEOCODIG_M')

# ---- Selecionar coluna de variáveis para gerar mapa
casos <- dados_mapa$casos
pop <- dados_mapa$POP2017
dadosT <- data.frame(Population = pop, CasosPop = casos / pop)
dadosT <- dadosT |> 
  mutate(
    RIE = CasosPop / (sum(casos) / sum(pop)) # Calcula o RIE
  )

# ---- Criar intervalo de classes
classes <- c(-Inf, 0.00000000001, 0.49999999999, 0.99999999999, 1.49999999999, 1.99999999999, Inf)
# ---- Criar legenda
legenda = c('0','0,0--0,5','0,5|--1,0','1,0|--1,5','1,5|--2,0','2,0 ou +')
cores <- c("#ffffff", "#f1f1d1", "#e9e57f", "#cda300", "#a46500", "#593216")

# ---- Adicionando a variável RIE ao shapefile
dados_mapa <- dados_mapa |> 
  mutate(
    RIE = dadosT$RIE, # Adiciona o cálculo ao shapefile
    classe_var = cut(RIE, breaks = classes, include.lowest = TRUE) # Cria classes
  )


# ---- Plot com ggplot2
mapa <- ggplot(data = dados_mapa) +
  geom_sf(aes(fill = classe_var), color = "black") + # Preenchimento e bordas
  scale_fill_manual(values = cores, labels = legenda) + # Aplica cores e legenda
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Risco Relativo" # Nome da legenda
  ) +
  annotation_custom(
    grob = compassRoseGrob(x = 0.9, y = 0.9, rot = 0, cex = 2, scale = 0.5),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )+
  theme_minimal() +
  theme(
    #axis.text = element_blank(), # Remove textos dos eixos
    panel.grid = element_blank(), # Remove grade
    panel.border = element_rect(color = "black", fill = NA) # Adiciona borda
  )

mapa

# Adicionando a barra de escala
mapa + annotation_custom(
  grob = scalebar(loc = c(-38.5, -8.7), shape = "pb", mapa = mapa),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)
