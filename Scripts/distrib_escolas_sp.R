library(aopdata)
library(sf)
library(tidyverse)
library(h3jsr)

d <- fread("Dados/Dados Tratados/base_final_sp_todas.csv")
grid <- read_grid(city = "São Paulo")

# Using rename()
grid <- grid %>% 
  rename("id" = "id_hex")

# Filtering School
sch <- d %>%
  filter(Ano == 2014)

# Combining data
da <- grid %>%
  left_join(sch, by = "id")

da$resolution <- 8
da <- da %>% 
    mutate(lat = cellToLatLng(id)$lat) %>%
    mutate(lon = cellToLatLng(id)$lng)


# lat = c(df$y)
# lng = c(df$x)
# resolution = c(df$resolution,df$resolution)

da <- da %>% 
     mutate(id_8 = latLngToCell(lat, lon, resolution))

# Remover a coluna `geometry` da base contagem_h3
da <- da %>%
  st_drop_geometry()

# Converter o ID H3 em polígonos
da <- da %>%
  mutate(geometry = cell_to_polygon(id_8)) %>%
  st_as_sf()  # Converter para objeto sf  

# Contar o número único de CODESC por ID H3
contagem_h3 <- da %>%
  group_by(id_8) %>%
  summarize(
    n_escolas = if (all(is.na(CODESC))) 0 else n_distinct(CODESC, na.rm = TRUE),
    .groups = "drop"
  )

# Remover a coluna `geometry` da base contagem_h3
contagem_h3 <- contagem_h3 %>%
  st_drop_geometry()

# Combining data
f <- da %>%
  left_join(contagem_h3, by = "id_8")

# Plotar o mapa
ggplot(data = f) +
  geom_sf(aes(fill = n_escolas), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Number of Schools per h3") +
  theme_void() 

# Plot com resolução 1920x1080
ggsave(
  filename = paste0("Resultados/Gráficos/distrib_escolas_sp.png"),  # Caminho e nome do arquivo
  plot = last_plot(),                       # Último gráfico gerado
  width = 1920,                             # Largura em pixels
  height = 1080,                            # Altura em pixels
  dpi = 300,                                # Resolução em DPI
  units = "px",                             # Unidades em pixels
  device = "png"                            # Formato da imagem (png, jpeg, etc.)
)
