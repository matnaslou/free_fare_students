library(geobr)
library(h3r)
library(aopdata)
library(accessibility)
library(dplyr)
library(sf)
library(r5r)
library(osmdata)
library(ggplot2)
library(h3jsr)

#setwd("G:/.shortcut-targets-by-id/13s-t-swy07Av0TJBI4pTBk7Dye0Gk70m/free fare students/")

options(java.parameters = "-Xmx8G")

# Defining the State for analysis
estado <- "SP"

# Population Estimation of Sao Paulo based on 2010's Brazilian Census
aop <- read_population(
  city = "spo",
  year = 2010,
  geometry = FALSE,
  showProgress = TRUE
)

# Obtaining Sao Paulo bounding box
#bbox_sp <- getbb("Região Metropolitana de São Paulo, Região Geográfica Intermediária de São Paulo, São Paulo, Região Sudeste, Brasil")
# bbox_sp <- getbb("Cidade de São Paulo, São Paulo")


# Criar um polígono com as coordenadas do bounding box
# poligono_sp <- st_as_sfc(st_bbox(c(
#   xmin = bbox_sp["x", "min"],
#   xmax = bbox_sp["x", "max"],
#   ymin = bbox_sp["y", "min"],
#   ymax = bbox_sp["y", "max"]
# ), crs = 4326))

# Creating id_hex resolution 9 based on each polygon
# sp <- polygon_to_cells(geometry = poligono_sp, res = 9, simple = TRUE)
# sp <- data.frame(sp)
# colnames(sp)[colnames(sp) == "X0"] <- "id"

sp <- read_grid(city="spo",showProgress = TRUE)
colnames(sp)[colnames(sp) == "id_hex"] <- "id"


# Removing geom column for exporting
sp <- st_drop_geometry(sp)
sp <- as.data.frame(sp)

# Creating Latitude and Longitude based on id_hex
sp <- sp %>% 
  mutate(lat = cellToLatLng(id)$lat) %>%
  mutate(lon = cellToLatLng(id)$lng)

#sp <- sp %>% 
#  mutate(lat = cellToLatLng(id)$lat) %>%
#  mutate(lon = cellToLatLng(id)$lng)

# NOT NEEDED RIGHT NOW WITH aopdata
# escolas <- read_schools(year=2020,showProgress = TRUE)
# escolas <- escolas[(escolas$abbrev_state %in% estado),]
# 
# df <- sfheaders::sf_to_df(escolas)[c("x", "y")]
# df$CODESC <- escolas$code_school
# df$UF <- escolas$abbrev_state
# 
# df <- df[!(df$x %in% NaN),]
# #df <- df[!(df$y %in% NaN),]
# 
# df$resolution <- 9
# lat = c(df$y)
# lng = c(df$x)
# resolution = c(df$resolution,df$resolution)
# 
# df <- df[(df$UF %in% estado),]
# 
# df <- df %>% 
#   mutate(id = latLngToCell(lat, lng, resolution))
# 
# df <- df %>%
#   group_by(id) %>%
#   summarise(
#     #lat = mean(y, na.rm = TRUE),  # Calcula a média da latitude por id_hex
#     #lon = mean(x, na.rm = TRUE),  # Calcula a média da longitude por id_hex
#     contagem_unica = n_distinct(CODESC)   # Conta os valores únicos de CODESC por id_hex
#   )
# 
# df <- df %>% 
#   mutate(lat = cellToLatLng(id)$lat) %>%
#   mutate(lon = cellToLatLng(id)$lng)
# 
# df_final <- merge(x=sp,y=df, 
#                by=c("id","lat","lon"), all.x=TRUE)
# 
# df_final[is.na(df_final)] <- 0

# Renaming for merge
colnames(aop)[colnames(aop) == "id_hex"] <- "id"

# Merging aopdata info with schools
#mdf <- merge(df_final, aop, by = "id", all = TRUE)
mdf <- merge(sp, aop, by = "id", all = TRUE)

pasta <- system.file("extdata/spo", package = "r5r")
pasta

pasta <- "network_data"

fs::dir_tree(pasta)

conexao_r5r <- setup_r5(pasta, verbose = FALSE)
fs::dir_tree(pasta)

# Only execute if necessary: Calculation is very demanding
matriz <- travel_time_matrix(
  conexao_r5r,
  origins = sp,
  destinations = sp,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = as.POSIXct(
    "13-05-2014 14:00:00",                 # Pre-Treatment travelling
    format = "%d-%m-%Y %H:%M:%S"
  ),
  max_walk_time = 30,
  max_trip_duration = 120,
  verbose = FALSE,
  progress = TRUE
)
head(matriz)

# Exporting Travel Time Matrix calculations
write.csv(matriz, "matriz.csv", row.names = FALSE)

# Reading (if already calculated)
matriz <- read.csv("matriz.csv", header = TRUE, sep = ",")

data.table::setnames(matriz, "travel_time_p50", "travel_time") # If already calculated, not needed
oportunidades_cumulativas <- cumulative_cutoff(
  travel_matrix = matriz,
  land_use_data = mdf,
  opportunity = "P001",
  travel_cost = "travel_time",
  cutoff = 120,
  active = FALSE
)
head(oportunidades_cumulativas)

# Adding polygon to data
oportunidades_cumulativas$geom <- cell_to_polygon(input = oportunidades_cumulativas$id, simple = TRUE)
base_sf <- st_as_sf(oportunidades_cumulativas, crs = 4674) 
base_h3_sf <- st_transform(base_sf, crs = 4674)

# Data for graph
muns <- read_municipality(code_muni = "all")
rmsp <- muns[muns$code_muni%in%c(3503901,3505708,3506607,
                                 3509007,3509205,3510609,3513009,
                                 3513801,3515004,3515103,3515707,3516309,
                                 3516408,3518305,3518800,3522208,
                                 3522505,3523107,3525003,3526209,
                                 3528502,3529401,3530607,3534401,
                                 3539103,3539806,3543303,3544103,
                                 3545001,3546801,3547304,3547809,
                                 3548708,3548807,3549953,3550308,
                                 3552502,3552809,3556453),]
sao <- muns[muns$code_muni%in%c(3550308),]

# Merging accessibility data with Sao Paulo limits
h3_dentro_multipolygon <- st_intersection(base_h3_sf, sao)

# Replacing NA for 0, so it shows also on graph
h3_dentro_multipolygon <- h3_dentro_multipolygon %>%
  mutate(P001 = ifelse(is.na(P001), 0, P001))


# Visualization
ggplot() +
  geom_sf(data = h3_dentro_multipolygon, aes(fill = P001), color = NA) +  # Remove as bordas dos hexágonos
  geom_sf(data = rmsp, fill = NA, color = "white") +  # Multipolygon em branco
  scale_fill_viridis_c(option = "inferno") +
  labs(
    fill = "Estimativa",
    title = "Quantas Pessoas Acessam em até 120min este hex?"  # Define o título do gráfico
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove o quadriculado principal
    panel.grid.minor = element_blank(),  # Remove o quadriculado menor
    axis.text = element_blank(),         # Remove os valores dos eixos
    axis.title = element_blank(),        # Remove os títulos dos eixos
    axis.ticks = element_blank(),        # Remove as marcações dos eixos
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
  )

# Removing geom column for exporting
df_sem_geom <- st_drop_geometry(oportunidades_cumulativas)

# Exporting Results
write.csv(df_sem_geom, "Dados/accessibility.csv", row.names = FALSE)