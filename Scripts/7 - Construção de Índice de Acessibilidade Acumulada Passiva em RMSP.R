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

options(java.parameters = "-Xmx6G")


# Obter a bounding box de São Paulo
#bbox_sp <- getbb("São Paulo, São Paulo, Brazil")
bbox_sp <- getbb("Região Metropolitana de São Paulo, Região Geográfica Intermediária de São Paulo, São Paulo, Região Sudeste, Brasil")
# Criar um polígono com as coordenadas do bounding box
poligono_sp <- st_as_sfc(st_bbox(c(
  xmin = bbox_sp["x", "min"],
  xmax = bbox_sp["x", "max"],
  ymin = bbox_sp["y", "min"],
  ymax = bbox_sp["y", "max"]
), crs = 4326))

sp <- polygon_to_cells(geometry = poligono_sp, res = 9, simple = TRUE)
sp <- data.frame(sp)
colnames(sp)[colnames(sp) == "X0"] <- "id"

sp <- sp %>% 
  mutate(lat = cellToLatLng(id)$lat) %>%
  mutate(lon = cellToLatLng(id)$lng)

sp <- sp %>% 
  mutate(lat = cellToLatLng(id)$lat) %>%
  mutate(lon = cellToLatLng(id)$lng)


escolas <- read_schools(year=2020,showProgress = TRUE)
escolas <- escolas[(escolas$abbrev_state %in% "SP"),]

df <- sfheaders::sf_to_df(escolas)[c("x", "y")]
df$CODESC <- escolas$code_school
df$UF <- escolas$abbrev_state

df <- df[!(df$x %in% NaN),]
#df <- df[!(df$y %in% NaN),]

df$resolution <- 9
lat = c(df$y)
lng = c(df$x)
resolution = c(df$resolution,df$resolution)

df <- df %>% 
  mutate(id = latLngToCell(lat, lng, resolution))

df <- df[(df$UF %in% "SP"),]

df <- df %>%
  group_by(id) %>%
  summarise(
    #lat = mean(y, na.rm = TRUE),  # Calcula a média da latitude por id_hex
    #lon = mean(x, na.rm = TRUE),  # Calcula a média da longitude por id_hex
    contagem_unica = n_distinct(CODESC)   # Conta os valores únicos de CODESC por id_hex
  )

df <- df %>% 
  mutate(lat = cellToLatLng(id)$lat) %>%
  mutate(lon = cellToLatLng(id)$lng)

df_final <- merge(x=sp,y=df, 
               by=c("id","lat","lon"), all.x=TRUE)

df_final[is.na(df_final)] <- 0

pasta <- system.file("extdata/spo", package = "r5r")
pasta

pasta <- "C:/Users/rosan/Downloads/Nova Pasta"

fs::dir_tree(pasta)

conexao_r5r <- setup_r5(pasta, verbose = FALSE)
fs::dir_tree(pasta)



matriz <- travel_time_matrix(
  conexao_r5r,
  origins = sp,
  destinations = sp,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = as.POSIXct(
    "13-05-2019 14:00:00",
    format = "%d-%m-%Y %H:%M:%S"
  ),
  max_walk_time = 30,
  max_trip_duration = 120,
  verbose = FALSE,
  progress = FALSE
)
head(matriz)

matriz <- read.csv("C:/Users/rosan/Downloads/matriz.csv", header = TRUE, sep = ",")

#data.table::setnames(matriz, "travel_time_p50", "travel_time")
oportunidades_cumulativas <- cumulative_cutoff(
  travel_matrix = matriz,
  land_use_data = df_final,
  opportunity = "contagem_unica",
  travel_cost = "travel_time",
  cutoff = 120,
  active = TRUE
)
head(oportunidades_cumulativas)

#dataset_final = left_join(sp, oportunidades_cumulativas, by=c("id"))
oportunidades_cumulativas$geom <- cell_to_polygon(input = oportunidades_cumulativas$id, simple = TRUE)
#dataset_final <- na.omit(dataset_final)
#grade_gua <- aopdata::read_grid(city = "Guarulhos")
#grade_sp <- aopdata::read_grid(city = "São Paulo")
#grade <-bind_rows(grade_sp, grade_gua)
base_sf <- st_as_sf(oportunidades_cumulativas, crs = 4674) 
base_h3_sf <- st_transform(base_sf, crs = 4674)


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
#rmsp$resolution <- 9
#rmsp$id <- polygonToCells(rmsp$geom, resolution = rmsp$resolution)
#colnames(grade)[colnames(grade) == "id_hex"] <- "id"
#dataset_final = left_join(grade, oportunidades_cumulativas, by=c("id"))

h3_dentro_multipolygon <- st_intersection(base_h3_sf, rmsp)

ggplot() +
  geom_sf(data = h3_dentro_multipolygon, aes(fill = contagem_unica), color = NA) +  # Remover as bordas dos hexágonos
  geom_sf(data = rmsp, fill = NA, color = "white") +  # Multipolygon em cinza
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Quantas Oportunidade Acessíveis em até 120min") +
  theme_minimal()


#ggplot(dataset_final) +
  #geom_sf(aes(geometry = geom)) +
#  geom_sf(aes(geometry = geom, fill = contagem_unica),size=0) +
#  scale_fill_viridis_c(option = "inferno") +
#  labs(fill = "Quão acessível é cada hexágono") +
#  theme_minimal()