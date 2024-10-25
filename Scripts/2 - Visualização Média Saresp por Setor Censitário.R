library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

setwd("C:/Users/rosan/Dropbox/Matheus/Projeto SARESP")
# Códigos com # foram tentativas, não necessárias para rodar o código em si,
# mas não queria perder essas funções e comandos

# Mapa Microrregiões SP
micro_sp <- read_micro_region(code_micro="SP", year=2014)

# Mapa de Setor Censitário:
c <- read_census_tract(code_tract="SP", year=2017)

# Dados CEP para Lat-Long
#cep_to_setor <- read_delim("Dados/Dados IBGE/3550308_SAO_PAULO.csv", 
#                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Dados SARESP Tratados
df_2014 <-  df_2014 <- read_csv("Dados/Dados Tratados/df_2014.csv", 
                                locale = locale(encoding = "ISO-8859-1"))

# Nota Média do SARESP por escola:
df_2014$saresp_med <- rowMeans(df_2014[,c(383,384,385)], na.rm=TRUE)

#cep_to_setor = subset(cep_to_setor, select = c(CEP,LATITUDE,LONGITUDE))
#colnames(cep_to_setor)[colnames(cep_to_setor) == "CEP"] ="CO_CEP"

# Group by mean using dplyr
#df_cep_medias <- cep_to_setor %>%
#  group_by(CO_CEP) %>%
#  summarize(
#    LATITUDE_MEDIA = mean(LATITUDE, na.rm = TRUE),
#    LONGITUDE_MEDIA = mean(LONGITUDE, na.rm = TRUE)
#  )


#df_2014 <- merge(df_2014, df_cep_medias, by = c("CO_CEP"))


# Coluna geométrica com Latitude Média e Longitude Média:
#df_2014 <- df_2014 %>% st_as_sf(coords = c("LONGITUDE_MEDIA", "LATITUDE_MEDIA"),
#                                remove = FALSE) %>%
#  st_set_crs(4674)


#df_2014 <- df_2014 %>%
#  mutate(
#    LATITUDE = LATITUDE / 1e8,
#    LONGITUDE = LONGITUDE / 1e8
#  ) %>%
#  st_as_sf(coords = c("LATITUDE", "LONGITUDE"), remove = FALSE) %>%
#  st_set_crs(4674)
#nchar_lat <- nchar(abs(df_2014$LATITUDE))
#nchar_long <- nchar(abs(df_2014$LONGITUDE))
# Visualizar um resumo da quantidade de dígitos
#table(nchar_lat)
#table(nchar_long)
#min(df_2014$LONGITUDE)

# Ajuste nas Colunas de Latitude e Longitude, que tem quantidade de dígitos distintos
df_2014 <- df_2014 %>%
  mutate(
    LATITUDE = case_when(
      nchar(abs(LATITUDE)) == 10 ~ LATITUDE / 1e8,  # Dividir por 10^7 para 10 dígitos
      nchar(abs(LATITUDE)) == 9  ~ LATITUDE / 1e7,  # Dividir por 10^7 para 9 dígitos
      nchar(abs(LATITUDE)) == 8  ~ LATITUDE / 1e6,  # Dividir por 10^7 para 8 dígitos
      nchar(abs(LATITUDE)) == 7  ~ LATITUDE / 1e5,  # Dividir por 10^5 para 7 dígitos
      nchar(abs(LATITUDE)) == 6  ~ LATITUDE / 1e4,  # Dividir por 10^4 para 6 dígitos
      nchar(abs(LATITUDE)) == 5  ~ LATITUDE / 1e3,  # Dividir por 10^3 para 5 dígitos
      TRUE ~ LATITUDE  # Deixar como está para outros casos
    ),
    LONGITUDE = case_when(
      nchar(abs(LONGITUDE)) == 10 ~ LONGITUDE / 1e8,  # Dividir por 10^7 para 10 dígitos
      nchar(abs(LONGITUDE)) == 9  ~ LONGITUDE / 1e7,  # Dividir por 10^7 para 9 dígitos
      nchar(abs(LONGITUDE)) == 8  ~ LONGITUDE / 1e6,  # Dividir por 10^6 para 8 dígitos
      nchar(abs(LONGITUDE)) == 7  ~ LONGITUDE / 1e5,  # Dividir por 10^5 para 7 dígitos
      nchar(abs(LONGITUDE)) == 6  ~ LONGITUDE / 1e4,  # Dividir por 10^4 para 6 dígitos
      nchar(abs(LONGITUDE)) == 5  ~ LONGITUDE / 1e3,  # Dividir por 10^3 para 5 dígitos
      nchar(abs(LONGITUDE)) == 4  ~ LONGITUDE / 1e2,  # Dividir por 10^3 para 4 dígitos
      nchar(abs(LONGITUDE)) == 3  ~ LONGITUDE / 1e1,  # Dividir por 10^3 para 3 dígitos
      TRUE ~ LONGITUDE  # Deixar como está para outros casos
    )
  )

# Coluna de Coordenada
df_2014 <- df_2014 %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                                remove = FALSE) %>%
  st_set_crs(4674)


# Merge Espacial, point to polygon
df_near <- df_2014 %>%
  st_join(c,
          join = st_intersects, 
          left = TRUE) 

print("Count of missing values in wickets column")
sum(is.na(df_near$code_tract)) # Perde 308 escolas nessa etapa se usa st_intersect
sum(!is.na(df_near$code_tract))

# Base com menos colunas, para checagem
checagem = subset(df_near, select = c(CODESC,CO_CEP,NOMESC,LATITUDE,LONGITUDE,
                                      CO_MICRORREGIAO,
                                      CO_DISTRITO,NO_BAIRRO,name_district,
                                      CIENCIAS,LINGUA_PORT,MATEMATICA,
                                      saresp_med,code_tract,geometry))

# Média Saresp por setor censitário
agg_tbl <- checagem %>% group_by(code_tract) %>% 
  summarise(med_saresp=mean(saresp_med),
            .groups = 'drop')

# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()

# Montagem da Base para Gráfico e Visualização
dataset_final = left_join(c, df2, by=c("code_tract"))
dataset_final <- filter(dataset_final, code_muni == 3550308)
a <- max(dataset_final$med_saresp[!is.infinite(dataset_final$med_saresp)], na.rm=TRUE)
b <- min(dataset_final$med_saresp[!is.infinite(dataset_final$med_saresp)], na.rm=TRUE)

sum(!is.na(dataset_final$med_saresp)) # Contagem de Setores Censitários com valor

# Visualização
ggplot() +
  geom_sf(data=dataset_final, aes(fill=med_saresp), color= NA, size=.15)+
  labs(title="Média SARESP, por Setor Censitário - São Paulo (2014)",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Greens", limits=c(b, a),
                       name="Nota Saresp")+
  theme_minimal()

