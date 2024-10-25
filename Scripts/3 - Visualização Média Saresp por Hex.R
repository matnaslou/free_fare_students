library(aopdata)
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

# baixa a grade espacial
grade_sp <- aopdata::read_grid(city = "São Paulo")

# Dados SARESP Tratados
df_2014 <-  df_2014 <- read_csv("Dados/Dados Tratados/df_2014.csv", 
                                locale = locale(encoding = "ISO-8859-1"))

# Nota Média do SARESP por escola:
df_2014$saresp_med <- rowMeans(df_2014[,c(383,384,385)], na.rm=TRUE)

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

grade_sp <- grade_sp %>% st_set_crs(4674)

# Merge Espacial, point to polygon
df_near_teste <- df_2014 %>%
  st_join(grade_sp,
          join = st_intersects, 
          left = TRUE) 

print("Count of missing values in wickets column")
sum(is.na(df_near$code_tract)) # Perde 308 escolas nessa etapa se usa st_intersect
sum(!is.na(df_near$code_tract))

# Base com menos colunas, para checagem
checagem = subset(df_near_teste, select = c(CODESC,CO_CEP,NOMESC,LATITUDE,LONGITUDE,
                                      CO_MICRORREGIAO,
                                      CO_DISTRITO,NO_BAIRRO,
                                      CIENCIAS,LINGUA_PORT,MATEMATICA,
                                      saresp_med,id_hex,geometry))


# Média Saresp por setor censitário
agg_tbl <- checagem %>% group_by(id_hex) %>% 
  summarise(med_saresp=mean(saresp_med),
            .groups = 'drop')

# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()

# Montagem da Base para Gráfico e Visualização
dataset_final = left_join(grade_sp, df2, by=c("id_hex"))
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