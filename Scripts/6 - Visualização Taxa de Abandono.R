# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)

d <- read.csv("C:/Users/rosan/Downloads/rend_esc.csv")
da <- select(d,c(Ano,cod_mun,CODESC,abandono_tot_em,aprov_tot_em,reprov_tot_em))

nrow(da[da$cod_mun== 3509007 & da$abandono_tot_em == 0,])

# Substitua 'codigo_sp' pelo código real de São Paulo
codigo_sp <- 3550308

# Lista de códigos das cidades de interesse
rmsp <- c(3503901,3505708,3506607,
                       3509007,3509205,3510609,3513009,
                       3513801,3515103,3515707,3516309,
                       3516408,3518305,3518800,3522208,
                       3522505,3523107,3525003,3526209,
                       3528502,3529401,3530607,3534401,
                       3539103,3539806,3543303,3544103,
                       3545001,3546801,3547304,3547809,
                       3548708,3548807,3549953,3552502,
                       3552809,3556453)  

# Filtrar dados para São Paulo e calcular a média da taxa de abandono por ano
d_pais <- d %>%
  group_by(Ano) %>%
  summarize(media_taxa_abandono_pais = mean(abandono_tot_em, na.rm = TRUE))


# Filtrar dados para São Paulo e calcular a média da taxa de abandono por ano
df_sp <- d %>%
  filter(cod_mun == codigo_sp) %>%
  group_by(Ano) %>%
  summarize(media_taxa_abandono = mean(abandono_tot_em, na.rm = TRUE))

# Filtrar dados para o estado de São Paulo
df_estado_sp <- d %>%
  filter(UF == "SP" & !(cod_mun %in% c(codigo_sp, rmsp))) %>%
  group_by(Ano) %>%
  summarize(media_taxa_abandono_estado_sp = mean(abandono_tot_em, na.rm = TRUE))

# Filtrar dados para as cidades de interesse e calcular a média
df_cidades <- d %>%
  filter(cod_mun %in% rmsp) %>%
  group_by(Ano) %>%
  summarize(media_taxa_abandono_cidades = mean(abandono_tot_em, na.rm = TRUE))

# Filtrar dados para todas as localidades que não estão em São Paulo
df_restante <- d %>%
  filter(UF != "SP") %>%
  group_by(Ano) %>%
  summarize(media_taxa_abandono_restante = mean(abandono_tot_em, na.rm = TRUE))

# Combinar os dados em um único data frame
df_combined <- d_pais %>%
  left_join(df_sp, by = "Ano") %>%
  left_join(df_estado_sp, by = "Ano") %>%
  left_join(df_cidades, by = "Ano") %>%
  left_join(df_restante, by = "Ano")

# Criar o gráfico
ggplot(df_combined, aes(x = Ano)) +
  geom_line(aes(y = media_taxa_abandono_pais, color = "Nacional"), size = 1) +
  geom_point(aes(y = media_taxa_abandono_pais, color = "Nacional"), size = 2) +
  geom_line(aes(y = media_taxa_abandono, color = "Cidade de São Paulo"), size = 1) +
  geom_point(aes(y = media_taxa_abandono, color = "Cidade de São Paulo"), size = 2) +
  geom_line(aes(y = media_taxa_abandono_estado_sp, color = "Estado de São Paulo"), linetype = "dashed", size = 1) +
  geom_point(aes(y = media_taxa_abandono_estado_sp, color = "Estado de São Paulo"), shape = 17, size = 2) +
  geom_line(aes(y = media_taxa_abandono_cidades, color = "RMSP"), linetype = "dotted", size = 1) +
  geom_point(aes(y = media_taxa_abandono_cidades, color = "RMSP"), shape = 15, size = 2) +
  geom_line(aes(y = media_taxa_abandono_restante, color = "Restante da Base"), linetype = "dotdash", size = 1) +
  geom_point(aes(y = media_taxa_abandono_restante, color = "Restante da Base"), shape = 18, size = 2) +
  labs(title = "Taxa de Abandono Médio por Ano",
       x = "Ano",
       y = "Taxa de Abandono Médio (%)",
       color = "Legendas") +
  theme_minimal()

summary(d$abandono_tot_em)
summary(d$abandono_tot_em)