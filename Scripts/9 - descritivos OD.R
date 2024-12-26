library(dplyr)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)
library(data.table)
library(foreign)
library(od)

sp_2017 <- read.dbf("Dados/OD 2017/Banco de dados/OD_2017.dbf")
sp_2007 <- read.dbf("Dados/OD 2007/Banco de dados/OD_2007_v2d.dbf")

# estudante ens médio
em17 <- sp_2017[sp_2017$ESTUDA%in%4,]
em07 <- sp_2007[sp_2007$ESTUDA%in%4,]

# viajens escola
vem07 <- em07[em07$MOTIVO_D %in% 4,]
vem17 <- em17[em17$MOTIVO_D %in% 4,]

# Verifica IDs com mais de um valor de DATA
ids_com_varias_datas <- sp_2017 %>%
  group_by(ID_PESS) %>%                 # Agrupa por ID_PESS
  summarise(n_datas = n_distinct(DATA)) %>% # Conta valores distintos na coluna DATA
  filter(n_datas > 1)                   # Filtra IDs com mais de uma DATA

# Exibe os IDs problemáticos
ids_com_varias_datas # Vazio


# Calculate the average hour of going to school
average_hour_g <- mean(vem07$H_SAIDA[vem07$N_VIAG == 1], na.rm = TRUE) 
average_min_g <- mean(vem07$MIN_SAIDA[vem07$N_VIAG == 1], na.rm = TRUE)
average_duration <- mean(vem17$DURACAO[vem17$N_VIAG == 1], na.rm = TRUE) 
average_walking <- mean(vem07$ANDA_D[vem07$N_VIAG == 1], na.rm = TRUE) 

table(em07$H_SAIDA[em07$MOTIVO_D%in%4])
table(sp_2017$H_SAIDA[sp_2017$MOTIVO_D%in%4 & sp_2017$TIPO_ESC%in%1])



# viagens procurar emprego
table(em17$PAG_VIAG[em17$TIPO_ESC%in%1])
table(em17$PAG_VIAG[em17$TIPO_ESC%in%2])
table(em07$PAG_VIAG)



# modal
table(vem17$MODOPRIN)


# Criando o gráfico de densidade
ggplot(vem17, aes(x = H_SAIDA)) +
  geom_density(fill = "blue", alpha = 0.4) +
  scale_x_continuous(
    breaks = seq(floor(min(vem07$H_SAIDA)), ceiling(max(vem07$H_SAIDA)), by = 1)
  ) +
  labs(
    x = "Hour",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()  # Remove todas as linhas de grade
  )
