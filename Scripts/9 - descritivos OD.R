# DiD em que Toda São Paulo é tratada, mas tiro o resto da RMSP da base
## Restante da Base é controle
library(dplyr)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)
library(data.table)
library(foreign)

sp_2017 <- read.dbf("Dados/OD 2017/Banco de dados/OD_2017.dbf")
sp_2007 <- read.dbf("Dados/OD 2007/Banco de dados/OD_2007_v2d.dbf")

# estudante ens médio
em17 <- sp_2017[sp_2017$ESTUDA%in%4,]
em07 <- sp_2007[sp_2007$ESTUDA%in%4,]


table(sp_2017$TIPVG[sp_2017$MOTIVO_D%in%4 & sp_2017$TIPO_ESC%in%1])



# viagens procurar emprego
table(em17$PAG_VIAG[em17$TIPO_ESC%in%1])
table(em17$PAG_VIAG[em17$TIPO_ESC%in%2])
table(em07$PAG_VIAG)



# modal
table(vem17$MODOPRIN)
