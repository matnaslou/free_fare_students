# DiD em que Escolas Públicas de São Paulo são tratada, mas tiro o resto da RMSP da base
## Privadas de São Paulo como controle
library(dplyr)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)
library(data.table)
library(did)
library(readxl)
library(geobr)
library(h3r)

da <- fread("Dados/Dados Tratados/base_final_br.csv")
dta_sp <- fread("Dados/Dados Tratados/base_final_sp_todas.csv")
dta_sp_anos <- fread("Dados/Dados Tratados/base_final_sp_todososanos.csv")
dta_sp_inse <- fread("Dados/Dados Tratados/base_final_sp_inse6.csv")

##############################################################
# DiD Simples - Para DRDiD, pode pular esta parte do código! #
##############################################################
# Simple DiD
# didreg = lm(abandono_tot_em ~ treated + time + did, data = da)
# summary(didreg)
# 
# # Another way of calculating Simple DiD, following The Effect
# da <- da %>%
#   mutate(Treated = cod_mun == 3550308 & (rede == "Estadual" |
#               rede == "Municipal"|
#               rede == "Federal") & 
#            Ano >= 2015)
# 
# clfe <- feols(abandono_tot_em ~ Treated | CODESC + Ano,
#               data = da)
# msummary(clfe, stars = c('*' = .1, '**' = .05, '***' = .01))
# 
# # Teste Placebo, para ter uma ideia se viola tendências paralelas
# da2 <- da %>%
#   # Use only pre-treatment data
#   filter(Ano <= 2014)
# 
# # Create our fake treatment variables
# da2 <- da2 %>%
#   mutate(FakeTreat1 = cod_mun == 3550308 & 
#            Ano >= 2014,
#          FakeTreat2 = cod_mun == 3550308 &
#            Ano >= 2011)
# 
# # Run the same model we did before but with our fake treatment
# clfe1 <- feols(abandono_tot_em ~ FakeTreat1 | CODESC + Ano,
#                data = da2)
# clfe2 <- feols(abandono_tot_em ~ FakeTreat2 | CODESC + Ano,
#                data = da2)
# 
# msummary(list(clfe1,clfe2), stars = c('*' = .1, '**' = .05, '***' = .01))
# 
# # Dynamic DiD
# # Treatment variable
# da <- da %>% mutate(sp = cod_mun == 3550308 & 
#                     (rede == "Estadual" |
#                      rede == "Municipal"|
#                      rede == "Federal"))
# 
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da)
# 
# # And use iplot() for a graph of effects
# iplot(clfe)
# 
# # Dynamic DiD only with the State of Sao Paulo
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da[da$UF == "SP", ])
# 
# # And use iplot() for a graph of effects
# iplot(clfe)
# 
# # Dynamic DiD only with state capitals of Southeast
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da[da$cod_mun%in%c(3550308,3304557,
#                                                          3205309,3106200), ])
# 
# # And use iplot() for a graph of effects
# iplot(clfe)
# 
# # Dynamic DiD only with all state capitals
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da[da$cod_mun%in%c(3550308,3304557,
#                                                          3205309,3106200,
#                                                          1100205,1200401,
#                                                          1302603,1400100,
#                                                          1501402,1600303,
#                                                          1721000,2111300,
#                                                          2211001,2304400,
#                                                          2408102,2507507,
#                                                          2611606,2704302,
#                                                          2800308,2927408,
#                                                          4106902,4205407,
#                                                          4314902,5002704,
#                                                          5103403,5208707,
#                                                          5300108), ])
# 
# # And use iplot() for a graph of effects
# iplot(clfe)
# 
# # Dynamic DiD only with cities from aopdata (most populated)
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da[da$cod_mun%in%c(3106200,3509502,
#                                                          4106902,2304400,
#                                                          5208707,4314902,
#                                                          2611606,3304557,
#                                                          3550308,1501402,
#                                                          5300108,5002704,
#                                                          3301702,2704302,
#                                                          1302603,2408102,
#                                                          2927408,3304904,
#                                                          2111300), ])
# 
# # And use iplot() for a graph of effects
# iplot(clfe)
# 
# # Dynamic DiD only with only Sao Paulo 
# clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
#                 CODESC + Ano, data = da[da$cod_mun%in%c(3106200,3509502,
#                                                          4106902,2304400,
#                                                          5208707,4314902,
#                                                          2611606,3304557,
#                                                          3550308,1501402,
#                                                          5300108,5002704,
#                                                          3301702,2704302,
#                                                          1302603,2408102,
#                                                          2927408,3304904,
#                                                          2111300), ])
# 
# # And use iplot() for a graph of effects
# iplot(clfe)


#####################
# Doubly Robust DiD #
#####################

# Control Variables
variaveis <- c(
  # School Structure
  "IN_AREA_VERDE","IN_AUDITORIO","IN_BIBLIOTECA","IN_LABORATORIO_CIENCIAS",
  "IN_LABORATORIO_INFORMATICA","IN_PARQUE_INFANTIL",
  "IN_QUADRA_ESPORTES","IN_DEPENDENCIAS_PNE","IN_LAVANDERIA",
  "IN_BANDA_LARGA", "sem_enem", "NU_MEDIA_MT_x"
)
# Criar a fórmula dinâmica
xformla <- as.formula(paste("~", paste(variaveis, collapse = " + ")))
#xformla <- as.formula(paste("~", paste(selected_variables, collapse = " + ")))



# estimate group-time average treatment effects without covariates
mw.attgt <- att_gt(yname = "abandono_3a_em",
                   tname = "Ano",
                   idname = "CODESC",
                   gname = "first.treat",
                   xformla = ~sem_enem+P001_x,
                   data = dta_sp_filtrada_inse[dta_sp_filtrada_inse$Ano<2020,],
                   base_period = "universal",
                   control_group = "nevertreated",
                   allow_unbalanced_panel = FALSE,
                   panel = TRUE,
                   anticipation = 0
                   )

#P001+IN_NOTURNO+IN_DESPENSA+IN_FUND_AI+QT_MAT_BAS_PRETA+QT_MAT_BAS_15_17+QT_MAT_BAS_N+QT_DOC_ESP
# summarize the results
summary(mw.attgt)


#mw.attgt$DIDparams
#?att_gt()

#dta_sp_filtrada_inse
# plot the results
# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt, ylim = c(-.05, .05))


agg.ovearll <- aggte(mw.attgt, type = "simple")
summary(agg.ovearll)
