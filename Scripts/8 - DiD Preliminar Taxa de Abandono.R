# DiD em que Toda São Paulo é tratada, mas tiro o resto da RMSP da base
## Restante da Base é controle
library(dplyr)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)
library(data.table)

d <- fread("Dados/Dados Tratados/rend_esc.csv")

# Removing other cities from the Metropolitan Area (could be partially treated)
da <- d %>% filter(!(d$cod_mun%in%c(3503901,3505708,3506607,
                         3509007,3509205,3510609,3513009,
                         3513801,3515004,3515103,3515707,3516309,
                         3516408,3518305,3518800,3522208,
                         3522505,3523107,3525003,3526209,
                         3528502,3529401,3530607,3534401,
                         3539103,3539806,3543303,3544103,
                         3545001,3546801,3547304,3547809,
                         3548708,3548807,3549953,
                         3552502,3552809,3556453)))


# Desconsiderar escolas Particulares e Privadas (OPCIONAL)
#da <- da %>% filter(!(rede %in% c("Particular", "Privada")))

# Making numeric columns percent
da[, 10:ncol(da)] <- da[, 10:ncol(da)] / 100

# Dummy de começo do Tratamento: 2015
# Dummy do grupo tratado
# Interação entre Tratamento e tempo
da <- da %>% 
  mutate(time = ifelse(Ano >= 2015, 1, 0),
         treated = ifelse(cod_mun == 3550308 
                          & (rede == "Estadual" |
                             rede == "Municipal"|
                             rede == "Federal"), 1, 0),
         did = time * treated)

# Simple DiD
didreg = lm(abandono_tot_em ~ treated + time + did, data = da)
summary(didreg)

# Another way of calculating Simple DiD, following The Effect
da <- da %>%
  mutate(Treated = cod_mun == 3550308 & (rede == "Estadual" |
              rede == "Municipal"|
              rede == "Federal") & 
           Ano >= 2015)

clfe <- feols(abandono_tot_em ~ Treated | CODESC + Ano,
              data = da)
msummary(clfe, stars = c('*' = .1, '**' = .05, '***' = .01))

# Teste Placebo, para ter uma ideia se viola tendências paralelas
da2 <- da %>%
  # Use only pre-treatment data
  filter(Ano <= 2014)

# Create our fake treatment variables
da2 <- da2 %>%
  mutate(FakeTreat1 = cod_mun == 3550308 & 
           Ano >= 2014,
         FakeTreat2 = cod_mun == 3550308 &
           Ano == 2011)

# Run the same model we did before but with our fake treatment
clfe1 <- feols(abandono_tot_em ~ FakeTreat1 | CODESC + Ano,
               data = da2)
clfe2 <- feols(abandono_tot_em ~ FakeTreat2 | CODESC + Ano,
               data = da2)

msummary(list(clfe1,clfe2), stars = c('*' = .1, '**' = .05, '***' = .01))

# Dynamic DiD
# Treatment variable
da <- da %>% mutate(sp = cod_mun == 3550308 & 
                    (rede == "Estadual" |
                     rede == "Municipal"|
                     rede == "Federal"))

clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da)

# And use iplot() for a graph of effects
iplot(clfe)

# Dynamic DiD only with the State of Sao Paulo
clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da[da$UF == "SP", ])

# And use iplot() for a graph of effects
iplot(clfe)

# Dynamic DiD only with state capitals of Southeast
clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da[da$cod_mun%in%c(3550308,3304557,
                                                         3205309,3106200), ])

# And use iplot() for a graph of effects
iplot(clfe)

# Dynamic DiD only with all state capitals
clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da[da$cod_mun%in%c(3550308,3304557,
                                                         3205309,3106200,
                                                         1100205,1200401,
                                                         1302603,1400100,
                                                         1501402,1600303,
                                                         1721000,2111300,
                                                         2211001,2304400,
                                                         2408102,2507507,
                                                         2611606,2704302,
                                                         2800308,2927408,
                                                         4106902,4205407,
                                                         4314902,5002704,
                                                         5103403,5208707,
                                                         5300108), ])

# And use iplot() for a graph of effects
iplot(clfe)

# Dynamic DiD only with cities from aopdata (most populated)
clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da[da$cod_mun%in%c(3106200,3509502,
                                                         4106902,2304400,
                                                         5208707,4314902,
                                                         2611606,3304557,
                                                         3550308,1501402,
                                                         5300108,5002704,
                                                         3301702,2704302,
                                                         1302603,2408102,
                                                         2927408,3304904,
                                                         2111300), ])

# And use iplot() for a graph of effects
iplot(clfe)

# Dynamic DiD only with only Sao Paulo 
clfe <- feols(abandono_tot_em ~ i(Ano, sp, ref = 2014) | 
                CODESC + Ano, data = da[da$cod_mun%in%c(3106200,3509502,
                                                         4106902,2304400,
                                                         5208707,4314902,
                                                         2611606,3304557,
                                                         3550308,1501402,
                                                         5300108,5002704,
                                                         3301702,2704302,
                                                         1302603,2408102,
                                                         2927408,3304904,
                                                         2111300), ])

# And use iplot() for a graph of effects
iplot(clfe)