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

# Enem
enem <- fread("Dados/Dados Enem/DADOS/MICRODADOS_ENEM_ESCOLA.csv")

enem <- enem %>%
  filter(NU_ANO%in%(2014))

# Renaming Column for merge
enem <- enem %>%
  rename(CODESC = CO_ESCOLA_EDUCACENSO)

# Selecionar apenas a coluna 'CODESC' e as demais colunas listadas
colunas_desejadas <- c("CODESC", "NU_TAXA_PARTICIPACAO", "NU_MEDIA_CN", "NU_MEDIA_CH", 
                       "NU_MEDIA_LP", "NU_MEDIA_MT", "NU_MEDIA_RED", "PC_FORMACAO_DOCENTE")

# Selecionar as colunas com select() do dplyr
enem <- enem %>%
  select(all_of(colunas_desejadas))

# INSE
# Socio-economic data
inse <- read_excel("Dados/Dados Censo/Indicador_INSE_por_Escola.xlsx", sheet = "Banco",
                   skip = 9)

# Renaming Column for merge
inse <- inse %>%
  rename(CODESC = COD_ESCOLA)

# Renaming Column for merge
inse <- inse %>%
  rename(INSE = `INSE - CLASSIFICAÇÃO`)

# Num to int variable
inse <- inse %>%
  mutate(CODESC = as.integer(CODESC))

# Selecting only relevant information
inse <- inse[, c(1, (ncol(inse) - 1):ncol(inse))]

# Turning socioeconomic info in factor
inse$INSE <- factor(
  inse$INSE,
  levels = c("Muito Baixo", "Baixo", "Médio Baixo", "Médio", "Médio Alto",
             "Alto", "Muito Alto"),
  labels = c("Muito Baixo", "Baixo", "Médio Baixo", "Médio", "Médio Alto",
             "Alto", "Muito Alto")
)

inse$INSE_num <- as.numeric(inse$INSE)

# Scholar Census data
# Reading Scholar Census Data
censo <- fread("Dados/Dados Censo/microdados_ed_basica_2014/dados/microdados_ed_basica_2014.csv")

# Renaming Column for merge
censo <- censo %>%
  rename(CODESC = CO_ENTIDADE)

# Dropout and Abandonment rates data
d <- fread("Dados/Dados Tratados/rend_esc.csv")

# Combining data
da <- d %>%
  left_join(censo, by = "CODESC")

da <- da %>%
  left_join(inse, by = "CODESC")

da <- da %>%
  left_join(enem, by = "CODESC")

# Removing other cities from the Metropolitan Area (could be partially treated)
da <- da %>% filter(!(da$cod_mun%in%c(3503901,3505708,3506607,
                         3509007,3509205,3510609,3513009,
                         3513801,3515004,3515103,3515707,3516309,
                         3516408,3518305,3518800,3522208,
                         3522505,3523107,3525003,3526209,
                         3528502,3529401,3530607,3534401,
                         3539103,3539806,3543303,3544103,
                         3545001,3546801,3547304,3547809,
                         3548708,3548807,3549953,
                         3552502,3552809,3556453)))


# Making numeric columns percent
#da[, 10:ncol(da)] <- da[, 10:ncol(da)] / 100
# Dividir as colunas desejadas por 100
da <- da %>%
  mutate(across(aprov_1o_ano:abandono_tot_em, ~ . / 100))

# Frequência Matrículas
da <- da %>%
  mutate(
    freq_mat_bas_masc = QT_MAT_BAS_MASC / QT_MAT_BAS,
    freq_mat_bas_branca = QT_MAT_BAS_BRANCA / QT_MAT_BAS,
    freq_mat_bas_preta = QT_MAT_BAS_PRETA / QT_MAT_BAS,
    freq_mat_bas_parda = QT_MAT_BAS_PARDA / QT_MAT_BAS,
    freq_mat_bas_amarela = QT_MAT_BAS_AMARELA / QT_MAT_BAS
  )

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
           Ano >= 2011)

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


#####################
# Doubly Robust DiD #
#####################

dta_sp <- da %>%
  filter(cod_mun%in%c(3550308))

# Ordering Data
dta_sp <- dta_sp %>%
  arrange(CODESC, Ano)

# Group Variable (in this case, its only the 2015 group)
dta_sp <- dta_sp %>%
  mutate(first.treat = ifelse(rede %in% c("Particular", "Privada"), 0, 2015))

# Treat Variable (Value 1 for every treated unit, 0 otherwise)
dta_sp <- dta_sp %>%
  mutate(treat = ifelse(rede %in% c("Particular", "Privada"), 0, 1))

# Control Variables
# Identificar as variáveis a partir de "TP_OCUPACAO_PREDIO_ESCOLAR"
variaveis <- c(
  # School Structure
  "IN_AREA_VERDE","IN_AUDITORIO","IN_BIBLIOTECA","IN_LABORATORIO_CIENCIAS",
  "IN_LABORATORIO_INFORMATICA","IN_PARQUE_INFANTIL",
  "IN_QUADRA_ESPORTES",
  "IN_DEPENDENCIAS_PNE","IN_LAVANDERIA",
  "IN_ESGOTO_REDE_PUBLICA","IN_BANDA_LARGA")
# Criar a fórmula dinâmica
xformla <- as.formula(paste("~", paste(variaveis, collapse = " + ")))

# Data Cleaning: Removing schools that did not exist before treatment
# Filtrar apenas as linhas onde abandono_tot_em não é vazio (não é NA ou não é uma string vazia)
dta_sp_filtrada <- dta_sp %>% 
  filter(!is.na(abandono_tot_em))


# Filtrar dados para anos antes de 2015
antes_2015 <- dta_sp_filtrada %>% filter(Ano < 2015)

# Filtrar dados para anos a partir de 2015
depois_2015 <- dta_sp_filtrada %>% filter(Ano >= 2015)

# Identificar CODESC que aparecem apenas depois de 2015, mas não antes
codesc_antes_2015 <- unique(antes_2015$CODESC)
codesc_depois_2015 <- unique(depois_2015$CODESC)

# CODESC que aparecem apenas depois de 2015
codesc_exclusivos_depois_2015 <- setdiff(codesc_depois_2015, codesc_antes_2015)

# Mostrar os CODESC que aparecem apenas após 2015
codesc_exclusivos_depois_2015

# Remover os CODESC que aparecem apenas depois de 2015
dta_sp_filtrada2 <- dta_sp_filtrada %>% 
  filter(!CODESC %in% codesc_exclusivos_depois_2015)

# Contar o número de anos distintos para cada CODESC
escolas_por_ano <- dta_sp_filtrada %>%
  group_by(CODESC) %>%
  summarise(anos_presentes = n_distinct(Ano))

# Identificar as escolas que aparecem em todos os anos de 2007 a 2023
escolas_com_todos_anos <- escolas_por_ano %>%
  filter(anos_presentes == 17) %>%  # 17 anos de 2007 a 2023
  pull(CODESC)

# Filtrar a base original para manter apenas as escolas que aparecem em todos os anos
dta_sp_filtrada3 <- dta_sp %>% 
  filter(CODESC %in% escolas_com_todos_anos)


# estimate group-time average treatment effects without covariates
mw.attgt <- att_gt(yname = "abandono_tot_em",
                   gname = "first.treat",
                   idname = "CODESC",
                   tname = "Ano",
                   xformla = xformla,
                   data = dta_sp_filtrada3,
                   base_period = "universal",
                   allow_unbalanced_panel = TRUE
)

# summarize the results
summary(mw.attgt)

# plot the results
# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt, ylim = c(-.059, .059))
