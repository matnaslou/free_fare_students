library(dplyr)
library(data.table)
library(did)

da <- fread("Dados/Dados Tratados/base_final_br.csv")

da <- da %>%
  filter(rede != "Particular" & rede != "Privada")

# Ordering Data
da <- da %>%
  arrange(CODESC, Ano)

# Group Variable (in this case, its only the 2015 group)
da <- da %>%
  mutate(first.treat = ifelse(rede %in% c("Particular", "Privada") |
                              cod_mun != 3550308, 0, 2015))

# Treat Variable (Value 1 for every treated unit, 0 otherwise)
da <- da %>%
  mutate(treat = ifelse(rede %in% c("Particular", "Privada") |
                          cod_mun != 3550308, 0, 1))

# Data Cleaning: Removing schools that did not exist before treatment
# Filtrar apenas as linhas onde abandono_tot_em não é vazio (não é NA ou não é uma string vazia)
da_filtrada <- da %>% 
  filter(!is.na(abandono_tot_em))

# Filtrando a base da para anos maiores que 2007
da_filtrada2 <- da_filtrada %>%
  filter(Ano > 2008)

# Filtrar dados para anos antes de 2015
antes_2015 <- da_filtrada2 %>% filter(Ano < 2015)

# Filtrar dados para anos a partir de 2015
depois_2015 <- da_filtrada2 %>% filter(Ano >= 2015)

# Identificar CODESC que aparecem apenas depois de 2015, mas não antes
codesc_antes_2015 <- unique(antes_2015$CODESC)
codesc_depois_2015 <- unique(depois_2015$CODESC)

# CODESC que aparecem apenas depois de 2015
codesc_exclusivos_depois_2015 <- setdiff(codesc_depois_2015, codesc_antes_2015)

# Mostrar os CODESC que aparecem apenas após 2015
codesc_exclusivos_depois_2015

# Remover os CODESC que aparecem apenas depois de 2015
da_filtrada3 <- da_filtrada2 %>% 
  filter(!CODESC %in% codesc_exclusivos_depois_2015)

# Remover Anos pós Pandemia
da_filtrada3 <- da_filtrada3 %>%
  filter(Ano < 2020)

# Contar o número de anos distintos para cada CODESC
escolas_por_ano <- da_filtrada3 %>%
  group_by(CODESC) %>%
  summarise(anos_presentes = n_distinct(Ano))

# Identificar as escolas que aparecem em todos os anos de 2008 a 2019
escolas_com_todos_anos <- escolas_por_ano %>%
  filter(anos_presentes == 11) %>%  # 17 anos de 2007 a 2023
  pull(CODESC)

# Filtrar a base original para manter apenas as escolas que aparecem em todos os anos
da_filtrada4 <- da_filtrada3 %>% 
  filter(CODESC %in% escolas_com_todos_anos)

# Criando a variável 'sem_enem'
da_filtrada4$sem_enem <- ifelse(
  apply(
    da_filtrada4[, c("NU_TAXA_PARTICIPACAO", "NU_MEDIA_CN", "NU_MEDIA_CH", 
                         "NU_MEDIA_LP", "NU_MEDIA_MT", "NU_MEDIA_RED", "PC_FORMACAO_DOCENTE")], 
    1, 
    function(row) any(is.na(row))),
  1, 0)

# Renomeando a coluna do INSE
da_filtrada4 <- da_filtrada4 %>%
  rename(inse_abs = `INSE - VALOR ABSOLUTO`)


#summary(da_filtrada_inse$sem_enem)
#summary(da_filtrada_inse$NU_MEDIA_MT)
da_filtrada4$NU_MEDIA_MT_x <- ifelse(is.na(da_filtrada4$NU_MEDIA_MT), 0, da_filtrada4$NU_MEDIA_MT)
da_filtrada4$NU_MEDIA_CN_x <- ifelse(is.na(da_filtrada4$NU_MEDIA_CN), 0, da_filtrada4$NU_MEDIA_CN)
da_filtrada4$NU_MEDIA_CH_x <- ifelse(is.na(da_filtrada4$NU_MEDIA_CH), 0, da_filtrada4$NU_MEDIA_CH)

da_filtrada4$INSE_num_x <- ifelse(is.na(da_filtrada4$INSE_num), 0, da_filtrada4$INSE_num)
da_filtrada4$P001_x <- ifelse(is.na(da_filtrada4$P001), 0, da_filtrada4$P001)


da_filtrada4$INSE_4 <-ifelse(da_filtrada4$INSE_num%in%4,1,0)
da_filtrada4$INSE_5 <-ifelse(da_filtrada4$INSE_num%in%5,1,0)
da_filtrada4$INSE_6 <-ifelse(da_filtrada4$INSE_num%in%6,1,0)
da_filtrada4$INSE_7 <-ifelse(da_filtrada4$INSE_num%in%7,1,0)
#da_filtrada4

# Criar uma nova variável com a taxa de abandono da escola em 2008
da_filtrada4 <- da_filtrada4 %>%
  mutate(abandono_2009 = ifelse(Ano == 2009, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  mutate(abandono_2010 = ifelse(Ano == 2010, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  mutate(abandono_2011 = ifelse(Ano == 2011, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  mutate(abandono_2012 = ifelse(Ano == 2012, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  mutate(abandono_2013 = ifelse(Ano == 2013, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  mutate(abandono_2014 = ifelse(Ano == 2014, abandono_tot_em, NA)) %>% # Adiciona os valores de 2008
  group_by(CODESC) %>% # Agrupa por escola
  mutate(abandono_2009 = first(na.omit(abandono_2009))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  mutate(abandono_2010 = first(na.omit(abandono_2010))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  mutate(abandono_2011 = first(na.omit(abandono_2011))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  mutate(abandono_2012 = first(na.omit(abandono_2012))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  mutate(abandono_2013 = first(na.omit(abandono_2013))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  mutate(abandono_2014 = first(na.omit(abandono_2014))) %>% # Propaga o valor de 2008 para todas as linhas da escola
  ungroup() # Remove o agrupamento

# Calcular a média de abandono para cada escola no período de 2008 a 2014 e adicionar à base original
da_filtrada4 <- da_filtrada4 %>%
  group_by(CODESC) %>% # Agrupa por escola
  mutate(media_abandono = mean(abandono_tot_em[Ano >= 2008 & Ano <= 2014], na.rm = TRUE)) %>%
  ungroup() # Remove o agrupamento


# Lista dos códigos IBGE das capitais do Sul e Sudeste
cod_capitais <- c(4106902, 4205407, 4314902, 3550308, 
                  3304557, 3106200, 3205309)

da_capitais <- da_filtrada4 %>%
  filter(cod_mun %in% cod_capitais)

da_capitais$mun_ef <- factor(da_capitais$cod_mun)
da_capitais$cidade1 <- ifelse(da_capitais$cod_mun == 3304557,1,0)
da_capitais$cidade2 <- ifelse(da_capitais$cod_mun == 4106902,1,0)
da_capitais$cidade3 <- ifelse(da_capitais$cod_mun == 4205407,1,0)
da_capitais$cidade4 <- ifelse(da_capitais$cod_mun == 4314902,1,0)
da_capitais$cidade5 <- ifelse(da_capitais$cod_mun == 3106200,1,0) 
da_capitais$cidade6 <- ifelse(da_capitais$cod_mun == 3205309,1,0)

da_capitais_alta <- da_capitais[is.na(da_capitais$P001) | da_capitais$P001>5457926,]
da_capitais_baixa <- da_capitais[is.na(da_capitais$P001) | da_capitais$P001<5457926,]

base <- da_capitais

# estimate group-time average treatment effects without covariates
mw.attgt <- att_gt(yname = "abandono_tot_em",
                  tname = "Ano",
                   idname = "CODESC",
                   gname = "first.treat",
                   xformla = ~1,
                   data = base,
                   base_period = "universal",
                   control_group = "nevertreated",
                   allow_unbalanced_panel = FALSE,
                   panel = TRUE,
                   anticipation = 0,
                   print_details = TRUE,
                  weightsname = "QT_MAT_MED", 
                  est_method = "ipw"
)

# Testes com da_capitais
# abandono_2008+abandono_2009+abandono_2014+INSE_4+INSE_6+INSE_7
# abandono_2008+abandono_2009+abandono_2014+INSE_4+INSE_6+INSE_7+IN_LABORATORIO_CIENCIAS+QT_DOC_MED+QT_SALAS_UTILIZADAS
# abandono_2008+abandono_2009+abandono_2014+INSE_4+INSE_6+INSE_7+QT_DOC_MED+IN_LABORATORIO_CIENCIAS
# IN_LABORATORIO_CIENCIAS+QT_DOC_MED+abandono_2008+abandono_2009+abandono_2014+INSE_4+INSE_5+INSE_7+sem_enem+NU_MEDIA_MT_x+NU_MEDIA_CN_x
# summarize the results
summary(mw.attgt)

variaveis <- c(
  # School Structure
  "abandono_2009","abandono_2010","abandono_2011",
  "abandono_2012","abandono_2013","abandono_2014"
)
# Criar a fórmula dinâmica
xformla <- as.formula(paste("~", paste(variaveis, collapse = " + ")))


# estimate group-time average treatment effects without covariates
mw.attgt2 <- att_gt(yname = "abandono_tot_em",
                   tname = "Ano",
                   idname = "CODESC",
                   gname = "first.treat",
                   xformla = xformla,
                   data = base,
                   base_period = "universal",
                   control_group = "nevertreated",
                   allow_unbalanced_panel = FALSE,
                   panel = TRUE,
                   anticipation = 0,
                   print_details = TRUE,
                   weightsname = "QT_MAT_MED", 
                   est_method = "ipw"
)

# summarize the results
summary(mw.attgt2)

# estimate group-time average treatment effects without covariates
mw.attgt3 <- att_gt(yname = "abandono_tot_em",
                    tname = "Ano",
                    idname = "CODESC",
                    gname = "first.treat",
                    xformla = ~media_abandono,
                    data = base,
                    base_period = "universal",
                    control_group = "nevertreated",
                    allow_unbalanced_panel = FALSE,
                    panel = TRUE,
                    anticipation = 0,
                    print_details = TRUE,
                    weightsname = "QT_MAT_MED", 
                    est_method = "ipw"
)

# summarize the results
summary(mw.attgt3)


variaveis2 <- c(
  # School Structure
  "abandono_2009","abandono_2010","abandono_2011",
  "abandono_2012","abandono_2013","abandono_2014","sem_enem","NU_MEDIA_MT_x",
  "NU_MEDIA_CN_x","NU_MEDIA_CH_x"
)
# Criar a fórmula dinâmica
xformla2 <- as.formula(paste("~", paste(variaveis2, collapse = " + ")))


# estimate group-time average treatment effects without covariates
mw.attgt4 <- att_gt(yname = "abandono_tot_em",
                    tname = "Ano",
                    idname = "CODESC",
                    gname = "first.treat",
                    xformla = xformla2,
                    data = base,
                    base_period = "universal",
                    control_group = "nevertreated",
                    allow_unbalanced_panel = FALSE,
                    panel = TRUE,
                    anticipation = 0,
                    print_details = TRUE,
                    weightsname = "QT_MAT_MED", 
                    est_method = "ipw"
)

# summarize the results
summary(mw.attgt4)

# plot the results
# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt4, ylim = c(-.07, .07))
es <- aggte(mw.attgt, type = "simple")
summary(es)
ggdid(es)