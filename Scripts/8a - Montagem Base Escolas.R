library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(geobr)
library(h3r)

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

# Accessibility Data
access <- fread("Dados/Dados Tratados/accessibility_2014.csv")

# Schools Data
escolas1 <- read_schools(year=2020,showProgress = TRUE)
escolas1 <- escolas1[(escolas1$name_muni %in% "São Paulo"),]
escolas <- sfheaders::sf_to_df(escolas1)[c("x", "y")]
escolas$CODESC <- escolas1$code_school
escolas <- escolas[!(escolas$x %in% NaN),]
escolas <- escolas[!(escolas$y %in% NaN),]
escolas$resolution <- 9
lat = c(escolas$y)
lng = c(escolas$x)
resolution = c(escolas$resolution,escolas$resolution)
escolas <- escolas %>% 
  mutate(id = latLngToCell(lat, lng, resolution))

sch_access <- escolas %>%
  left_join(access, by = "id")
# Dropout and Abandonment rates data
d <- fread("Dados/Dados Tratados/rend_esc.csv")

# Combining data
da <- d %>%
  left_join(censo, by = "CODESC")

da <- da %>%
  left_join(inse, by = "CODESC")

da <- da %>%
  left_join(enem, by = "CODESC")

da <- da %>%
  left_join(sch_access, by = "CODESC")

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

# Only the city of São Paulo
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

# Data Cleaning: Removing schools that did not exist before treatment
# Filtrar apenas as linhas onde abandono_tot_em não é vazio (não é NA ou não é uma string vazia)
dta_sp_filtrada <- dta_sp %>% 
  filter(!is.na(abandono_tot_em))

# Filtrando a base dta_sp para anos maiores que 2007
dta_sp_filtrada2 <- dta_sp_filtrada %>%
  filter(Ano > 2007)

# Filtrar dados para anos antes de 2015
antes_2015 <- dta_sp_filtrada2 %>% filter(Ano < 2015)

# Filtrar dados para anos a partir de 2015
depois_2015 <- dta_sp_filtrada2 %>% filter(Ano >= 2015)

# Identificar CODESC que aparecem apenas depois de 2015, mas não antes
codesc_antes_2015 <- unique(antes_2015$CODESC)
codesc_depois_2015 <- unique(depois_2015$CODESC)

# CODESC que aparecem apenas depois de 2015
codesc_exclusivos_depois_2015 <- setdiff(codesc_depois_2015, codesc_antes_2015)

# Mostrar os CODESC que aparecem apenas após 2015
codesc_exclusivos_depois_2015

# Remover os CODESC que aparecem apenas depois de 2015
dta_sp_filtrada3 <- dta_sp_filtrada2 %>% 
  filter(!CODESC %in% codesc_exclusivos_depois_2015)

# Contar o número de anos distintos para cada CODESC
escolas_por_ano <- dta_sp_filtrada3 %>%
  group_by(CODESC) %>%
  summarise(anos_presentes = n_distinct(Ano))

# Identificar as escolas que aparecem em todos os anos de 2007 a 2023
escolas_com_todos_anos <- escolas_por_ano %>%
  filter(anos_presentes == 16) %>%  # 17 anos de 2007 a 2023
  pull(CODESC)

# Filtrar a base original para manter apenas as escolas que aparecem em todos os anos
dta_sp_filtrada4 <- dta_sp_filtrada2 %>% 
  filter(CODESC %in% escolas_com_todos_anos)

# Criando a variável 'sem_enem'
dta_sp_filtrada4$sem_enem <- ifelse(
  apply(
    dta_sp_filtrada4[, c("NU_TAXA_PARTICIPACAO", "NU_MEDIA_CN", "NU_MEDIA_CH", 
                         "NU_MEDIA_LP", "NU_MEDIA_MT", "NU_MEDIA_RED", "PC_FORMACAO_DOCENTE")], 
    1, 
    function(row) any(is.na(row))),
  1, 0)

# Renomeando a coluna do INSE
dta_sp_filtrada4 <- dta_sp_filtrada4 %>%
  rename(inse_abs = `INSE - VALOR ABSOLUTO`)


#summary(dta_sp_filtrada_inse$sem_enem)
#summary(dta_sp_filtrada_inse$NU_MEDIA_MT)
dta_sp_filtrada4$NU_MEDIA_MT_x <- ifelse(is.na(dta_sp_filtrada4$NU_MEDIA_MT), 0, dta_sp_filtrada4$NU_MEDIA_MT)


dta_sp_filtrada4$INSE_num_x <- ifelse(is.na(dta_sp_filtrada4$INSE_num), 0, dta_sp_filtrada4$INSE_num)
dta_sp_filtrada4$P001_x <- ifelse(is.na(dta_sp_filtrada4$P001), 0, dta_sp_filtrada4$P001)


dta_sp_filtrada4$INSE_4 <-ifelse(dta_sp_filtrada4$INSE_num%in%4,1,0)
dta_sp_filtrada4$INSE_5 <-ifelse(dta_sp_filtrada4$INSE_num%in%5,1,0)
dta_sp_filtrada4$INSE_6 <-ifelse(dta_sp_filtrada4$INSE_num%in%6,1,0)
dta_sp_filtrada4$INSE_7 <-ifelse(dta_sp_filtrada4$INSE_num%in%7,1,0)

# Base de dados apenas com escolas com INSE "Alto"
dta_sp_filtrada_inse <- dta_sp_filtrada4 %>%
  filter(INSE_num == 6)

rm(d)
gc()

# Exportando
write.csv(da, "Dados/Dados Tratados/base_final_br.csv", row.names = FALSE)
write.csv(dta_sp_filtrada, "Dados/Dados Tratados/base_final_sp.csv", row.names = FALSE)
write.csv(dta_sp_filtrada3, "Dados/Dados Tratados/base_final_sp_todas.csv", row.names = FALSE)
write.csv(dta_sp_filtrada4, "Dados/Dados Tratados/base_final_sp_todososanos.csv", row.names = FALSE)
write.csv(dta_sp_filtrada4, "Dados/Dados Tratados/base_final_sp_inse6.csv", row.names = FALSE)

