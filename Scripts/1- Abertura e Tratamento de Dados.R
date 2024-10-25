library(dplyr)
library(readr)
library(tidyr)
library(stringr)

setwd("C:/Users/rosan/Dropbox/Matheus/Projeto SARESP")
# Dados de Endereços das escolas estaduais de São Paulo
end_esc_2018 <- read_delim("Dados/Dados SARESP/ENDERECO_ESCOLAS_2018_0(2).csv", 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-2"), 
                       trim_ws = TRUE)

# Dados de notas do SARESP, por Escola
saresp_2014 <- read_delim("Dados/Dados SARESP/Proficiência do SARESP por Escola de 2014(1).csv", 
                          delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                          trim_ws = TRUE)
# Dados do Censo Escolar
censo_2014 <- read_delim("Dados/Dados Censo/microdados_ed_basica_2014.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
# Apenas escolas do Estado de SP
censo_2014 <- censo_2014[censo_2014$CO_UF == 35 , ]

# Ajustando a coluna do Código da escola para juntar com SARESP
censo_2014$CO_ENTIDADE_str = as.character(censo_2014$CO_ENTIDADE)
censo_2014$CO_ENTIDADE_str = substr(censo_2014$CO_ENTIDADE_str, start = 3, stop = nchar(censo_2014$CO_ENTIDADE_str)) 
censo_2014$CO_ENTIDADE_sem_zeros = str_remove_all(censo_2014$CO_ENTIDADE_str, "^0+")
censo_2014$CO_ENTIDADE_final = as.numeric(censo_2014$CO_ENTIDADE_sem_zeros)

# Renomeando Coluna para Merge
colnames(end_esc_2018)[colnames(end_esc_2018) == "COD_ESC"] ="CODESC"
# Removendo Colunas que geraram duplicatas (Endereço cadastrado de duas formas diferentes, mas mesmo CEP)
end_esc_2018 <- subset(end_esc_2018, select = c(CODESC,LATITUDE,
                                                LONGITUDE))
# Removendo Duplicatas
end_esc_2018 <- end_esc_2018[!duplicated(end_esc_2018), ]
# Mantendo apenas Escolas com 3º-EM
saresp_2014 <- saresp_2014[saresp_2014$SERIE_ANO == 'EM-3ª série' & saresp_2014$PERIODO == 'GERAL', ]
# Tirando colunas do endereço escrito por extenso, pois há duplicatas de escolas com endereço diferente
saresp_2014$ENDESC <- NULL
saresp_2014$NUMESC <- NULL
saresp_2014$COMPLEND <- NULL
# Removendo Duplicatas (Mesmo CEP, mas endereço por extenso diferente)
saresp_2014 <- saresp_2014[!duplicated(saresp_2014), ]
# Substituindo Notas com valor "NULL" por ´NA´
saresp_2014 <- saresp_2014 %>%
  mutate(MEDPROF = ifelse(MEDPROF == "NULL", NA, MEDPROF))
#Transformando Nota em numérico
saresp_2014 <- saresp_2014 %>%
  mutate(MEDPROF = as.numeric(MEDPROF))
# Checando total de escolas na base
length(unique(saresp_2014$CODESC))
# Juntando informações de endereços para as Escolas
saresp_2014_end <- merge(saresp_2014, end_esc_2018, by = c("CODESC"))
# Checando total de escolas na base
length(unique(saresp_2014_end$CODESC))
# Ajuste na coluna de Código do MEC para juntar com Censo Escolar
colnames(censo_2014)[colnames(censo_2014) == "CO_ENTIDADE_final"] ="CODESC"
# Merge com Censo
saresp_2014_censo <- merge(saresp_2014_end, censo_2014, by = c("CODESC"))
# Checando total de escolas na base
length(unique(saresp_2014_censo$CODESCMEC))
unique(saresp_2014$NOMEDEPBOL)
# Criando colunas de nota em cada disciplina para cada escola
# Finding maximum value 
max_values <- saresp_2014_censo %>%
  group_by(CODESC,DS_COMP) %>%
  summarise(max_value = max(MEDPROF))
# Transformando para formato largo
wide_max_values <- max_values %>%
  pivot_wider(names_from = DS_COMP, values_from = max_value)
# Adicionando essas colunas de notas
saresp_2014_censo <- merge(saresp_2014_censo, wide_max_values, by = c("CODESC"))
# Checando total de escolas na base
length(unique(saresp_2014_censo$CODESCMEC))
# Retirando linhas duplicadas
saresp_2014_censo = subset(saresp_2014_censo, select = -c(CO_COMP,DS_COMP,MEDPROF) )
saresp_2014_censo <- saresp_2014_censo[!duplicated(saresp_2014_censo), ]
# Checando total de escolas na base
length(unique(saresp_2014_censo$CODESC))

# Arrumando para Exportar
colnames(saresp_2014_censo)[colnames(saresp_2014_censo) == "CIÊNCIAS"] ="CIENCIAS"
colnames(saresp_2014_censo)[colnames(saresp_2014_censo) == "LÍNGUA PORTUGUESA"] ="LINGUA_PORT"
colnames(saresp_2014_censo)[colnames(saresp_2014_censo) == "MATEMÁTICA"] ="MATEMATICA"

# Removendo a duplicata
saresp_2014_censo <- saresp_2014_censo[!(saresp_2014_censo$CODESC == 15519 
                                         & saresp_2014_censo$LATITUDE ==
                                           saresp_2014_censo$LONGITUDE),]

# Remover observações com LATITUDE E LONGITUDE == 0
saresp_2014_censo <- saresp_2014_censo[!(saresp_2014_censo$LATITUDE == 0 
                                         & saresp_2014_censo$LONGITUDE == 0),]


write.csv(saresp_2014_censo, 
          "C:/Users/rosan/Dropbox/Matheus/Projeto SARESP/Dados/Dados Tratados/df_2014.csv",
          row.names=FALSE)