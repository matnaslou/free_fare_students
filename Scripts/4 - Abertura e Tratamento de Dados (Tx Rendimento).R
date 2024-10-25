library(gdata)
library(dplyr)
library(readxl)
############################################## 2007 ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2007/TX RENDIMENTO ESCOLAS 2007.xls"
sheet_names <- excel_sheets(file_path)


# Função para ler uma aba a partir da 7ª linha
read_sheet_from_7th_line <- function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, skip = 7)
}

# Ler e combinar todas as abas
df <- lapply(sheet_names, read_sheet_from_7th_line) %>%
  bind_rows()

# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Aprovação no 1º Ano do Ensino Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

# Tirando linha inicial que só tem vazios
df <- df[-1, ]

# Ajustando vazios
df[df == "--"] <- NA

# Tornando as taxas numéricos
df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.numeric)

# Retirando as últimas linhas de cada sheet, que são valores vazios
df <- df[!is.na(df$CODESC), ]


write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2007.csv",
          row.names=FALSE)

############################################## 2008 ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escola_2008/TX RENDIMENTO POR ESCOLA 2008.xls"
sheet_names <- excel_sheets(file_path)


# Função para ler uma aba a partir da 7ª linha
read_sheet_from_7th_line <- function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, skip = 7)
}

df <- lapply(sheet_names, read_sheet_from_7th_line)
converter_colunas <- function(df) {
  df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)
  return(df)
}

# Aplicando a função a cada data frame na lista
df <- lapply(df, converter_colunas)

df_combinado <- do.call(rbind, df)

# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Aprovação no 1º Ano do Ensino Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df_combinado)[1:10] <- novos_nomes

df_combinado <- df_combinado[-1, ]

df_combinado[df_combinado == "--"] <- NA

df_combinado[, 10:ncol(df_combinado)] <- lapply(df_combinado[, 10:ncol(df_combinado)], 
                                                as.numeric)
df_combinado <- df_combinado[!is.na(df_combinado$CODESC), ]
write.csv(df_combinado, 
          "C:/Users/rosan/Downloads/rend_esc_2008.csv",
          row.names=FALSE)

############################################## 2009 ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escola_2009/TX RENDIMENTO POR ESCOLA 2009.xls"
sheet_names <- excel_sheets(file_path)


# Função para ler uma aba a partir da 7ª linha
read_sheet_from_7th_line <- function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, skip = 7)
}

df <- lapply(sheet_names, read_sheet_from_7th_line)
converter_colunas <- function(df) {
  df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)
  return(df)
}

# Aplicando a função a cada data frame na lista
df <- lapply(df, converter_colunas)

# Acessar o sexto data frame da lista
df_sexto <- df[[6]]

# Renomear a coluna específica
colnames(df_sexto)[colnames(df_sexto) == "Aprovação na 4ª série/5º Ano"] <- "Aprovaçãona 4ª série/5º Ano"

# Atualizar o sexto data frame na lista com a nova coluna
df[[6]] <- df_sexto

df_combinado <- do.call(rbind, df)

# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Aprovação no 1º Ano do Ensino Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df_combinado)[1:10] <- novos_nomes

df_combinado <- df_combinado[-1, ]

df_combinado[df_combinado == "--"] <- NA

df_combinado[, 10:ncol(df_combinado)] <- lapply(df_combinado[, 10:ncol(df_combinado)], 
                                                as.numeric)
df_combinado <- df_combinado[!is.na(df_combinado$CODESC), ]
write.csv(df_combinado, 
          "C:/Users/rosan/Downloads/rend_esc_2009.csv",
          row.names=FALSE)

############################################## 2010 ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2010_19082011/tx_rendimento_escolas_2010_19082011.xls"
sheet_names <- excel_sheets(file_path)


# Função para ler uma aba a partir da 7ª linha
read_sheet_from_7th_line <- function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, skip = 7)
}

df <- lapply(sheet_names, read_sheet_from_7th_line)
converter_colunas <- function(df) {
  df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)
  return(df)
}

# Aplicando a função a cada data frame na lista
df <- lapply(df, converter_colunas)

# Acessar o sexto data frame da lista
df_sexto <- df[[6]]

# Renomear a coluna específica
colnames(df_sexto)[colnames(df_sexto) == "Aprovação na 4ª série/5º Ano"] <- "Aprovaçãona 4ª série/5º Ano"

# Atualizar o sexto data frame na lista com a nova coluna
df[[6]] <- df_sexto

df_combinado <- do.call(rbind, df)

# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Aprovação no 1º Ano do Ensino Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df_combinado)[1:10] <- novos_nomes

df_combinado <- df_combinado[-1, ]

df_combinado[df_combinado == "--"] <- NA

df_combinado[, 10:ncol(df_combinado)] <- lapply(df_combinado[, 10:ncol(df_combinado)], 
                                                as.numeric)
df_combinado <- df_combinado[!is.na(df_combinado$CODESC), ]
write.csv(df_combinado, 
          "C:/Users/rosan/Downloads/rend_esc_2010.csv",
          row.names=FALSE)

############################################## 2011  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2011_2/tx_rendimento_escolas_2011.xls"
sheet_names <- excel_sheets(file_path)


# Função para ler uma aba a partir da 7ª linha
read_sheet_from_7th_line <- function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, skip = 7)
}

df <- lapply(sheet_names, read_sheet_from_7th_line)
converter_colunas <- function(df) {
  df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)
  return(df)
}

# Aplicando a função a cada data frame na lista
df <- lapply(df, converter_colunas)

df_combinado <- do.call(rbind, df)

# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df_combinado)[1:10] <- novos_nomes

df_combinado <- df_combinado[-1, ]

df_combinado[df_combinado == "--"] <- NA

df_combinado[, 10:ncol(df_combinado)] <- lapply(df_combinado[, 10:ncol(df_combinado)], 
                                                as.numeric)
df_combinado <- df_combinado[!is.na(df_combinado$CODESC), ]
length(unique(df_combinado$CODESC))

write.csv(df_combinado, 
          "C:/Users/rosan/Downloads/rend_esc_2011.csv",
          row.names=FALSE)

############################################## 2012  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2012/tx_rendimento_escolas_2012.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", 
                 "loc", "rede", "CODESC", "NOMESC", "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                                                as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2012.csv",
          row.names=FALSE)

############################################## 2013  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2013/TAXAS RENDIMENTOS ESCOLAS 2013.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "NOMESC", 
                 "CODESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2013.csv",
          row.names=FALSE)

############################################## 2014  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2014/TAXAS RENDIMENTOS ESCOLAS 2014.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "NOMESC", 
                 "CODESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2014.csv",
          row.names=FALSE)

############################################## 2015  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rendimento_escolas_2015/TAXA_REND_2015_ESCOLAS/TX_REND_ESCOLAS_2015.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2015.csv",
          row.names=FALSE)

############################################## 2016  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/TAXA_REND_2016_ESCOLAS/TAXA_REND_2016_ESCOLAS/TX_REND_ESCOLAS_2016.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2016.csv",
          row.names=FALSE)

############################################## 2017  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/TAXA_REND_2017_ESCOLAS/TAXA_REND_2017_ESCOLAS/TX_REND_ESCOLAS_2017.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2017.csv",
          row.names=FALSE)

############################################## 2018  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/TX_REND_ESCOLAS_2018/TX_REND_ESCOLAS_2018/TX_REND_ESCOLAS_2018.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2018.csv",
          row.names=FALSE)
############################################## 2019  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rend_escolas_2019/tx_rend_escolas_2019/tx_rend_escolas_2019.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2019.csv",
          row.names=FALSE)

############################################## 2020  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rend_escolas_2020/tx_rend_escolas_2020/tx_rend_escolas_2020.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2020.csv",
          row.names=FALSE)

############################################## 2021  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rend_escolas_2021/tx_rend_escolas_2021/tx_rend_escolas_2021.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2021.csv",
          row.names=FALSE)

############################################## 2022  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rend_escolas_2022/tx_rend_escolas_2022/tx_rend_escolas_2022.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2022.csv",
          row.names=FALSE)

############################################## 2023  ########################################
# Listar todas as abas (sheets) do arquivo
file_path <- "C:/Users/rosan/Downloads/tx_rend_escolas_2023/tx_rend_escolas_2023/tx_rend_escolas_2023.xlsx"


# Função para ler uma aba a partir da 7ª linha
df <- read_excel(file_path, skip = 7)

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], as.character)


# Criando um vetor com os novos nomes das colunas
novos_nomes <- c("Ano", "Região", "UF", "cod_mun", "nom_mun", "CODESC", 
                 "NOMESC","loc", "rede",
                 "Total Aprovação no Ens. Fundamental")

# Alterando os nomes das primeiras 10 colunas
colnames(df)[1:10] <- novos_nomes

df <- df[-1, ]

df[df == "--"] <- NA

df[, 10:ncol(df)] <- lapply(df[, 10:ncol(df)], 
                            as.numeric)

df <- df[!is.na(df$CODESC), ]
length(unique(df$CODESC))

write.csv(df, 
          "C:/Users/rosan/Downloads/rend_esc_2023.csv",
          row.names=FALSE)