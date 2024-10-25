# Define o caminho para a pasta de Downloads
caminho_pasta <- "C:/Users/rosan/Downloads/"

# Define os anos que serão lidos
anos <- 2007:2023

# Cria um data frame para armazenar os nomes das colunas de cada ano
df_nomes_colunas <- data.frame(Ano = anos)

# Loop para ler cada arquivo e armazenar os nomes das colunas em uma linha
for (ano in anos) {
  # Define o nome completo do arquivo
  arquivo <- paste0(caminho_pasta, "rend_esc_", ano, ".csv")
  
  # Lê o arquivo (apenas os nomes das colunas)
  colunas <- colnames(read.csv(arquivo, nrows = 1))
  
  # Adiciona as colunas ao data frame
  df_nomes_colunas[as.character(ano), 2:(length(colunas) + 1)] <- colunas
}

# Salva o resultado em um arquivo CSV na pasta de Downloads
write.csv(df_nomes_colunas, paste0(caminho_pasta, "nomes_colunas.csv"), row.names = FALSE, na = "")
