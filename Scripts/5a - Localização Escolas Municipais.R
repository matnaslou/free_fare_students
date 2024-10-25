# Carrega o pacote dplyr
library(dplyr)

# Define os anos
anos <- 2010:2023

# Inicializa uma lista para armazenar os data frames
lista_bases <- list()

# Loop para ler e processar cada arquivo CSV
for (ano in anos) {
  # Define o nome do arquivo
  nome_arquivo <- paste0("C:/Users/rosan/Downloads/escolas-dez-", ano, ".csv")
  
  # Lê o arquivo CSV
  base <- read.csv2(nome_arquivo)
  
  # Se o ano for 2018 ou posterior, converte os nomes das colunas para maiúsculas
  if (ano >= 2018) {
    names(base) <- toupper(names(base))
  }
  
  if (ano == 2015) {
    base$LATITUDE2 <- base$LATITUDE
    base$LATITUDE <- base$LONGITUDE
    base$LONGITUDE <- base$LATITUDE2
  }
  
  # Mantém apenas as colunas CODESC, LATITUDE e LONGITUDE
  base <- base %>% select(CODESC, LATITUDE, LONGITUDE)
  
  
  
  # Adiciona a coluna "Ano"
  base$Ano <- ano
  #base$LATITUDE <- gsub(",", ".", base$LATITUDE)
  #base$LONGITUDE <- gsub(",", ".", base$LONGITUDE)
  # Retira ponto e vírgula das informações
  base$LATITUDE <- gsub("[.,]", "", base$LATITUDE)
  base$LONGITUDE <- gsub("[.,]", "", base$LONGITUDE)
  # Transforma as informações em string
  base$LATITUDE <- as.character(base$LATITUDE)
  base$LONGITUDE <- as.character(base$LONGITUDE)
  # Adiciona a ponto depois do segundo dígito e torna numérico
  base$LATITUDE <- as.numeric(sub("(\\d{2})(\\d+)", "\\1.\\2", base$LATITUDE))
  base$LONGITUDE <- as.numeric(sub("(\\d{2})(\\d+)", "\\1.\\2", base$LONGITUDE))
  # Latitudes e Longitudes iguais a zero viram vazio
  base$LATITUDE[base$LATITUDE == 0] <- NA
  base$LONGITUDE[base$LONGITUDE == 0] <- NA
  # Latitude igual a Longitude vira vazio
  base$LATITUDE[base$LATITUDE == base$LONGITUDE] <- NA
  base$LONGITUDE[base$LONGITUDE == base$LATITUDE] <- NA
  # Corrigindo latitudes e longitudes positivas
  base$LATITUDE <- ifelse(base$LATITUDE > 0, 
                                      -base$LATITUDE, 
                                      base$LATITUDE)
  base$LONGITUDE <- ifelse(base$LONGITUDE > 0, 
                          -base$LONGITUDE, 
                          base$LONGITUDE)
  # Tornando vazio inconsistências de localização
  base$LATITUDE <- ifelse(base$LATITUDE < -23.99 | base$LATITUDE > -23, 
                          NA, 
                          base$LATITUDE)
  base$LONGITUDE <- ifelse(base$LONGITUDE < -46.89 | base$LONGITUDE > -46, 
                           NA, 
                           base$LONGITUDE)
  
  
  # Armazena o data frame na lista
  lista_bases[[as.character(ano)]] <- base
}

# Junta todas as bases em um único data frame, ignorando colunas ausentes em alguns arquivos
base_completa <- bind_rows(lista_bases)
# Quantidade de escolas na base completa
length(unique(base_completa$CODESC)) #8671

# Criar uma nova base com latitude e longitude ponderadas pelos anos
# Ideia é pegar valor mais consistente ao longo dos anos
nova_base <- base_completa %>%
  mutate(peso = Ano - min(Ano) + 1) %>%   # Cria uma coluna de peso, dando mais valor aos anos mais recentes
  group_by(CODESC, LATITUDE, LONGITUDE) %>%   # Agrupa por escola, latitude e longitude
  summarise(frequencia_ponderada = sum(peso), .groups = "drop") %>%   # Soma os pesos para obter a "frequência ponderada"
  group_by(CODESC) %>%   # Agrupa por escola para encontrar a latitude e longitude mais frequente para cada escola
  slice(which.max(frequencia_ponderada)) %>%   # Seleciona a linha com maior frequência ponderada para cada escola
  ungroup() %>%  # Desfaz o agrupamento
  select(CODESC, LATITUDE, LONGITUDE)  # Mantém apenas as colunas desejadas

# Quantidade de escolas na base consistente (para ver se perde alguém no processo)
length(unique(nova_base$CODESC)) #8671

write.csv(nova_base, "C:/Users/rosan/Downloads/loc_esc-mun.csv", row.names = FALSE)
