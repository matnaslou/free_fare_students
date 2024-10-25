# Carrega o pacote dplyr
library(dplyr)


# Define os anos
anos <- 2018:2023

# Inicializa uma lista para armazenar os data frames
lista_bases <- list()

# Loop para ler e processar cada arquivo CSV
for (ano in anos) {
  # Define o nome do arquivo
  nome_arquivo <- paste0("C:/Users/rosan/Downloads/ENDERECO_ESCOLAS_", ano, "_0", ".csv")
  
  # Lê o arquivo CSV usando read.csv2 (assumindo que usa ; como separador)
  base <- read.csv2(nome_arquivo)
  
  # Se o ano for 2018 ou posterior, converte os nomes das colunas para maiúsculas
  if (ano >= 2019) {
    base$LATITUDE <- base$DS_LATITUDE
    base$LONGITUDE <- base$DS_LONGITUDE
  }
  
  # Mantém apenas as colunas CODESC, LATITUDE e LONGITUDE
  base$CODESC <- base$COD_ESC
  base <- base %>% select(CODESC, LATITUDE, LONGITUDE)
  
  # Adiciona a coluna "Ano"
  base$Ano <- ano
  # Substitui as vírgulas por pontos na coluna LATITUDE
  base$LATITUDE <- gsub(",", ".", base$LATITUDE)
  base$LONGITUDE <- gsub(",", ".", base$LONGITUDE)
  base$LATITUDE <- as.numeric(base$LATITUDE)
  base$LONGITUDE <- as.numeric(base$LONGITUDE)
  # Armazena o data frame na lista
  lista_bases[[as.character(ano)]] <- base
}  

# Junta todas as bases em um único data frame, ignorando colunas ausentes em alguns arquivos
base_completa <- bind_rows(lista_bases)

# Extrai as informações da base para o ano de 2010
base_2018 <- base_completa %>% filter(Ano == 2018) %>% select(-Ano)

# Calcula o número de linhas da base de 2010
num_linhas_2018 <- nrow(base_2018)

# Cria linhas para os anos de 2007, 2008 e 2009 com as mesmas informações da base de 2010
base_anteriores <- base_2018 %>%
  slice(rep(1:n(), times = 11)) %>%
  mutate(Ano = rep(c(2007,2008,2009,2010,2011,2012,2013,2014,
                     2015,2016,2017), each = num_linhas_2018)) %>%
  arrange(Ano)

# Junta as linhas adicionais com a base completa
base_completa <- bind_rows(base_completa, base_anteriores)

# Checagem de quantas estão sem Latitude e/ou Longitude
base_completa %>%
  filter(LONGITUDE == 0) %>%
  distinct(CODESC) %>%
  nrow()
# Contagem de Escolas Únicas
length(unique(base_completa$CODESC))

# Remove observações onde LATITUDE ou LONGITUDE são iguais a 0
base_completa <- base_completa %>%
  filter(LATITUDE != 0 & LONGITUDE != 0)
