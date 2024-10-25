library(dplyr)
library(readr)
anos <- 2007:2023
arquivos <- paste0("C:/Users/rosan/Downloads/rend_esc_", anos, ".csv")
lista_bases <- lapply(arquivos, read_csv)

# Combinar todos os data frames em um único
base_completa <- bind_rows(lista_bases)

#enem_por_escola <- read_csv2("C:/Users/rosan/Downloads/microdados_enem_por_escola/DADOS/MICRODADOS_ENEM_ESCOLA.csv")
#enem_por_escola <- enem_por_escola %>% 
#  rename("Ano" = "NU_ANO",
#        "CODESC" = "CO_ESCOLA_EDUCACENSO")

#base_completa <- left_join(base_completa, enem_por_escola, by = c("CODESC", "Ano"))

# Salvando o dataframe 
write.csv(base_completa, "C:/Users/rosan/Downloads/rend_esc.csv", row.names = FALSE)
