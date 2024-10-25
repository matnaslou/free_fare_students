obter_nomes_colunas <- function(df) {
  names(df)
}

# Obter os nomes das colunas para todos os data frames na lista
nomes_colunas_lista <- lapply(df, obter_nomes_colunas)

# Identificar as colunas únicas presentes em cada data frame
colunas_unicas <- lapply(nomes_colunas_lista, unique)

# Comparar as colunas entre os data frames
comparar_colunas <- function(colunas_lista) {
  comparacoes <- list()
  for (i in seq_along(colunas_lista)) {
    for (j in seq_along(colunas_lista)) {
      if (i != j) {
        diferencas <- setdiff(colunas_lista[[i]], colunas_lista[[j]])
        if (length(diferencas) > 0) {
          comparacoes[[paste("DF", i, "vs DF", j)]] <- diferencas
        }
      }
    }
  }
  return(comparacoes)
}

# Realizar a comparação
resultado_comparacao <- comparar_colunas(nomes_colunas_lista)

# Exibir o resultado da comparação
resultado_comparacao