# Função para realizar o teste de médias para múltiplas colunas
test_medianas_covariadas_multiplas <- function(base_dados, colunas_covariadas) {
  
  # Criar uma lista para armazenar os resultados
  resultados <- list()
  
  # Iterar sobre cada coluna de covariada
  for (coluna_covariada in colunas_covariadas) {
    
    # Separar os dados em dois grupos
    grupo_1 <- base_dados %>% filter(treat == 1) %>% pull(.data[[coluna_covariada]])
    grupo_0 <- base_dados %>% filter(treat == 0) %>% pull(.data[[coluna_covariada]])
    
    # Verificar normalidade com o teste de Shapiro-Wilk
    shapiro_1 <- shapiro.test(grupo_1)
    shapiro_0 <- shapiro.test(grupo_0)
    
    # Se ambos os grupos forem normalmente distribuídos, fazer o teste t de Student
    if (shapiro_1$p.value > 0.05 && shapiro_0$p.value > 0.05) {
      cat("Para a variável", coluna_covariada, ": Dados são aproximadamente normais. Realizando o teste t de Student.\n")
      resultado <- t.test(grupo_1, grupo_0)
    } else {
      cat("Para a variável", coluna_covariada, ": Pelo menos um dos grupos não segue distribuição normal. Realizando o teste de Mann-Whitney.\n")
      resultado <- wilcox.test(grupo_1, grupo_0)
    }
    
    # Armazenar o resultado para a coluna atual
    resultados[[coluna_covariada]] <- resultado
  }
  
  return(resultados)
}

# Exemplo de uso: realizar o teste para várias colunas de covariadas
colunas_covariadas <- c("P001", "IN_NOTURNO","IN_DESPENSA","IN_FUND_AI",
                        "QT_MAT_BAS_PRETA","QT_MAT_BAS_15_17","QT_MAT_BAS_N",
                        "QT_DOC_ESP","IN_BIBLIOTECA",
                        "IN_PARQUE_INFANTIL","QT_TUR_INF_PRE","freq_mat_bas_branca")  # Substitua pelos nomes das suas colunas
colunas_covariadas <- selected_variables
resultados <- test_medianas_covariadas_multiplas(da_cov_2014, colunas_covariadas)

# Imprimir os resultados
print(resultados)
