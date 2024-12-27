filter_non_perfect_predictors <- function(data, treat_col) {
  # Identifica as colunas numéricas ou fatoriais (ignora `treat_col`)
  cols_to_check <- setdiff(names(data), treat_col)
  
  # Inicializa vetor para armazenar colunas válidas
  valid_cols <- sapply(cols_to_check, function(col) {
    # Verifica os valores únicos da variável e da coluna treat
    # Cria uma tabela cruzada entre a variável e a coluna treat
    cross_tab <- table(data[[col]], data[[treat_col]])
    
    # Para a coluna ser válida, mais de um valor de treat deve estar associado a pelo menos um valor da variável
    any(rowSums(cross_tab > 0) > 1)
  })
  
  # Obtém apenas os nomes das colunas válidas
  valid_col_names <- names(valid_cols[valid_cols])
  
  # Retorna um data frame com as colunas válidas e a coluna treat
  return(data[, c(treat_col, valid_col_names), drop = FALSE])
}

da_cov <- filter_non_perfect_predictors(da_capitais,"treat")