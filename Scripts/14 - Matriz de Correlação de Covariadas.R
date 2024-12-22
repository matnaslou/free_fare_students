############################################
#########   Matriz de Correlação   #########
############################################
# Escolha da Base (Nacional ou São Paulo)
base <- "da_capitais"
# Escolha do nível de correlação mínima
corr <- 0.02
# Selecionar apenas as variáveis numéricas para a matriz de correlação
variaveis_numericas <- get(base) %>% select(where(is.numeric))
variaveis_numericas <- variaveis_numericas[, colSums(is.na(variaveis_numericas)) < nrow(variaveis_numericas)]
variaveis_numericas <- variaveis_numericas[rowSums(is.na(variaveis_numericas)) < ncol(variaveis_numericas), ]


var2 <- "treat"

# Calcular a matriz de correlação
matriz_correlacao <- cor(variaveis_numericas, use = "pairwise.complete.obs")

# Extrair apenas a coluna de correlação com TP_DEPENDENCIA
cor_tipo_escola <- matriz_correlacao[, var2, drop = FALSE]

# Convertendo para dataframe
cor_tipo_escola_df <- as.data.frame(cor_tipo_escola)
cor_tipo_escola_df <- cor_tipo_escola_df %>% filter(row.names(.) != var2)

# Adiciona uma coluna com os nomes das variáveis para facilitar o gráfico
cor_tipo_escola_df$Variavel <- row.names(cor_tipo_escola_df)

# Filtrar as correlações para valores >= 0.3 ou <= -0.3
cor_tipo_escola_df_filtrado <- cor_tipo_escola_df %>%
  filter(get(var2) >= -corr & get(var2) <= corr)

# Cria o gráfico de barras colorido para visualização das correlações
ggplot(cor_tipo_escola_df_filtrado, aes(x = reorder(Variavel, get(var2)), y = get(var2))) +
  geom_bar(stat = "identity", aes(fill = get(var2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlação de tipo_escola com outras variáveis",
       x = "Variável",
       y = "Correlação") +
  theme_minimal() +
  coord_flip() # Gira o gráfico para facilitar a leitura das labels
