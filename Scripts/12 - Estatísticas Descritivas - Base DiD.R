############################
# Estatísticas Descritivas #
############################
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(scales)

# Função para criar o gráfico
plot_distribuicao <- function(base_dados, coluna_covariada) {
  # Calcula as médias para cada grupo (Público e Privado)
  medias <- base_dados %>%
    filter(Ano == 2014) %>%
    group_by(treat) %>%
    summarise(media = mean(.data[[coluna_covariada]], na.rm = TRUE))
  
  # Cria o gráfico
  base_dados %>%
    filter(Ano == 2014) %>%
    ggplot(aes(x = .data[[coluna_covariada]], fill = factor(treat, levels = c(1, 0), labels = c("Pública", "Privada")))) + 
    geom_density(alpha = 0.5) +
    scale_fill_manual(
      values = c("Pública" = "blue", "Privada" = "red"),  # Azul para Pública e Vermelho para Privada
      labels = c("Pública", "Privada")
    ) +
    labs(
      title = paste("Distribuição da covariada:", coluna_covariada),
      x = coluna_covariada,
      y = "Densidade",
      fill = "Tipo de Escola"
    ) +
    # Adiciona as linhas verticais para as médias de cada grupo, sem legenda
    geom_vline(data = medias, 
               aes(xintercept = media, color = factor(treat, levels = c(1, 0), labels = c("Pública", "Privada"))),
               linetype = "dashed", size = 1, show.legend = FALSE) +  # Remove a legenda para as linhas de média
    scale_color_manual(values = c("Pública" = "blue", "Privada" = "red")) +  # Ajuste das cores
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # Remove as linhas principais do grid
      panel.grid.minor = element_blank()   # Remove as linhas menores do grid
    ) +
    scale_x_continuous(labels = label_number(scale = 1, suffix = "")) +  # Formatação do eixo X
    scale_y_continuous(labels = label_number(scale = 1, suffix = ""))    # Formatação do eixo Y
}

# Exemplo de uso
plot_distribuicao(dta_sp_filtrada4, "sem_enem")

# Função para criar o gráfico
plot_codesc_por_ano <- function(base_dados) {
  base_dados %>%
    group_by(Ano) %>%
    summarise(quantidade_codesc = n_distinct(CODESC)) %>%
    ggplot(aes(x = Ano, y = quantidade_codesc)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(
      title = "Quantidade de CODESC Únicos por Ano",
      x = "Ano",
      y = "Quantidade de CODESC Únicos"
    ) +
    theme_minimal()
}

# Chamar a função passando a base de dados
plot_codesc_por_ano(dta_sp_filtrada4)


# Função para calcular a perda de CODESC únicos entre os anos
plot_codesc_perdidos <- function(base_dados) {
  # Criar um dataframe com os CODESC únicos por Ano
  codesc_por_ano <- base_dados %>%
    group_by(Ano) %>%
    summarise(codesc_unicos = list(unique(CODESC)))
  
  # Calcular a perda de CODESC entre os anos consecutivos
  perdas <- data.frame(Ano = numeric(), perdidos = integer()) # DataFrame vazio para armazenar resultados
  
  for (i in 2:nrow(codesc_por_ano)) {
    codesc_ano_anterior <- codesc_por_ano$codesc_unicos[[i-1]]
    codesc_ano_atual <- codesc_por_ano$codesc_unicos[[i]]
    
    # Calcular os CODESC perdidos
    perdidos <- setdiff(codesc_ano_anterior, codesc_ano_atual)
    
    perdas <- rbind(perdas, data.frame(Ano = codesc_por_ano$Ano[i], perdidos = length(perdidos)))
  }
  
  # Criar o gráfico de barras
  ggplot(perdas, aes(x = Ano, y = perdidos)) +
    geom_bar(stat = "identity", fill = "salmon", color = "black") +
    labs(
      title = "Quantidade de CODESC Perdidos de um Ano para o Seguinte",
      x = "Ano",
      y = "Quantidade de CODESC Perdidos"
    ) +
    theme_minimal()
}

# Chamar a função passando a base de dados
plot_codesc_perdidos(dta_sp_filtrada)


plot_codasc_by_treated <- function(data) {
  # Verificando se as colunas necessárias estão presentes
  if (!all(c("CODESC", "Ano", "treated") %in% names(data))) {
    stop("O data frame deve conter as colunas: 'CODESC', 'Ano' e 'treated'.")
  }
  
  # Agrupando os dados
  summarized_data <- data %>%
    group_by(Ano, treated) %>%
    summarise(unique_codasc = n_distinct(CODESC), .groups = "drop")
  
  # Criando o plot
  ggplot(summarized_data, aes(x = Ano, y = unique_codasc, fill = factor(treated))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("0" = "darkblue", "1" = "red"), 
                      name = "Treated",
                      labels = c("0 (Não tratado)", "1 (Tratado)")) +
    labs(
      title = "Quantidade de CODESC únicos por Ano",
      x = "Ano",
      y = "Quantidade de CODESC únicos"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

plot_codasc_by_treated(dta_sp_filtrada3)
