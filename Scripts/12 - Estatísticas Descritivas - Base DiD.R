############################
# Estatísticas Descritivas #
############################
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)
library(tidyr)
library(viridis)
library(gridExtra)

da <- fread("Dados/Dados Tratados/base_final_br.csv")
dta_sp <- fread("Dados/Dados Tratados/base_final_sp.csv")
dta_sp_all <- fread("Dados/Dados Tratados/base_final_sp_todas.csv")
dta_sp_anos <- fread("Dados/Dados Tratados/base_final_sp_todososanos.csv")
dta_sp_inse <- fread("Dados/Dados Tratados/base_final_sp_inse6.csv")

# Treat Variable (Value 1 for every treated unit, 0 otherwise)
da <- da %>%
  mutate(treat = ifelse(rede %in% c("Particular", "Privada"), 0, 1))

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
    ggplot(aes(x = .data[[coluna_covariada]], fill = factor(treat, levels = c(1, 0), labels = c("Treated", "Control")))) + 
    geom_density(alpha = 0.5) +
    scale_fill_manual(
      values = c("Treated" = "blue", "Control" = "red"),  # Azul para Pública e Vermelho para Privada
      labels = c("Treated", "Control")
    ) +
    labs(
      #title = paste("Distribuição da covariada:", coluna_covariada),
      x = coluna_covariada,
      y = "Density",
      fill = "School Type"
    ) +
    # Adiciona as linhas verticais para as médias de cada grupo, sem legenda
    geom_vline(data = medias, 
               aes(xintercept = media, color = factor(treat, levels = c(1, 0), labels = c("Treated", "Control"))),
               linetype = "dashed", size = 1, show.legend = FALSE) +  # Remove a legenda para as linhas de média
    scale_color_manual(values = c("Treated" = "blue", "Control" = "red")) +  # Ajuste das cores
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # Remove as linhas principais do grid
      panel.grid.minor = element_blank()   # Remove as linhas menores do grid
    ) +
    scale_x_continuous(labels = label_number(scale = 1, suffix = "")) #+  # Formatação do eixo X
    #scale_y_continuous(labels = label_number(scale = 1, suffix = ""))    # Formatação do eixo Y
}

# Exemplo de uso
plot_distribuicao(da_capitais, "abandono_2009")

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
plot_codesc_por_ano(da_capitais)


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
      x = "Year",
      y = "Quantity"
    ) +
    theme_classic() +
    theme(axis.title = element_text(size = 10))
}

# Chamar a função passando a base de dados
plot_codesc_perdidos(da_filtrada2)


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

plot_codasc_by_treated(da2)

# Função para contar CODESC únicos e gerar tabela em LaTeX
gerar_tabela_latex <- function(data) {
  # Contar CODESC únicos para cada valor de treated
  contagem <- data %>%
    group_by(treated) %>%
    summarise(Unique_CODESC = n_distinct(CODESC)) %>%
    ungroup()
  
  # Gerar o código da tabela em LaTeX
  tabela_latex <- paste0(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\begin{tabular}{lc}\n",
    "\\hline\n",
    "Treated & Unique CODESC \\\\\n",
    "\\hline\n",
    paste(contagem[["treated"]], "&", contagem$Unique_CODESC, "\\\\\n", collapse = ""),
    "\\hline\n",
    "\\end{tabular}\n",
    "\\caption{Unique CODESC counts by treatment status}\n",
    "\\label{tab:unique_cod_treated}\n",
    "\\end{table}"
  )
  
  return(tabela_latex)
}

# Gerar a tabela LaTeX
codigo_latex <- gerar_tabela_latex(dta_sp_inse)

# Exibir o código
cat(codigo_latex)


# Função para contar CODESC únicos que aparecem apenas após 2015
contar_codesc_apenas_apos_2015 <- function(data) {
  # Filtrar CODESC que aparecem após 2015
  cod_apenas_apos_2015 <- data %>%
    group_by(CODESC) %>%
    summarise(min_ano = min(Ano)) %>%
    filter(min_ano >= 2015) %>%
    nrow()
  
  return(cod_apenas_apos_2015)
}

contar_codesc_apenas_apos_2015(dta_sp)

# Função para gerar o plot
plot_abandono <- function(data) {
  # Verifica se a base tem as colunas necessárias
  required_cols <- c("Ano", "treat", "abandono_tot_em", "abandono_3a_em")
  if (!all(required_cols %in% colnames(data))) {
    stop("A base deve conter as colunas: 'Ano', 'treat', 'abandono_tot_em', 'abandono_3a_em'.")
  }
  
  # Calcula a média de abandono por ano e tipo de escola
  df_plot <- data %>%
    group_by(Ano, treat) %>%
    summarise(
      mean_abandono_tot = mean(abandono_tot_em, na.rm = TRUE),
      mean_abandono_3a = mean(abandono_3a_em, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = starts_with("mean_abandono"), 
                 names_to = "variable", 
                 values_to = "mean") %>%
    mutate(
      variable = recode(variable, 
                        "mean_abandono_tot" = "Total Dropout Rate (EM)", 
                        "mean_abandono_3a" = "3rd Year Dropout Rate (EM)"),
      treat_label = ifelse(treat == 1, "Treated Schools", "Control Schools")
    )
  
  # Cria o plot
  ggplot(df_plot, aes(x = Ano, y = mean, color = treat_label, linetype = variable)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      x = "Year",
      y = "Average Dropout Rate (%)",
      color = "School Type",
      linetype = "Dropout Type",
      title = "Average Dropout Rates for Public and Private Schools"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Treated Schools" = "blue", "Control Schools" = "red"))
}

# Exemplo de uso
plot_abandono(da_filtrada4)
# RJ 3304557
# Curitiba 4106902
# Campinas 3509502
# Brasilia 5300108
# BH 3106200
# Fortaleza 2304400

plot_media_abandono <- function(dados) {
  
  # Calcula a média por ano e grupo de tratamento
  dados_media <- dados %>%
    group_by(Ano, treat) %>%
    summarise(media_abandono = mean(abandono_tot_em, na.rm = TRUE), .groups = "drop")
  
  # Cria o gráfico
  ggplot(dados_media, aes(x = Ano, y = media_abandono, color = factor(treat))) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = function(x) unique(round(seq(min(x), max(x), length.out = 10)))) +
    scale_color_viridis_d(
      option = "D",
      end = 0.8,
      labels = c("0" = "Control", "1" = "Treatment"),
      name = "Grupo"
    ) +
    labs(
         x = "Year",
         y = "Dropout Rate") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          legend.position = "bottom",
  plot.margin = margin(t = 5, r = 5, b = 20, l = 5, unit = "pt"),
  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
}

g1 <-plot_media_abandono(da_filtrada4)
g2 <- plot_media_abandono(da_capitais)
g1
# Organizar os gráficos lado a lado
grid.arrange(g1, g2, ncol = 2)
