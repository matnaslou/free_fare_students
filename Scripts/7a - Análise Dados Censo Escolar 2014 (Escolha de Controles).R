# Reading and database treatment
library(dplyr)
library(data.table)
library(ggplot2)

###################################################
#################   LEITURA   #####################
###################################################
# Reading Scholar Census Data
censo <- fread("Dados/Dados Censo/microdados_ed_basica_2014/dados/microdados_ed_basica_2014.csv")

censo$tipo_escola <- factor(censo$TP_DEPENDENCIA, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("Federal", "Estadual", "Municipal", "Privada"))


###################################################
#################   VARIÁVEIS   ###################
###################################################

# Criar a variável binária 'escola_privada'
censo$escola_privada <- ifelse(censo$TP_DEPENDENCIA == 4, 1, 0)

censo$esc_priv_fac <- factor(censo$escola_privada,
                             levels = c(0,1),
                             labels = c("Pública","Privada"))

censo$escola_federal <- ifelse(censo$TP_DEPENDENCIA == 1, 1, 0)

censo$esc_fed_fac <- factor(censo$escola_federal,
                             levels = c(0,1),
                             labels = c("Outra","Federal"))


censo$escola_estadual <- ifelse(censo$TP_DEPENDENCIA == 2, 1, 0)

censo$esc_est_fac <- factor(censo$escola_estadual,
                            levels = c(0,1),
                            labels = c("Outra","Estadual"))

censo$escola_municipal <- ifelse(censo$TP_DEPENDENCIA == 3, 1, 0)

censo$esc_mun_fac <- factor(censo$escola_municipal,
                            levels = c(0,1),
                            labels = c("Outra","Municipal"))

censo$saopaulo <- ifelse(censo$CO_MUNICIPIO == 3550308, 1, 0) # 6995 escolas de São Paulo

# Base filtrando apenas São Paulo
censosp <- censo[censo$saopaulo == 1,]
censospem <- censosp[censosp$IN_MED == 1,]
############################################
#########   Matriz de Correlação   #########
############################################
# Escolha da Base (Nacional ou São Paulo)
base <- "censospem"
# Escolha do nível de correlação mínima
corr <- 0.3
# Selecionar apenas as variáveis numéricas para a matriz de correlação
variaveis_numericas <- get(base) %>% select(where(is.numeric))

var2 <- "escola_privada"

# Calcular a matriz de correlação
matriz_correlacao <- cor(variaveis_numericas, use = "complete.obs")

# Extrair apenas a coluna de correlação com TP_DEPENDENCIA
cor_tipo_escola <- matriz_correlacao[, var2, drop = FALSE]

# Convertendo para dataframe
cor_tipo_escola_df <- as.data.frame(cor_tipo_escola)
cor_tipo_escola_df <- cor_tipo_escola_df %>% filter(row.names(.) != var2)

# Adiciona uma coluna com os nomes das variáveis para facilitar o gráfico
cor_tipo_escola_df$Variavel <- row.names(cor_tipo_escola_df)

# Filtrar as correlações para valores >= 0.3 ou <= -0.3
cor_tipo_escola_df_filtrado <- cor_tipo_escola_df %>%
  filter(get(var2) <= -corr | get(var2) >= corr)

# Cria o gráfico de barras colorido para visualização das correlações
ggplot(cor_tipo_escola_df_filtrado, aes(x = reorder(Variavel, get(var2)), y = get(var2))) +
  geom_bar(stat = "identity", aes(fill = get(var2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlação de tipo_escola com outras variáveis",
       x = "Variável",
       y = "Correlação") +
  theme_minimal() +
  coord_flip() # Gira o gráfico para facilitar a leitura das labels

###############################################
########   VISUALIZAÇÃO DE VARIÁVEIS   ########
###############################################
# Escolha a Variável
var <- "QT_TUR_MED"
# Gerar o gráfico de densidade para cada tipo de escola (Pública ou Privada)
ggplot(get(base), aes(x = get(var), color = esc_priv_fac)) +
  geom_density(fill= NA, size = 1) + # alpha ajusta a transparência para sobreposição
  labs(title = paste0("Densidade de ", var, " por Tipo de Escola"),
       x = paste0(var),
       y = "Densidade") +
  scale_color_manual(values = c("Pública" = "blue", "Privada" = "red")) +
  theme_minimal()

# Gerar o gráfico de densidade para cada tipo de escola (Repartido por Tipo)
ggplot(get(base), aes(x = get(var), color = tipo_escola)) +
  geom_density(fill= NA, size = 1) + # alpha ajusta a transparência para sobreposição
  labs(title = paste0("Densidade de ", var, " por Tipo de Escola"),
       x = paste0(var),
       y = "Densidade") +
  scale_color_manual(values = c("Federal" = "blue", "Estadual" = "green", 
                                "Municipal" = "orange", "Privada" = "red")) +
  theme_minimal()


############################################
#########   ESCOLHA DE CONTROLES   #########
############################################
# Construção de Variáveis Possíveis Controles
## Tem Laboratório?


