library(glmnet)

da_cov_2014 <- dta_sp_filtrada4 %>%
  filter(Ano == 2014)

# Suponha que sua base de dados seja chamada 'dados'
da_cov <- da_cov_2014 %>% 
  select(-treated,-first.treat,-did,-time,-CO_CEP,-treat,
         -TP_CATEGORIA_ESCOLA_PRIVADA,-IN_MANT_ESCOLA_PRIVADA_EMP,
         -IN_MANT_ESCOLA_PRIVADA_ONG,-IN_MANT_ESCOLA_PRIVADA_OSCIP,        
         -IN_MANT_ESCOLA_PRIV_ONG_OSCIP,-IN_MANT_ESCOLA_PRIVADA_SIND,        
         -IN_MANT_ESCOLA_PRIVADA_SIST_S,-IN_MANT_ESCOLA_PRIVADA_S_FINS,
         -IN_ALIMENTACAO,-IN_NOTURNO,-IN_INF,-IN_INF_PRE,
         -TP_LOCALIZACAO_DIFERENCIADA,-IN_CONVENIADA_PP,-TP_CONVENIO_PODER_PUBLICO,
         -IN_LOCAL_FUNC_PREDIO_ESCOLAR,IN_AGUA_FILTRADA,-IN_BANHEIRO_EI,
         -IN_EDUCACAO_INDIGENA,-TP_INDIGENA_LINGUA,-TP_AEE,-QT_TUR_INF_PRE,
         -IN_LOCAL_FUNC_OUTROS,-IN_SALA_ATENDIMENTO_ESPECIAL,-CO_LINGUA_INDIGENA_1,
         -QT_DOC_INF_PRE,-QT_TUR_INF)


# Remover colunas do tipo "character"
da_cov <- da_cov %>% 
  select(where(~ !is.character(.)))

# Remover todas as colunas até (e incluindo) 'TP_DEPENDENCIA'
da_cov <- da_cov %>% 
  select(-(1:which(names(da_cov) == "TP_DEPENDENCIA")))

# Remover colunas com valores missing (NA)
da_cov <- da_cov %>%
  select(where(~ !any(is.na(.))))

# Independent variables
X <- makeX(da_cov, na.impute = FALSE)

# Dependent variable
y <- da_cov_2014 %>% 
  pull(treated)

# Determine lambda values 
lambda_seq <- 10^seq(200, 100, length=100)

# Building the Lasso Regression model
lasso_model <- glmnet(X, y, alpha=1, lambda=lambda_seq)

# Selecting a lambda value with cross validation
cv_fit <- cv.glmnet(X, y, alpha=1, family = "binomial")
best_lambda <- cv_fit$lambda.min

# Building final model
final_model <- glmnet(X, y, alpha=1, lambda=0.25, family = "binomial")

# Getting coefficients 
coef(final_model, s=best_lambda)

# Obter os coeficientes do modelo Lasso
coef_final <- coef(final_model, s = best_lambda)

# A função coef() retorna um objeto "sparseMatrix" com coeficientes
# Convertê-lo em um vetor para facilitar a manipulação
coef_values <- as.vector(coef_final)

# Obter os nomes das variáveis (coeficientes diferentes de zero)
selected_variables <- rownames(coef_final)[coef_values != 0]

# Remover o intercepto, se presente
selected_variables <- selected_variables[selected_variables != "(Intercept)"]

# Remover Problemáticas
selected_variables <- selected_variables[selected_variables != "freq_mat_bas_preta"]
selected_variables <- selected_variables[selected_variables != "freq_mat_bas_parda"]

# Exibir as variáveis selecionadas
selected_variables
#selected_variables <- selected_variables[selected_variables != "TP_LOCALIZACAO_DIFERENCIADA"]
#selected_variables <- selected_variables[selected_variables != "IN_CONVENIADA_PP"]
#selected_variables <- selected_variables[selected_variables != "TP_CONVENIO_PODER_PUBLICO"]
#selected_variables <- selected_variables[selected_variables != "IN_LOCAL_FUNC_PREDIO_ESCOLAR"]
#selected_variables <- selected_variables[selected_variables != "IN_AGUA_FILTRADA"]
#selected_variables <- selected_variables[selected_variables != "IN_BANHEIRO_EI"]
#selected_variables <- selected_variables[selected_variables != "IN_EDUCACAO_INDIGENA"]
#selected_variables <- selected_variables[selected_variables != "TP_INDIGENA_LINGUA"]
#selected_variables <- selected_variables[selected_variables != "TP_AEE"]
#selected_variables <- selected_variables[selected_variables != "QT_TUR_INF_PRE"]
#selected_variables <- selected_variables[selected_variables != "IN_LOCAL_FUNC_OUTROS"]
#selected_variables <- selected_variables[selected_variables != "IN_SALA_ATENDIMENTO_ESPECIAL"]
#selected_variables <- selected_variables[selected_variables != "CO_LINGUA_INDIGENA_1"]
#selected_variables <- selected_variables[selected_variables != "QT_DOC_INF_PRE"]
#selected_variables <- selected_variables[selected_variables != "QT_TUR_INF"]
