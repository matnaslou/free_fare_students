variaveis <- c(
  # School Structure
  "IN_AREA_VERDE","IN_AUDITORIO","IN_BIBLIOTECA","IN_LABORATORIO_CIENCIAS",
  "IN_LABORATORIO_INFORMATICA","IN_PARQUE_INFANTIL",
  "IN_QUADRA_ESPORTES",
  "IN_DEPENDENCIAS_PNE","IN_LAVANDERIA",
  "IN_ESGOTO_REDE_PUBLICA","IN_BANDA_LARGA",
  "IN_ALIMENTACAO","TP_ATIVIDADE_COMPLEMENTAR",
  "IN_INTERNET","IN_BANHEIRO_EI","IN_INF_CRE","IN_BERCARIO",
  "IN_NOTURNO","QT_TUR_MED","QT_MAT_MED_INT","QT_MAT_MED",
  # Teachers Quality
  "QT_DOC_ESP","QT_DOC_INF_PRE","QT_DOC_INF","QT_DOC_MED",
  # Socio-economic information
  "inse",
  # Demography
  "QT_MAT_BAS_MASC","QT_MAT_BAS_BRANCA",
  "QT_MAT_BAS_PRETA","QT_MAT_BAS_PARDA","QT_MAT_BAS_AMARELA",
  # School Performance
  "NU_TAXA_PARTICIPACAO","NU_MEDIA_CN","NU_MEDIA_CH","NU_MEDIA_LP","NU_MEDIA_MT",
  "NU_MEDIA_RED","PC_FORMACAO_DOCENTE"
  
)

# Calcular o número de NAs por variável
n_ausentes <- sapply(variaveis, function(x) sum(is.na(dta_sp_filtrada2[[x]])))

# Ordenar e exibir as variáveis com mais valores ausentes
n_ausentes <- sort(n_ausentes, decreasing = TRUE)
n_ausentes