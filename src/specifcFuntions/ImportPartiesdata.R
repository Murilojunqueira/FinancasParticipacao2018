
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Create tables Partidos "Parties".

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)
# Most of the data come from CEPESP API: https://github.com/Cepesp-Fgv/cepesp-r 

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-03-09

############### Create "Partidos" table ###############

# Debug:
# CandidatoAno <- fread("data/dataset/CandidatoAno.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)

ImportPartiesdata <- function(CandidatoAno) {
  
  # Adiciona o ano de 1996
  Partidos <- CandidatoAno  %>% 
    dplyr::filter(Partido_Sigla != "") %>% 
    # Extrai o número do partido
    mutate(Partido_Numero = substr(as.character(CandAno_Numero), 1, 2)) %>%
    # Seleciona colunas relevantes.
    select(Partido_Sigla, Partido_Numero, CandAno_Ano) %>% 
    # Corrige o caso do PRONA
    mutate(Partido_Sigla = ifelse(Partido_Numero == 56, "PRONA", Partido_Sigla)) %>% 
    # Cria uma coluna identificadora do partido (numero_sigla sem espaço).
    mutate(Partido_Id = paste0(Partido_Numero, "_", trimws(Partido_Sigla) )) %>% 
    # Coloca a coluna identificadora em primeiro lugar
    select(Partido_Id, everything()) %>% 
    # Cria o banco final, colocando a primeira e última vez que o partido aparece no banco.
    # remove repetições
    arrange(CandAno_Ano) %>% 
    distinct(Partido_Numero, Partido_Sigla, CandAno_Ano, .keep_all = TRUE) %>% 
    # Garante que o ano da eleição é inteiro (integer).
    mutate(CandAno_Ano = as.integer(CandAno_Ano)) %>% 
    # Agrupa os dados por partido
    group_by(Partido_Id) %>% 
    # Coloca os anos onde o partido surgiu pela primeira e última vez.
    mutate(Partido_AnoInicial = min(CandAno_Ano)) %>% 
    mutate(Partido_AnoFinal = max(CandAno_Ano)) %>% 
    # Retira a variável ano.
    select(-CandAno_Ano) %>%
    # Retira o agrupamento.
    ungroup() %>% 
    # Faz com que cada partido apareça uma única vez.
    distinct(Partido_Id, .keep_all = TRUE) %>% 
    # Ordena os dados por número do partido
    arrange(Partido_Numero)
  
  return(Partidos)
  
}

# End