
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import electoral data of 2000 election

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-03-09

# Dependencies:



############### Importing Electoral Data 2000 ###############


# Debug:
# InputFolder = "data/raw/CepespData/"
# setwd(rstudioapi::getActiveProject())




ElectoralData2000 <- function(InputFolder) {
  
  
  # Because the Cepesp API records information on your computer, in order to expedite 
  # extraction repeatability, it is important to select InputFolder as the 
  # working directory. Make sure this directory is suitable for receiving new 
  # information (a dedicated Dropbox directory, for example).
  OldWd <- getwd() # Save old working directory to relocate later
  setwd(InputFolder)
  # getwd()
  
  
  # Se os dados acima não estiverem funcionando, usar o arquivo extraido do site do Cepesp.
  DadosRaw.CandVer <- fread(paste0(InputFolder, "CANDIDATOS_VEREADOR_2000.csv"),
                            stringsAsFactors = FALSE, encoding = "UTF-8")
  
  # Libera memória.
  rm(filter_index)
  
  # Testa para ver se os dados estão OK.
  class(DadosRaw.CandVer)
  dim(DadosRaw.CandVer)
  names(DadosRaw.CandVer)
  
  
  # Seleciona e formata os dados.
  DadosCandidatos.Vereador.Ano <- DadosRaw.CandVer %>% 
    # Seleciona as colunas relevantes.
    select(SIGLA_UE, NUM_TURNO, ANO_ELEICAO, NUMERO_CANDIDATO, NOME_CANDIDATO, 
           SIGLA_PARTIDO, NUMERO_PARTIDO, CODIGO_LEGENDA, COD_SIT_TOT_TURNO) %>% 
    # Resolve problemas de classe
    mutate(COD_SIT_TOT_TURNO = CharaterToInteger(COD_SIT_TOT_TURNO)) %>% 
    # Retira candidatos sem situação eleitoral devido a impugnação
    filter(!is.na(COD_SIT_TOT_TURNO)) %>% 
    # Retira casos de candidatos repetidos (não sei porque houve isso)
    arrange(SIGLA_UE, NUMERO_CANDIDATO) %>% 
    distinct(SIGLA_UE, NUMERO_CANDIDATO, NUM_TURNO, .keep_all = TRUE) %>% 
    # Garante que o CODIGO_LEGENDA (identificação da coligação) esteja em um 
    # formato amigável, retirando a notação científica.
    mutate(CODIGO_LEGENDA = format(CODIGO_LEGENDA, scientific = FALSE)) %>% 
    mutate(CODIGO_LEGENDA = trimws(CODIGO_LEGENDA))
  
  # Testa para ver se os dados estão OK.
  head(DadosCandidatos.Vereador.Ano)
  names(DadosCandidatos.Vereador.Ano)
  dim(DadosCandidatos.Vereador.Ano)
  # View(DadosCandidatos.Vereador.Ano)
  
  # Retira os dados brutos (libera memória).
  rm(DadosRaw.CandVer)
  
  # Baixa os dados de votação (esses estão funcionando no API CepespData)
  DadosRaw.VoteVer <- get_votes(year = 2000, position = "Councillor", 
                                regional_aggregation = "Municipality", cached=TRUE)
  rm(filter_index)
  
  # Testa para ver se os dados estão OK.
  names(DadosRaw.VoteVer)
  head(DadosRaw.VoteVer)
  
  # Mescla os dados de candidatos e votação.
  DadosEleicao.Vereador.Ano <- DadosRaw.VoteVer %>% 
    # Seleciona as colunas relevantes.
    select(SIGLA_UE, COD_MUN_IBGE, ANO_ELEICAO, DESCRICAO_CARGO, NUM_TURNO, 
           NUMERO_CANDIDATO, QTDE_VOTOS) %>% 
    # Insere dados sobre os candidatos.
    left_join(DadosCandidatos.Vereador.Ano, by = c("SIGLA_UE", "NUM_TURNO", "ANO_ELEICAO",
                                                   "NUMERO_CANDIDATO")) %>% 
    # Trata os votos em legenda.
    arrange(COD_MUN_IBGE, ANO_ELEICAO, as.character(NUMERO_CANDIDATO)) %>% 
    mutate(COD_SIT_TOT_TURNO = as.integer(COD_SIT_TOT_TURNO)) %>% 
    mutate(COD_SIT_TOT_TURNO = ifelse(nchar(as.character(NUMERO_CANDIDATO)) == 2, -5, COD_SIT_TOT_TURNO))  %>% 
    mutate(NUMERO_PARTIDO = ifelse(nchar(as.character(NUMERO_CANDIDATO)) == 2, NUMERO_CANDIDATO, NUMERO_PARTIDO)) %>% 
    mutate(CODIGO_LEGENDA = ifelse(nchar(as.character(NUMERO_CANDIDATO)) == 2 & NUMERO_PARTIDO == lead(NUMERO_PARTIDO), 
                                   lead(CODIGO_LEGENDA), CODIGO_LEGENDA)) %>% 
    # Garante que a Sigla do partido não é factor
    mutate(SIGLA_PARTIDO = as.character(SIGLA_PARTIDO)) %>% 
    mutate(SIGLA_PARTIDO = ifelse(nchar(as.character(NUMERO_CANDIDATO)) == 2 & NUMERO_PARTIDO == lead(NUMERO_PARTIDO), 
                                  lead(SIGLA_PARTIDO), SIGLA_PARTIDO)) %>% 
    # Trata Brancos e Nulos
    mutate(COD_SIT_TOT_TURNO = ifelse(NUMERO_CANDIDATO == 95, -6, COD_SIT_TOT_TURNO)) %>% 
    mutate(COD_SIT_TOT_TURNO = ifelse(NUMERO_CANDIDATO == 96, -7, COD_SIT_TOT_TURNO)) %>% 
    # Ordena as variáveis.
    select(COD_MUN_IBGE, ANO_ELEICAO, DESCRICAO_CARGO, NUM_TURNO, NUMERO_CANDIDATO, 
           NOME_CANDIDATO, SIGLA_PARTIDO, CODIGO_LEGENDA, QTDE_VOTOS, COD_SIT_TOT_TURNO)
  
  
  # Libera Memória.
  rm(DadosCandidatos.Vereador.Ano)
  rm(DadosRaw.VoteVer)
  
  # Corrige nomes.
  names(DadosEleicao.Vereador.Ano) <- c("Munic_Id",
                                        "CandAno_Ano",
                                        "CandAno_Cargo",
                                        "CandAno_Turno",
                                        "CandAno_Numero",
                                        "CandAno_Nome",
                                        "Partido_Sigla",
                                        "Coligacao_Id",
                                        "CandAno_QtVotos",
                                        "CandAno_SituacaoElec")
  
  # Observa os dados.
  # View(DadosEleicao.Vereador.Ano)
  
  # Agora carrega os dados dos candidatos a prefeito em 2000.
  message("Downloading mayor's data")
  
  # Search CEPESP API for data on mayoral candidates.
  DadosRaw.CandPref <- get_candidates(year = yearsExtract[i], 
                                      position = "Mayor", 
                                      cached = TRUE)
  
  # Formats the data of mayoral candidates.
  DadosCandidatos.Prefeito.Ano <- DadosRaw.CandPref %>% 
    # Selects relevant columns.
    select(SIGLA_UE, NUM_TURNO, ANO_ELEICAO, NUMERO_CANDIDATO, NOME_CANDIDATO, 
           SIGLA_PARTIDO, CODIGO_LEGENDA, COD_SIT_TOT_TURNO, DESC_SIT_TOT_TURNO) %>% 
    # Solve class problems
    mutate(COD_SIT_TOT_TURNO = CharaterToInteger(COD_SIT_TOT_TURNO)) %>% 
    # Withdraws candidates without election situation due to impugnation
    dplyr::filter(!is.na(COD_SIT_TOT_TURNO))  %>% 
    # Removes Null Candidates
    dplyr::filter(COD_SIT_TOT_TURNO != -1) %>% 
    # Removes repeat candidate cases (I don't know why that happened)
    arrange(SIGLA_UE, SIGLA_PARTIDO, NUM_TURNO, COD_SIT_TOT_TURNO) %>%
    distinct(SIGLA_UE, SIGLA_PARTIDO, NUM_TURNO, .keep_all = TRUE) %>% 
    # Ensures that CODIGO_LEGENDA (Coalition Identification) is in a friendly 
    # format, removing scientific notation.
    mutate(CODIGO_LEGENDA = format(CODIGO_LEGENDA, scientific = FALSE)) %>% 
    mutate(CODIGO_LEGENDA = trimws(CODIGO_LEGENDA))
  
  # Free memory.
  rm(DadosRaw.CandPref)
  
  # Busca no API CEPESP os dados das votações para prefeito
  DadosRaw.VotePref <- get_votes(year = yearsExtract[i], 
                                 position = "Mayor", 
                                 regional_aggregation = "Municipality", 
                                 cached = TRUE)
  
  # Formata dados dos candidatos e das votações
  DadosEleicao.Prefeito.Ano <- DadosRaw.VotePref %>% 
    # Seleciona colunas relevantes.
    select(SIGLA_UE, COD_MUN_IBGE, ANO_ELEICAO, DESCRICAO_CARGO, NUM_TURNO, 
           NUMERO_CANDIDATO, QTDE_VOTOS) %>% 
    # Insere dados sobre os candidatos.
    left_join(DadosCandidatos.Prefeito.Ano, by = c("SIGLA_UE", "NUM_TURNO", "ANO_ELEICAO",
                                                   "NUMERO_CANDIDATO")) %>% 
    mutate(COD_SIT_TOT_TURNO = as.integer(COD_SIT_TOT_TURNO)) %>% 
    # Ordena as variáveis.
    select(COD_MUN_IBGE, ANO_ELEICAO, DESCRICAO_CARGO, NUM_TURNO, NUMERO_CANDIDATO, 
           NOME_CANDIDATO, SIGLA_PARTIDO, CODIGO_LEGENDA, QTDE_VOTOS, COD_SIT_TOT_TURNO, DESC_SIT_TOT_TURNO) %>% 
    # Corrige o nome das variáveis.
    magrittr::set_names(c("Munic_Id", "CandAno_Ano", "CandAno_Cargo", "CandAno_Turno",
                          "CandAno_Numero", "CandAno_Nome", "Partido_Sigla", "Coligacao_Id",
                          "CandAno_QtVotos","CandAno_SituacaoElec", "DESC_SIT_TOT_TURNO"))
  
  # Free memory.
  rm(DadosRaw.VotePref)
  rm(DadosCandidatos.Prefeito.Ano)
  
  
  
}

# End
