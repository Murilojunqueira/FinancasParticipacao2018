
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import electoral data from CEPESP API

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)
# Most of the data come from CEPESP API: https://github.com/Cepesp-Fgv/cepesp-r 

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-03-09

# Dependencies:
# Instala Pacote do CEPESPdata. Se o pacote já está instalado, desabilite essas linhas.
# Instaling curl
# install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("Cepesp-Fgv/cepesp-r")

# Dependencies:
library(curl)
library(cepespR)


################## Tutorial ##################

# Documentação do API: https://github.com/Cepesp-Fgv/cepesp-r 

################## CodeBook ##################

# Código da Situação Eleitoral	Descrição da Situação Eleitoral
#   -7    Voto Nulo
#   -6    Voto em Branco
#   -5    Voto na Legenda
#   -1  	#NULO# (campo inválido por problemas de sistema)
#   1	    ELEITO
#   2	    SUPLENTE
#   3	    RENÚNCIA/FALECIMENTO/CASSAÇÃO ANTES DA ELEIÇÃO
#   4	    NÃO ELEITO
#   5	    ELEITO POR MÉDIA
#   6	    2º TURNO
#   7	    RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO
#   8	    REGISTRO NEGADO ANTES DA ELEIÇÃO
#   9	    REGISTRO NEGADO APÓS A ELEIÇÃO
#   10	  SUBSTITUÍDO
#   11	  INDEFERIDO COM RECURSO
#   12	  ELEITO POR QP

############### Importing Electoral Data ###############

# Debug:
# setwd(rstudioapi::getActiveProject())
# yearsExtract <- c(2016, 2012, 2008, 2004, 2000)
# InputFolder <- "data/raw/CepespData/"
# UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
# DeParaSitEleitoral <- fread("data/dataset/DeParaSitEleitoral.csv", sep = ";", dec = ",")


ImportElectoralData <- function(yearsExtract, InputFolder, DeParaSitEleitoral) {
  
  
  # Because the Cepesp API records information on your computer, in order to expedite 
  # extraction repeatability, it is important to select InputFolder as the 
  # working directory. Make sure this directory is suitable for receiving new 
  # information (a dedicated Dropbox directory, for example).
  OldWd <- getwd() # Save old working directory to relocate later
  setwd(InputFolder)
  # # Código da Situação Eleitoral	Descrição da Situação Eleitoral
  #   -7    Voto Nulo
  #   -6    Voto em Branco
  #   -5    Voto na Legenda
  #   -1  	#NULO# (campo inválido por problemas de sistema)
  #   1	    ELEITO
  #   2	    SUPLENTE
  #   3	    RENÚNCIA/FALECIMENTO/CASSAÇÃO ANTES DA ELEIÇÃO
  #   4	    NÃO ELEITO
  #   5	    MÉDIA
  #   6	    2º TURNO
  #   7	    RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO
  #   8	    REGISTRO NEGADO ANTES DA ELEIÇÃO
  #   9	    REGISTRO NEGADO APÓS A ELEIÇÃO
  #   10	  SUBSTITUÍDO
  #   11	  INDEFERIDO COM RECURSO
  #   
  
  
  # Helpful functions:
  CharaterToNumeric <- function(x) {
    x <- sub(",", ".", x)
    x <- as.numeric(x)
    return(x)
  }
  
  CharaterToInteger <- function(x) {
    x <- sub(",", ".", x)
    x <- as.integer(x)
    return(x)
  }
  
  # Filter relevant variables from "from/to" electoral situation dataset
  DeParaSitElec_Filted <- DeParaSitEleitoral %>% 
    select(SitElec_Ano, SitElec_De, SitElec_Para)
  
  # Creates a table for candidate data.
  DadosCandidatos <- tibble()
  
  # Loop for each year fetched.
  for(i in seq_along(yearsExtract)){
    
    # Debug:
    # i <- 1
    
    message("Downloading data from year ", yearsExtract[i])
    
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
    
    # Exibe que se está buscando os dados dos vereadores.
    message("Baixando dados dos candidatos vereadores")
    
    # Busca no API CEPESP os dados dos candidatos a vereador.
    DadosRaw.CandVer <- get_candidates(year = yearsExtract[i], 
                                       position = "Councillor", 
                                       cached = TRUE)
    
    # Processa dados dos candidatos a vereador.
    DadosCandidatos.Vereador.Ano <- DadosRaw.CandVer %>% 
      # Seleciona colunas relevantes.
      select(SIGLA_UE, NUM_TURNO, ANO_ELEICAO, NUMERO_CANDIDATO, NOME_CANDIDATO, 
             NUMERO_PARTIDO, SIGLA_PARTIDO, CODIGO_LEGENDA, COD_SIT_TOT_TURNO, DESC_SIT_TOT_TURNO) %>% 
      # Resolve problemas de classe
      mutate(COD_SIT_TOT_TURNO = CharaterToInteger(COD_SIT_TOT_TURNO)) %>% 
      # Retira candidatos sem situação eleitoral devido a impugnação
      dplyr::filter(!is.na(COD_SIT_TOT_TURNO)) %>% 
      # Retira candidatos com situação nula
      dplyr::filter(COD_SIT_TOT_TURNO != -1) %>% 
      # Retira casos de candidatos repetidos (não sei porque houve isso)
      arrange(SIGLA_UE, SIGLA_PARTIDO, NUM_TURNO, COD_SIT_TOT_TURNO) %>%
      distinct(SIGLA_UE, NUMERO_CANDIDATO, NUM_TURNO, .keep_all = TRUE) %>% 
      # Garante que o CODIGO_LEGENDA (identificação da coligação) esteja em um 
      # formato amigável, retirando a notação científica.
      mutate(CODIGO_LEGENDA = format(CODIGO_LEGENDA, scientific = FALSE)) %>% 
      mutate(CODIGO_LEGENDA = trimws(CODIGO_LEGENDA))
    
    # Free memory.
    rm(DadosRaw.CandVer)
    
    # Busca no API CEPESP os dados das votações para vereador.
    DadosRaw.VoteVer <- get_votes(year = yearsExtract[i], 
                                  position = "Councillor", 
                                  regional_aggregation = "Municipality", 
                                  cached=TRUE)

    # Formata dados dos candidatos e das votações.
    DadosEleicao.Vereador.Ano <- DadosRaw.VoteVer %>% 
      # Seleciona colunas relevantes.
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
      # Ordena as variáveis.
      select(COD_MUN_IBGE, ANO_ELEICAO, DESCRICAO_CARGO, NUM_TURNO, NUMERO_CANDIDATO, 
             NOME_CANDIDATO, SIGLA_PARTIDO, CODIGO_LEGENDA, QTDE_VOTOS, COD_SIT_TOT_TURNO, DESC_SIT_TOT_TURNO) %>% 
      # Corrige o nome das variáveis.
      magrittr::set_names(c("Munic_Id", "CandAno_Ano", "CandAno_Cargo", "CandAno_Turno", "CandAno_Numero",
                            "CandAno_Nome", "Partido_Sigla", "Coligacao_Id", "CandAno_QtVotos",
                            "CandAno_SituacaoElec", "DESC_SIT_TOT_TURNO"))
    
    # Free memory.
    rm(DadosCandidatos.Vereador.Ano)
    rm(DadosRaw.VoteVer)

    
    # Empilha os dados sobre os prefeitos e sobre os vereadores.
    DadosCandidatos.Ano <- bind_rows(DadosEleicao.Prefeito.Ano, DadosEleicao.Vereador.Ano) %>% 
      mutate(CandAno_SituacaoElec = as.integer(CandAno_SituacaoElec)) %>% 
      # Um pequeno truque para diminuir o tamanho do banco.
      mutate(CandAno_Cargo = as.character(CandAno_Cargo)) %>% 
      mutate(CandAno_Cargo = substr(CandAno_Cargo, 1, 1)) %>% 
      # Standarize electoral situation
      left_join(DeParaSitElec_Filted, by = c("CandAno_Ano" = "SitElec_Ano", 
                                             "CandAno_SituacaoElec" = "SitElec_De")) %>% 
      mutate(CandAno_SituacaoElec = SitElec_Para) %>% 
      select(-SitElec_Para)
    
    # Free memory.
    rm(DadosEleicao.Prefeito.Ano, DadosEleicao.Vereador.Ano)

    # insere dados na tabela de agregação principal.
    DadosCandidatos <- bind_rows(DadosCandidatos, DadosCandidatos.Ano)
    
    # Free memory.
    rm(DadosCandidatos.Ano)
    gc()
  }
  
  # Retorna o antigo diretório de trabalho
  setwd(OldWd)
  rm(OldWd, i)
  rm(CharaterToInteger, CharaterToNumeric)
  
  # Output function
  return(DadosCandidatos)
}
# End