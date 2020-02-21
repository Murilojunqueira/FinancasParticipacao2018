
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import electoral data from 1996 mdb file

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)
# Site TSE (not working): http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

# Site TSE (2): http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-1996/resultados-das-eleicoes 
# Site TSE (3) (more convenient): http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-1996/divulgacao-candidatos-1996 


# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-05-25

# Dependencies:
library(tidyverse)
library(data.table)
library(readxl)

# Debug:
# InputFile1T <- "data/raw/TSE/TSE-relacao-candidatos-1996/Candidatos_1996.xlsx"
# InputFile2T <- "data/raw/TSE/TSE-relacao-candidatos-1996/Resultado_da_Eleição_(Por_Municipio)_1996.csv"
# UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)


ElectoralData1996 <- function(InputFile1T, InputFile2T, UEToCodIBGE) {
  
  message("Reading Electoral Data")
  
  # Carrega os dados a partir do Excel correspondente.
  CandidateVotes <- read_excel(InputFile1T, 
                               sheet = 1, 
                               col_types = rep("text", 22))
  
  message("Processing Data")
  
  # Format and rename the dataframe
  CandidatoAno.1996 <- CandidateVotes %>% 
    # Transform electoral city code in IBGE's city code
    mutate(COD_MUN = as.numeric(COD_MUN)) %>% 
    left_join(UEToCodIBGE, by = c("COD_MUN" = "SIGLA_UE")) %>% 
    # Linhas debug:
    # filter(Munic_Id == 3550308) %>% 
    # filter(CARGO == "Prefeito") %>% 
    # Add election's year
    mutate(CandAno_Ano = 1996) %>% 
    # Transform and Rename variables
    rename(CandAno_Cargo = CARGO) %>%
    mutate(CandAno_Cargo = as.character(CandAno_Cargo)) %>% 
    mutate(CandAno_Cargo = toupper(CandAno_Cargo)) %>% 
    mutate(CandAno_Cargo = substr(CandAno_Cargo, 1, 1)) %>% 
    mutate(CandAno_Turno = ifelse(is.na(SITUACAO2T), 1, 2)) %>% 
    rename(CandAno_Numero = NUMERO) %>%
    mutate(CandAno_Numero = as.integer(CandAno_Numero)) %>% 
    rename(CandAno_Nome = NOME) %>%
    rename(Partido_Sigla = SGL_PARTIDO) %>%
    # We don't have coligation data in this dataframe
    mutate(Coligacao_Id = NA) %>%
    rename(CandAno_QtVotos = QTD_VOTOS) %>%
    mutate(CandAno_SituacaoElec = ifelse(is.na(SITUACAO2T), SITUACAO1T, SITUACAO2T)) %>% 
    dplyr::filter(!is.na(CandAno_SituacaoElec)) %>% 
    mutate(DESC_SIT_TOT_TURNO = toupper(CandAno_SituacaoElec)) %>% 
    # Transmor candidate's situation description in candidate's situation code (2012 format)
    mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Eleito", "1", CandAno_SituacaoElec)) %>%
    mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "2º turno", "6", CandAno_SituacaoElec)) %>%
    mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Eleito por Média", "5", CandAno_SituacaoElec)) %>%
    mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Não eleito", "4", CandAno_SituacaoElec)) %>% 
    mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Suplente", "2", CandAno_SituacaoElec)) %>% 
    mutate(CandAno_SituacaoElec= as.integer(CandAno_SituacaoElec)) %>% 
    # Select relevant vars
    select("Munic_Id", "CandAno_Ano", "CandAno_Cargo", "CandAno_Turno", "CandAno_Numero",
           "CandAno_Nome", "Partido_Sigla", "Coligacao_Id", "CandAno_QtVotos",
           "CandAno_SituacaoElec", "DESC_SIT_TOT_TURNO")
  
  
  # Adiciona dados do segundo turno
  # O banco anterior mostra a quantidade total de votos de cada candidato, somando 
  #  primeiro e segundo turno (?)
  
  
  # Lê outro banco de dados, onde aparece apenas os votos do segundo turno
  # Carrega os dados a partir do Excel correspondente.
  PrefVotes <- fread(InputFile2T,
                     skip = 6, header = FALSE) %>% 
    select(-V11) %>% 
    magrittr::set_names(c("CandAno_Cargo",
                          "UF_Sigla",
                          "Munic_Nome",
                          "Partido_Sigla",
                          "CandAno_Numero",
                          "CandAno_Nome",
                          "CandAno_QtVotos",
                          "CandAno_SituacaoElecDesc",
                          "PerValidos",
                          "Coligacao_Id")) 
  
  PrefVotes <- PrefVotes %>%  
    select(CandAno_Nome, CandAno_QtVotos) %>% 
    rename(Votos2T = CandAno_QtVotos) %>% 
    mutate(Votos2T = str_replace_all(Votos2T, ",", "")) %>% 
    mutate(Votos2T = as.integer(Votos2T))

  
  # Seleciona candidatos que foram ao segundo turno
  CandidatoAno.1996.2t <- CandidatoAno.1996 %>% 
    dplyr::filter(CandAno_Turno == 2) %>% 
    left_join(PrefVotes, by = "CandAno_Nome") %>% 
    rename(VotosTotal = CandAno_QtVotos) %>% 
    mutate(VotosTotal = as.integer(VotosTotal)) %>% 
    mutate(Votos1T = VotosTotal - Votos2T) %>% 
    select(-VotosTotal, -CandAno_Turno) %>% 
    gather("CandAno_Turno", "CandAno_QtVotos", Votos1T, Votos2T) %>% 
    mutate(CandAno_Turno = ifelse(CandAno_Turno == "Votos1T", 1, CandAno_Turno)) %>% 
    mutate(CandAno_Turno = ifelse(CandAno_Turno == "Votos2T", 2, CandAno_Turno)) %>% 
    mutate(CandAno_Turno = as.integer(CandAno_Turno)) %>% 
    mutate(CandAno_SituacaoElec = ifelse(CandAno_Turno == 1, 6, CandAno_SituacaoElec)) %>% 
    mutate(DESC_SIT_TOT_TURNO = ifelse(CandAno_SituacaoElec == 6, "2º TURNO", DESC_SIT_TOT_TURNO)) %>% 
    arrange(Munic_Id, CandAno_Turno) %>% 
    select(names(CandidatoAno.1996))
  
  
  CandidatoAno.1996.final <- CandidatoAno.1996 %>% 
    dplyr::filter(CandAno_Turno != 2) %>% 
    mutate(CandAno_QtVotos = as.integer(CandAno_QtVotos)) %>% 
    bind_rows(CandidatoAno.1996.2t) %>% 
    # Resolve alguns problemas de classe
    mutate(CandAno_Ano = as.integer(CandAno_Ano)) %>% 
    mutate(CandAno_Turno = as.integer(CandAno_Turno)) %>% 
    mutate(CandAno_SituacaoElec = as.integer(CandAno_SituacaoElec)) %>% 
    mutate(Coligacao_Id = as.character(Coligacao_Id)) %>% 
    # Debug line:
    # dplyr::filter(Munic_Id == 3550308) %>% 
    arrange(Munic_Id, CandAno_Ano, CandAno_Cargo, CandAno_Turno, 
            desc(CandAno_QtVotos)) 
  
  return(CandidatoAno.1996.final)
}

