
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import electoral data of 2000 election from TSE files

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-05-25

# Dependencies:
library(tidyverse)
library(readxl)


############### Importing Electoral Data 1996 ###############


# Debug:
# InputFolders = c("data/raw/TSE/2000/")
# UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
# DeParaSitEleitoral <- fread("data/dataset/DeParaSitEleitoral.csv", sep = ";", dec = ",")


# Variáveis: 
# [1] "Munic_Id"             "CandAno_Ano"          "CandAno_Cargo"       
# [4] "CandAno_Turno"        "CandAno_Numero"       "CandAno_Nome"        
# [7] "Partido_Sigla"        "Coligacao_Id"         "CandAno_QtVotos"     
# [10] "CandAno_SituacaoElec" "DESC_SIT_TOT_TURNO" 

ImportElectoralDataTSE <- function(InputFolders, UEToCodIBGE, DeParaSitEleitoral) {
  
  RawNames <- c("DATA_GERACAO",
                "HORA_GERACAO",
                "ANO_ELEICAO",
                "NUM_TURNO",
                "DESCRICAO_ELEICAO",
                "SIGLA_UF",
                "SIGLA_UE",
                "CODIGO_MUNICIPIO",
                "NOME_MUNICIPIO",
                "NUMERO_ZONA",
                "CODIGO_CARGO",
                'NUMERO_CAND',
                'SQ_CANDIDATO',
                'NOME_CANDIDATO',
                'NOME_URNA_CANDIDATO',
                'DESCRICAO_CARGO',
                'COD_SIT_CAND_SUPERIOR',
                'DESC_SIT_CAND_SUPERIOR',
                'CODIGO_SIT_CANDIDATO',
                'DESC_SIT_CANDIDATO',
                'CODIGO_SIT_CAND_TOT',
                'DESC_SIT_CAND_TOT',
                'NUMERO_PARTIDO',
                'SIGLA_PARTIDO',
                'NOME_PARTIDO',
                'SEQUENCIAL_LEGENDA',
                'NOME_COLIGACAO',
                'COMPOSICAO_LEGENDA',
                'TOTAL_VOTOS')
  
  # Avoid class problems to Left_Join
  UEToCodIBGE$SIGLA_UE <- as.character(UEToCodIBGE$SIGLA_UE)
  
  ElectoralData <- tibble()
  
  for (i in seq_along(InputFolders)) {
    
    # i <- 1
    FilesList <- list.files(InputFolders[i], pattern = "^votacao_candidato.*txt$")
    
    for (j in seq_along(FilesList)) {
      
      # j <- 25
      message("Processing file ", FilesList[j])
      
      # Import Raw Data
      RawDataStateYear <- fread(paste0(InputFolders[i], FilesList[j])) 
      
      # Skip loop if the data as zero rows
      if (nrow(RawDataStateYear) == 0) next
      
      # Set variables names
      RawDataStateYear <- magrittr::set_names(RawDataStateYear, RawNames)
      
      # Format data
      ElectoralData_State_Year <- RawDataStateYear %>%
        # Insert IBGE municipal code
        mutate(SIGLA_UE = as.character(as.integer(CODIGO_MUNICIPIO))) %>% 
        left_join(UEToCodIBGE, by = "SIGLA_UE") %>% 
        # Select relevante columns
        select(Munic_Id, ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, NUMERO_CAND,
               NOME_CANDIDATO, SIGLA_PARTIDO, COMPOSICAO_LEGENDA, TOTAL_VOTOS,
               CODIGO_SIT_CAND_TOT, DESC_SIT_CAND_TOT) %>%
        # Group by everything but number of votes
        group_by(Munic_Id, ANO_ELEICAO, CODIGO_CARGO, NUM_TURNO, NUMERO_CAND,
                 NOME_CANDIDATO, SIGLA_PARTIDO, COMPOSICAO_LEGENDA,
                 CODIGO_SIT_CAND_TOT, DESC_SIT_CAND_TOT) %>% 
        summarise(CandAno_QtVotos = sum(TOTAL_VOTOS)) %>% 
        ungroup() %>% 
        # Correct names
        rename(CandAno_Ano = ANO_ELEICAO) %>% 
        rename(CandAno_Cargo = CODIGO_CARGO) %>% 
        rename(CandAno_Turno = NUM_TURNO) %>% 
        rename(CandAno_Numero = NUMERO_CAND) %>% 
        rename(CandAno_Nome = NOME_CANDIDATO) %>% 
        rename(Partido_Sigla = SIGLA_PARTIDO) %>% 
        rename(Coligacao_Id = COMPOSICAO_LEGENDA) %>% 
        # rename(CandAno_QtVotos = TOTAL_VOTOS) %>% 
        rename(CandAno_SituacaoElec = CODIGO_SIT_CAND_TOT) %>% 
        rename(DESC_SIT_TOT_TURNO = DESC_SIT_CAND_TOT) %>% 
        # Select relevant vars
        select("Munic_Id", "CandAno_Ano", "CandAno_Cargo", "CandAno_Turno", "CandAno_Numero",
               "CandAno_Nome", "Partido_Sigla", "Coligacao_Id", "CandAno_QtVotos",
               "CandAno_SituacaoElec", "DESC_SIT_TOT_TURNO")
        
      # Insert correct code for position
      
      # Detect year of the data
      YearData <-  -table(ElectoralData_State_Year$CandAno_Ano) %>% 
        sort() %>% 
        names() %>% 
        .[1] %>% 
        as.integer()
      
      if(YearData == 1996) {
        ElectoralData_State_Year <- ElectoralData_State_Year %>% 
          mutate(CandAno_Cargo = ifelse(CandAno_Cargo == 9, "P", CandAno_Cargo)) %>% 
          mutate(CandAno_Cargo = ifelse(CandAno_Cargo == 11, "V", CandAno_Cargo))
      }
      
      if(YearData >= 2000 & YearData < 2012) {
        ElectoralData_State_Year <- ElectoralData_State_Year %>% 
          mutate(CandAno_Cargo = ifelse(CandAno_Cargo == 11, "P", CandAno_Cargo)) %>% 
          mutate(CandAno_Cargo = ifelse(CandAno_Cargo == 13, "V", CandAno_Cargo))
      }
      
      ElectoralData <- bind_rows(ElectoralData, ElectoralData_State_Year)
      
      # Free memory
      rm(RawDataStateYear, ElectoralData_State_Year)
      
    }
  }
  
  # Filter relevant variables from "from/to" electoral situation dataset
  DeParaSitElec_Filted <- DeParaSitEleitoral %>% 
    select(SitElec_Ano, SitElec_De, SitElec_Para)
  
  # Standarize electoral situation
  ElectoralData <- ElectoralData %>% 
    mutate(CandAno_SituacaoElec = as.integer(CandAno_SituacaoElec)) %>%
    left_join(DeParaSitElec_Filted, by = c("CandAno_Ano" = "SitElec_Ano",
                                           "CandAno_SituacaoElec" = "SitElec_De")) %>%
    mutate(CandAno_SituacaoElec = SitElec_Para) %>%
    select(-SitElec_Para)
  
  return(ElectoralData)
}


# End
