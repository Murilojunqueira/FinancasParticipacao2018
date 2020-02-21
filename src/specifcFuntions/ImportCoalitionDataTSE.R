
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import electoral data of 2000 election

# Original source data from TSE - Tribunal Superior Eleitural (Superior Electoral Court)

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2019-12-05

# Dependencies:
library(tidyverse)

############### Importing Electoral Data 1996 ###############


# Debug:
# InputFolders = c("data/raw/TSE/2000/")
# UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)


# Variáveis: 
# > names(Coligacoes)
# [1] "Coligacao_Id"         "Munic_Id"             "Coligacao_Ano"        "Coligacao_Turno"     
# [5] "Coligacao_Cargo"      "Coligacao_Nome"       "Coligacao_IdTSE"      "Coligacao_composicao"

# > names(ColigacoesPartidos)
# [1] "Coligacao_Id"  "Coligacao_Ano" "Partido_Id"    "Partido_Sigla"

ImportCoalitionDataTSE <- function(InputFolders, UEToCodIBGE) {
  
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
  
  # Cria banco de dados para a extração dos dados das coligações e dos partidos.
  DadosColigacoes <- as_tibble()
  ColigacoesPartidos <- as_tibble()
  
  for (i in seq_along(InputFolders)) {
    
    # i <- 1
    FilesList <- list.files(InputFolders[i], pattern = "^votacao_candidato.*txt$")
    
    for (j in seq_along(FilesList)) {
      
      # j <- 25
      message("Processing file ", FilesList[j])
      
      # Import Raw Data
      RawDataStateYear <- fread(paste0(InputFolders[i], FilesList[j])) %>% 
        # Set variables names
        magrittr::set_names(RawNames)
      
      # Skip loop if the data as zero rows
      if (nrow(RawDataStateYear) == 0) next
      
      # Formata os dados das colinações dos prefeitos.
      DadosEleicao.Coligacao.Join <- RawDataStateYear  %>% 
        # Seleciona as colunas relevantes.
        mutate(SIGLA_UE = as.character(CODIGO_MUNICIPIO)) %>% 
        select(SIGLA_UE, ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO, NOME_COLIGACAO, 
               NUMERO_PARTIDO, SIGLA_PARTIDO, SEQUENCIAL_LEGENDA, COMPOSICAO_LEGENDA) %>% 
        # Abrevia a variável Cargo
        mutate(DESCRICAO_CARGO = substr(DESCRICAO_CARGO, 1, 1)) %>% 
        # Corrige o dado missing da composição da coalizão (acontece só em 2008)
        mutate(COMPOSICAO_LEGENDA  = ifelse(COMPOSICAO_LEGENDA == "#NE#", 
                                              NOME_COLIGACAO, COMPOSICAO_LEGENDA)) %>% 
        # Corrige o dado missing da composição da coalizão
        mutate(COMPOSICAO_LEGENDA  = ifelse(COMPOSICAO_LEGENDA == "#NULO#", 
                                            SIGLA_PARTIDO, COMPOSICAO_LEGENDA)) %>% 
        # Corrige o dado missing da composição da coalizão
        mutate(NOME_COLIGACAO  = ifelse(NOME_COLIGACAO == "#NULO#", 
                                        SIGLA_PARTIDO, NOME_COLIGACAO)) %>%
        # Deixa o número da coligação mais amigavel.
        mutate(SEQUENCIAL_LEGENDA = format(SEQUENCIAL_LEGENDA, scientific = FALSE)) %>% 
        mutate(SEQUENCIAL_LEGENDA = trimws(SEQUENCIAL_LEGENDA)) %>% 
        # insere a tabela de De/Para entre siglaUE e Cod_Munic IBGE.
        left_join(UEToCodIBGE, by = "SIGLA_UE")
      
      rm(RawDataStateYear)
      
      # Dados das Coligações 
      DadosEleicao.Coligacao.Ano <- DadosEleicao.Coligacao.Join %>% 
        # Teste Franca
        # dplyr::filter(Munic_Id == 3516200) %>%
        # Remove repetições de coligações
        distinct(ANO_ELEICAO, Munic_Id, NUM_TURNO, DESCRICAO_CARGO,
                 COMPOSICAO_LEGENDA, .keep_all = TRUE) %>% 
        # Cria nova variável de identificação da coalizão
        group_by(Munic_Id) %>% 
        mutate(SeqNum = row_number()) %>% 
        ungroup() %>% 
        mutate(Coligacao_Id = paste0(Munic_Id, "_", ANO_ELEICAO, "_", SeqNum)) %>% 
        select(-SeqNum) %>% 
        # Muda a ordem das variáveis.
        select(Coligacao_Id, Munic_Id, ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO,
               NOME_COLIGACAO, SEQUENCIAL_LEGENDA, COMPOSICAO_LEGENDA) %>% 
        # Ordena o banco pelo ano da eleição, município, cargo e coligação.
        arrange(ANO_ELEICAO, Munic_Id, NUM_TURNO, DESCRICAO_CARGO, Coligacao_Id) %>% 
        magrittr::set_names(c("Coligacao_Id",
                              "Munic_Id",
                              "Coligacao_Ano",
                              "Coligacao_Turno",
                              "Coligacao_Cargo",
                              "Coligacao_Nome",
                              "Coligacao_IdTSE",
                              "Coligacao_composicao"))
      
      # View(DadosEleicao.Coligacao.Ano)

      # Formata os dados da coalizão-partido.
      ColigacoesPartidos.Ano <- DadosEleicao.Coligacao.Ano %>% 
        # Teste Franca
        # dplyr::filter(Munic_Id == 3516200) %>% 
        mutate(Coligacao_composicao2 = Coligacao_composicao) %>% 
        separate(Coligacao_composicao2, into = letters[1:20], 
                 sep = "/", fill = "right") %>% 
        gather(temp, Partido_Sigla, 9:28) %>%  
        mutate(Partido_Sigla = trimws(Partido_Sigla)) %>% 
        dplyr::filter(!is.na(Partido_Sigla)) %>% 
        select(-temp) %>%
        arrange(Coligacao_Cargo, Coligacao_composicao) %>% 
        select(Coligacao_Id, Coligacao_Ano, Coligacao_Cargo, Partido_Sigla)
      
      # View(ColigacoesPartidos.Ano)
      
      # Agrega as extrações do ano nas tabelas agregadoras
      DadosColigacoes <- rbind(DadosColigacoes, DadosEleicao.Coligacao.Ano)
      ColigacoesPartidos <- rbind(ColigacoesPartidos, ColigacoesPartidos.Ano)
      
      rm(ColigacoesPartidos.Ano, DadosEleicao.Coligacao.Ano, DadosEleicao.Coligacao.Join)
    }
  }
  return(list(DadosColigacoes, ColigacoesPartidos))
}


# End
