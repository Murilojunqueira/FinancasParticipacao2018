
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Create tables "Coligacoes" (parties coalitions) and "CoalizõesPartidos" 
# (parties coalitions)

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

############### Create "Coalizoes" table ###############


# Debug:
# setwd(rstudioapi::getActiveProject())
# years = c(2016, 2012, 2008, 2004, 2000) # Vetor com os anos da extração.
# InputFolder <- "data/raw/CepespData/"
# UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)


ImportCoalitionsCEPESPdata <- function(years, InputFolder, UEToCodIBGE) {
  
  # Because the Cepesp API records information on your computer, in order to expedite 
  # extraction repeatability, it is important to select InputFolder as the 
  # working directory. Make sure this directory is suitable for receiving new 
  # information (a dedicated Dropbox directory, for example).
  OldWd <- getwd() # Save old working directory to relocate later
  setwd(InputFolder)
  # getwd()
  
  # Garante que a sigla_UE é uma variável texto.
  UEToCodIBGE$SIGLA_UE <- as.character(as.integer(UEToCodIBGE$SIGLA_UE))

  # Cria banco de dados para a extração dos dados das coligações e dos partidos.
  DadosColigacoes <- as_tibble()
  ColigacoesPartidos <- as_tibble()
  
  # Loop para cada ano da amostra.
  for (i in seq_along(years)) {
    
    # Linha de debug:
    # i <- 1
    
    # Exibe o ano que está sendo processado.
    print(paste0("Baixando dados do ano de ", years[i]))
    
    # Exibe progresso do código.
    print("Baixando dados de coligações")
    
    # Baixa os dados das coligações dos prefeitos e vereadores.
    DadosRaw.ColgPref.ano <- get_coalitions(year=years[i], position="Mayor", cached=TRUE)
    DadosRaw.ColgVer.ano <- get_coalitions(year=years[i], position="Councillor", cached=TRUE)
    
    # names(DadosRaw.ColgPref.ano)
    # names(DadosRaw.ColgVer.ano)
    
    # Formata os dados das colinações dos prefeitos.
    DadosEleicao.Coligacao.Join <- bind_rows(DadosRaw.ColgPref.ano, DadosRaw.ColgVer.ano)  %>% 
      # Transforma a sigla da UE em variável texto.
      mutate(SIGLA_UE = as.character(as.integer(SIGLA_UE))) %>% 
      # insere a tabela de De/Para entre siglaUE e Cod_Munic IBGE.
      left_join(UEToCodIBGE, by = "SIGLA_UE") %>% 
      # Teste Franca
      # dplyr::filter(Munic_Id == 3516200) %>% 
      # Seleciona as colunas relevantes.
      select(Munic_Id, ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO, NOME_COLIGACAO, 
             NUMERO_PARTIDO, SIGLA_PARTIDO, SEQUENCIA_COLIGACAO, COMPOSICAO_COLIGACAO) %>% 
      # Abrevia a variável Cargo
      mutate(DESCRICAO_CARGO = substr(DESCRICAO_CARGO, 1, 1)) %>% 
      # Corrige o dado missing da composição da coalizão (acontece só em 2008)
      mutate(COMPOSICAO_COLIGACAO  = ifelse(COMPOSICAO_COLIGACAO == "#NE#", 
                                            NOME_COLIGACAO, COMPOSICAO_COLIGACAO)) %>% 
      # Deixa o número da coligação mais amigavel.
      mutate(SEQUENCIA_COLIGACAO = format(SEQUENCIA_COLIGACAO, scientific = FALSE)) %>% 
      mutate(SEQUENCIA_COLIGACAO = trimws(SEQUENCIA_COLIGACAO))

    # Libera Memória
    rm(DadosRaw.ColgPref.ano, DadosRaw.ColgVer.ano)
    
    # Dados das Coligações 
    DadosEleicao.Coligacao.Ano <- DadosEleicao.Coligacao.Join %>% 
      # Remove variáveis descenecessárias
      # select(-SIGLA_UE, -NUMERO_PARTIDO) %>% 
      # Remove repetições de coligações
      distinct(ANO_ELEICAO, Munic_Id, NUM_TURNO, DESCRICAO_CARGO,
               COMPOSICAO_COLIGACAO, .keep_all = TRUE) %>% 
      # Cria nova variável de identificação da coalizão
      group_by(Munic_Id) %>% 
      mutate(SeqNum = row_number()) %>% 
      ungroup() %>% 
      mutate(Coligacao_Id = paste0(Munic_Id, "_", ANO_ELEICAO, "_", SeqNum)) %>% 
      select(-SeqNum) %>% 
      # Muda a ordem das variáveis.
      select(Coligacao_Id, Munic_Id, ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO,
             NOME_COLIGACAO, SEQUENCIA_COLIGACAO, COMPOSICAO_COLIGACAO) %>% 
      # Ordena o banco pelo ano da eleição, município, cargo e coligação.
      arrange(ANO_ELEICAO, Munic_Id, NUM_TURNO, DESCRICAO_CARGO, Coligacao_Id) %>% 
      # Teste Rio de Janeiro
      # dplyr::filter(Munic_Id == 3304557) %>% 
      magrittr::set_names(c("Coligacao_Id",
                            "Munic_Id",
                            "Coligacao_Ano",
                            "Coligacao_Turno",
                            "Coligacao_Cargo",
                            "Coligacao_Nome",
                            "Coligacao_IdTSE",
                            "Coligacao_composicao"))
    
    # View(DadosEleicao.Coligacao.Ano)
    
    # Exibe progresso do código.
    print("Baixando dados de coligações-Partidos")
    
    # Formata os dados da coalizão-partido.
    ColigacoesPartidos.Ano <- DadosEleicao.Coligacao.Join %>% 
      # Seleciona colunas relevantes.
      select(Munic_Id, NUMERO_PARTIDO, NUM_TURNO, DESCRICAO_CARGO, ANO_ELEICAO, 
             COMPOSICAO_COLIGACAO, SIGLA_PARTIDO) %>%
      # Add Coalition Id
      left_join(select(DadosEleicao.Coligacao.Ano, 
                       Munic_Id, Coligacao_composicao, Coligacao_Turno, 
                       Coligacao_Cargo, Coligacao_Id),
                by = c("Munic_Id" = "Munic_Id",
                       "DESCRICAO_CARGO" = "Coligacao_Cargo",
                       "NUM_TURNO" = "Coligacao_Turno",
                       "COMPOSICAO_COLIGACAO" = "Coligacao_composicao")) %>% 
      # Teste Franca
      # dplyr::filter(Munic_Id == 3516200) %>% 
      # Ordena por número da coligação.
      arrange(Munic_Id, ANO_ELEICAO, NUMERO_PARTIDO) %>% 
      select(Coligacao_Id, ANO_ELEICAO, DESCRICAO_CARGO, SIGLA_PARTIDO) %>% 
      magrittr::set_names(c("Coligacao_Id",
                            "Coligacao_Ano",
                            "Coligacao_Cargo", 
                            "Partido_Sigla"))
    
    # View(ColigacoesPartidos.Ano)
    
    if(nrow(ColigacoesPartidos.Ano) != nrow(DadosEleicao.Coligacao.Join)) {
      stop("Problably Join Problems")
    }
    
    # Agrega as extrações do ano nas tabelas agregadoras
    DadosColigacoes <- rbind(DadosColigacoes, DadosEleicao.Coligacao.Ano)
    ColigacoesPartidos <- rbind(ColigacoesPartidos, ColigacoesPartidos.Ano)
    
    # Libera Memória
    rm(DadosEleicao.Coligacao.Ano, ColigacoesPartidos.Ano)
    gc()
  }
  # Libera Memória
  rm(i, years)
  
  # Return ond working directory
  setwd(OldWd)
  rm(OldWd)
  
  return(list(DadosColigacoes, ColigacoesPartidos))
}

# End