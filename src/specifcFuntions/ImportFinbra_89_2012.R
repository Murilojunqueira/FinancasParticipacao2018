# Script to extract data from raw files of Finbra, Finances of Brasil (Finanças do Brasil) 
# from 1989 until 2012 into R.

# Raw data source: STN (Secretaria do Tesouro Nacional, National Treasure Department)

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Script creation data: 2018-02-22.


# Dependencies
library(dplyr)
library(data.table)
library(readxl)

################## External Functions ##################

source("src/specifcFuntions/FormatFinbra.R")
source("src/specifcFuntions/MuncCod6To7.R")

################## Function ##################

# This function aims to extract Finbra search data, which is already 
# inserted into Excel tables in the InputFolder directory for the
# model chosen to be the database, ie the table "MunicFinancas.csv".


# Debug:
# years_extract <- 2000:1997
# ContasPublicas <- ContasPublicas
# DeParaFinbra <- DeParaFinbra
# BDCamposFinbra <- BDCamposFinbra
# DeParaUGCodIBGE <- DeParaUGCodIBGE
# Municipios <- Municipios
# InputFolder <- "data/raw/Finbra/ExcelFiles/"


ImportFinbra_89_2012 <- function(years_extract,  ContasPublicas, DeParaFinbra, BDCamposFinbra, 
                                 DeParaUGCodIBGE, Municipios, InputFolder) {

  
  # Creates an empty database to aggregate municipal finance data.
  MunicFinancas <- tibble()
  

  # Loop for each row of the ContasPublicas table:
  for(i in seq_len(nrow(ContasPublicas))) {
    
    # Debug:
    # i <- 14
    
    # Displays the account being processed.
    message("Formatting Variable ", ContasPublicas$ContasPublica_Descricao[i])
    
    
    # Filters only the public account being processed from the DeParaFinbra table.
    DeParaFinbra.Select <- DeParaFinbra %>% 
      # Filters the corresponding public account.
      dplyr::filter(ContasPublica_Id == as.character(ContasPublicas$ContasPublica_Id[i])) %>% 
      # Sorts the rows by year.
      arrange(desc(DeParaFinbra_Ano)) %>% 
      # Ensures that the year is integer data.
      mutate(DeParaFinbra_Ano = as.integer(DeParaFinbra_Ano)) %>% 
      # Filters the years of analysis that
      dplyr::filter(DeParaFinbra_Ano %in% years_extract)
    
    # Creates an empty database to aggregate the account data being processed.
    MunicFinancas.NewVar <- as_tibble()
    
    # Loop for each row selected in the DeParaFinbra table (corresponding to the processed account).
    for(j in seq_len(nrow(DeParaFinbra.Select))) {
      
      # Linha de debug.
      # j <- 1
      
      message("Searching data for year ", DeParaFinbra.Select$DeParaFinbra_Ano[j])
      
      # Turns the year row in the DeParaFinbra table into a list.
      CampoFinbra.Ref <- DeParaFinbra.Select[j,] %>% 
        unlist %>% as.list()
      
      # Creates a list with the location (File, table and column) of the data to be
      # extracted.
      BDCamposFinbra.Select <- BDCamposFinbra %>% 
        dplyr::filter(FinbraCampo_Id == CampoFinbra.Ref$FinbraCampo_Id) %>% 
        unlist %>% as.list()
      
      # Creates a vector with the field names that identify the municipalities.
      Munic.Select <- BDCamposFinbra %>% 
        dplyr::filter(FinbraCampo_Ano == CampoFinbra.Ref$DeParaFinbra_Ano) %>% 
        dplyr::filter(FinbraCampo_Tabela == BDCamposFinbra.Select$FinbraCampo_Tabela) %>% 
        dplyr::filter(FinbraCampo_IDMunic == 1) %>% 
        select(FinbraCampo_Campo) %>% 
        unlist %>% as.character()
      
      # Input file's path
      FilePath <- paste0(InputFolder, BDCamposFinbra.Select$FinbraCampo_ArquivoXls)
      
      # debug:
      # file.exists(FilePath)
      
      # Load data from the corresponding Excel.
      BD.Fetch <- read_excel(FilePath, 
                             sheet = BDCamposFinbra.Select$FinbraCampo_Tabela)
      
      # Creates a vector with the required columns (columns of municipality identification 
      # and columns of data).
      Select.Columns <- c(Munic.Select, BDCamposFinbra.Select$FinbraCampo_Campo)
      
      # Formats the data in the desired format.
      MunicFinancas.New <- BD.Fetch %>% 
        # Selects the corresponding columns
        select(Select.Columns)  %>% 
        # Formats the data (currently the FormatFinbra function only improves the format of
        # city identification columns). 
        FormatFinbra(Ano = BDCamposFinbra.Select$FinbraCampo_Ano,
                     Aba = BDCamposFinbra.Select$FinbraCampo_Tabela,
                     UGtoCodIBGE = DeParaUGCodIBGE, 
                     BDCamposFinbra = BDCamposFinbra,
                     InputFolder = InputFolder) %>% 
        # Muda o formato do código IBGE de 6 dígitos (antigo) para 7 dígitos (novo).
        MuncCod6To7("Munic_Id6", Municipios) %>% 
        # Garante que o ano seja inteiro (integer)
        mutate(MunicFinancas_Ano = as.integer(BDCamposFinbra.Select$FinbraCampo_Ano)) %>%
        # Seleciona os dados finais
        select(Munic_Id, MunicFinancas_Ano, everything())
      
      # Padroniza o nome da variável, de acordo com a tabela ContasPublicas.
      names(MunicFinancas.New)[
        which(names(MunicFinancas.New) == BDCamposFinbra.Select$FinbraCampo_Campo)] <-
        ContasPublicas$ContasPublica_Nome[i]
      
      # Linha de debug:
      # head(MunicFinancas.New)
      
      # Acrescenta os dados processados do ano à tabela de agregação da variável processada.
      MunicFinancas.NewVar <- rbind(MunicFinancas.NewVar, MunicFinancas.New)
      
      # Libera memória
      rm(Select.Columns, MunicFinancas.New)
      rm(BD.Fetch, CampoFinbra.Ref, BDCamposFinbra.Select, Munic.Select)
    }
    
    if (nrow(MunicFinancas.NewVar) == 0) {
      next()
    }
    
    # Embilha os dados
    MunicFinancas.NewVar.Format <- MunicFinancas.NewVar %>% 
      gather(ContasPublica_Nome, MunicFinancas_ContaValor, 3)
    
    MunicFinancas <- bind_rows(MunicFinancas, MunicFinancas.NewVar.Format)
    
    # Libera memória
    rm(j, DeParaFinbra.Select, MunicFinancas.NewVar)
    rm(MunicFinancas.NewVar.Format)
    gc()
  }
  rm(i, years_extract)
  
  # Verifica os dados extraídos;
  # names(MunicFinancas)
  # dim(MunicFinancas)
  # table(MunicFinancas$MunicFinancas_Ano)
  # table(MunicFinancas$ContasPublica_Nome)
  # head(MunicFinancas, n = 10)
  # tail(MunicFinancas, n = 10)
  # View(MunicFinancas)
  
  # Verifica repetições no banco.
  # x <- table(MunicFinancas$Munic_Id, MunicFinancas$MunicFinancas_Ano) %>% as.data.frame()
  # x[x$Freq > 12,]
  # View(x)
  # table(x$Freq)
  
  #  test <-  MunicFinancas[MunicFinancas$Munic_Id %in% x$Var1[x$Freq > 8],] %>% arrange(Munic_Id, MunicFinancas_Ano)
  # View(test)
  # rm(test, x)
  
  ## Obtem o código das contas financeiras.
  ContasPublicas.Select <- ContasPublicas %>% 
    mutate(ContasPublica_Id = as.character(ContasPublica_Id)) %>% 
    select(ContasPublica_Nome, ContasPublica_Id)
  
  # Obtem o total de colunas.
  totalColunas <- ncol(MunicFinancas)
  
  # Substitui o nome das contas pelos códigos
  MunicFinancas.Short <- MunicFinancas %>% 
    # Acrecenta o código das contas públicas.
    left_join(ContasPublicas.Select, by = "ContasPublica_Nome") %>% 
    # Seleciona as colunas da base de dados final.
    select(Munic_Id, MunicFinancas_Ano, ContasPublica_Id, MunicFinancas_ContaValor)
  
  # names(MunicFinancas.Short)
  # head(MunicFinancas.Short)
  # table(MunicFinancas.Short$ContasPublica_Id)
  
  
  return(MunicFinancas.Short)
}



# End