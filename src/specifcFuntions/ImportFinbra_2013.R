# Script to extract data from raw files of Finbra, Finances of Brasil (Finanças do Brasil) 
# from the new pattern after 2013.

# Raw data source: STN (Secretaria do Tesouro Nacional, National Treasure Department)

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Script creation data: 2019-09-16.


# Dependencies

library(dplyr)
library(data.table)

################## External Functions ##################

source("src/generalFunctions/unzipTemp.R")

################## Function ##################


# Parameters:

# ListData: List of data frames
# FileNameXlsx: Name os the xlsx file

# Debug:
# years_extract = 2016:2013
# ExtractAccounts <- as.character(ContasPublicas$ContasPublica_Id)
# CsvFiles = CsvFiles
# ContasPublicas = ContasPublicas
# InputFolder = "data/raw/Finbra/OriginalFiles/"

ImportFinbra_2013 <- function(years_extract, ExtractAccounts, CsvFiles, ContasPublicas, 
                              InputFolder) {
  
  # Prevent class error
  ContasPublicas <- mutate(ContasPublicas, ContasPublica_Id = as.character(ContasPublica_Id))
  
  # Filter selected accounts
  FitlerAccounts <- ContasPublicas %>% 
    dplyr::filter(ContasPublica_Id %in% ExtractAccounts) 
  
  # Filter Conditions
  FilterRev <- any(FitlerAccounts$ContasPublica_RD %in% "r") # Filter Revenue accounts
  FilterSpen <- any(FitlerAccounts$ContasPublica_RD %in% "d") # Filter Spending accounts
  FilterEl <- any(FitlerAccounts$ContasPublica_ElFunc %in% "E") # Filter Elements accounts
  FilterFunc <- any(FitlerAccounts$ContasPublica_ElFunc %in% "F") # Filter Spend accounts
  
  # Select Files according year and accounts to be extracted
  Extrac_Files <- CsvFiles %>% 
    # Filter by year
    dplyr::filter(FinRawFiles_InitialYear >= min(years_extract)) %>% 
    dplyr::filter(FinRawFiles_FinalYear <= max(years_extract)) %>% 
    # Filter by filter conditions
    mutate(FilterVar = 0) %>% 
    mutate(FilterVar = ifelse(FilterRev & FinRawFiles_DataType == "Rev", 1, FilterVar)) %>% 
    mutate(FilterVar = ifelse(FilterEl & FilterSpen & FinRawFiles_DataType == "SpenEl", 1, FilterVar)) %>% 
    mutate(FilterVar = ifelse(FilterFunc & FinRawFiles_DataType == "SpenFunc", 1, FilterVar)) %>% 
    dplyr::filter(FilterVar == 1)
  
  rm(FilterRev, FilterSpen, FilterEl, FilterFunc)
  
  MunicFinancas <- tibble() # gathering all fitantial data
  
  # Extrac data by file.
  for (i in seq_len(nrow(Extrac_Files))) {
    # i <- 5
    
    message("Extract data from file ", Extrac_Files$FinRawFiles_FileName[i])
    
    rawfile <- unzipTemp(paste0(InputFolder, Extrac_Files$FinRawFiles_FileName[i]))
    
    # Removing first 3 rows (source, data, file)
    df_temp <- fread(rawfile, skip = 3, 
                     sep = ";", dec = ",",
                     #quote="",
                     colClasses = c("factor", "integer", "factor", "integer", "factor", "factor", "numeric"))
    
    # Deleta arquivo csv descompactado
    unlink(rawfile)
    rm(rawfile)
    
    # Formating data
    df_temp <- df_temp %>% 
      # Insert year
      mutate(MunicFinancas_Ano = Extrac_Files$FinRawFiles_InitialYear[i]) %>%
      # Rename variables
      rename(Munic_Id = Cod.IBGE) %>% 
      rename(MunicFinancas_ContaValor = Valor) %>%
      # Cread Account ID
      mutate(ContasPublica_Id = gsub("\\.", "", Conta)) %>% 
      mutate(ContasPublica_Id = substr(ContasPublica_Id, 1, 8))
    
    
    
    MunicFinancas.New <- df_temp %>% 
      # Filter only raised funds or paided speding 
      dplyr::filter(Coluna %in% c("Receitas Realizadas", "Receitas Brutas Realizadas", "Despesas Pagas")) %>%
      mutate(Teste1 = str_detect(Conta, "(?i)Total")) %>% 
      mutate(Teste2 = str_detect(Conta, "(?i)Despesa")) %>% 
      mutate(Teste3 = str_detect(Conta, "(?i)Receita")) %>% 
      mutate(ContasPublica_Id = ifelse(Teste1 & Teste3,
                                       "71200000000", ContasPublica_Id)) %>%
      mutate(ContasPublica_Id = ifelse(Teste1 & Teste2,
                                       "73400000000", ContasPublica_Id)) %>%
      # Selectind variables
      dplyr::select(Munic_Id, MunicFinancas_Ano, ContasPublica_Id,
                    MunicFinancas_ContaValor) %>%
      # Filtering selected accounts
      dplyr::filter(ContasPublica_Id %in% ExtractAccounts) 
      
    
    # In Revenues, we must extrac revenue deductions
    if (Extrac_Files$FinRawFiles_DataType[i] == "Rev") {
      
      # Filtering and formating deductions
      Deductions <- df_temp %>%
        dplyr::filter(Coluna == "Deduções da Receita") %>% 
        dplyr::filter(ContasPublica_Id == "10000000") %>% 
        mutate(ContasPublica_Id = "90000000") %>% 
        select(Munic_Id, MunicFinancas_Ano, ContasPublica_Id,
               MunicFinancas_ContaValor)
      
      MunicFinancas.New <- bind_rows(MunicFinancas.New, Deductions) %>% 
        arrange(MunicFinancas_Ano, Munic_Id, ContasPublica_Id)
      rm(Deductions)
    }
    
    
    MunicFinancas <- bind_rows(MunicFinancas, MunicFinancas.New)
    rm(MunicFinancas.New, df_temp)
    gc()
  }
  return(MunicFinancas)
}



# End