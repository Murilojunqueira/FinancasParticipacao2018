
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

## This script creates a tidy relational dataset from raw data of many sources

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-05-09

################## Setup working space ##################


############### 1 - Importing Participatory Budget Census ###############

# Importing Participatory Budget Census
source("src/specifcFuntions/ImportPBData.R")

############### 2 - Import public finance data ###############

# Workflow

## Import ExternalData: FinantialRawFiles.csv
## Function: download all zip Files
## Function: extrac xlsx Files from zip files
## Function: Create BDCamposFinbra File
## Import ExternalData: DeParaFinbra.csv
## Import ExternalData: ContasPublicas.csv
## Function: Extract data to MunicFinancas (according to each year)

############### 2.1 - Import ExternalData: Raw data source (URLs) ###############

FinantialRawFiles <- fread("data/dataset/FinantialRawFiles.csv", 
                           sep = ";", dec = ",", stringsAsFactors = FALSE)

# Select only raw files that uses Access databases (accdb)
AccessRawFiles <- FinantialRawFiles %>% 
  dplyr::filter(FinRawFiles_FormatRawFile == "accdb")

# Select only raw files that uses csv format
CsvFiles <- FinantialRawFiles %>% 
  dplyr::filter(FinRawFiles_FormatRawFile == "csv")


############### 2.2 - Function: download all zip Files ###############

source("src/generalFunctions/DownloadFiles.R")

DownloadFiles(urls = FinantialRawFiles$FinRawFiles_FileURL, 
              fileNames = FinantialRawFiles$FinRawFiles_FileName, 
              DirOutput = "data/raw/Finbra/OriginalFiles/", 
              Override = FALSE)

rm(DownloadFiles)

############### 2.3 - Function: extract data Files from zip files ###############

source("src/generalFunctions/ZipAccessToExcel.R")

# Extract all files from AccessRawFiles to xlsx files
for(i in seq_len(nrow(AccessRawFiles))) {
  
  ZipAccessToExcel(FileRaw = paste0("data/raw/Finbra/OriginalFiles/", AccessRawFiles$FinRawFiles_FileName[i]),
                   FileNameXlsx = paste0("data/raw/Finbra/ExcelFiles/", AccessRawFiles$FinRawFiles_FileXlsx[i]),
                   TableNameVar = "TABLE_NAME", 
                   tableType = "TABLE",
                   Override = FALSE)
}

rm(ZipAccessToExcel, unzipTemp, GetMSAccessData, i)


############### 2.4 - Create BDCamposFinbra ###############

# Function to list all fields in MS Excel list files
source("src/generalFunctions/ListFieldsExcel.R")
ListFieldsExcel_df <- ListFieldsExcel(paste0("data/raw/Finbra/ExcelFiles/", AccessRawFiles$FinRawFiles_FileXlsx))
rm(ListFieldsExcel)


source("src/specifcFuntions/Create_BDCamposFinbra.R")
Create_BDCamposFinbra(ListFieldsExcel_df = ListFieldsExcel_df, 
                      OutputDir = "data/dataset/")


rm(ListFieldsExcel_df, Create_BDCamposFinbra)


############### 2.5 - Import ExternalData  ###############

BDCamposFinbra <- fread("data/dataset/BDCamposFinbra.csv",
                        sep = ";", dec = ",", stringsAsFactors = FALSE,
                        encoding = "UTF-8")

DeParaFinbra <- fread("data/dataset/DeParaFinbra.csv",
                      sep = ";", dec = ",", stringsAsFactors = FALSE)

DeParaUGCodIBGE <- fread("data/dataset/DeParaUGCodIBGE.csv", 
                         sep = ";", dec = ",", stringsAsFactors = FALSE)

Municipios <- fread("data/dataset/Municipios.csv", 
                    sep = ";", dec = ",", stringsAsFactors = FALSE)

ContasPublicas <- fread("data/dataset/ContasPublicas.csv",
                        sep = ";", dec = ",", stringsAsFactors = FALSE)


############### 2.6 - Function: Extract data to MunicFinancas (according to each year) ###############

# Importing data from 1989 to 2012
source("src/specifcFuntions/ImportFinbra_89_2012.R")

Finbra_89_2012.df <- ImportFinbra_89_2012(years_extract = 2012:1994,  
                                          ContasPublicas = ContasPublicas, 
                                          DeParaFinbra = DeParaFinbra, 
                                          BDCamposFinbra = BDCamposFinbra, 
                                          DeParaUGCodIBGE = DeParaUGCodIBGE, 
                                          Municipios = Municipios, 
                                          InputFolder = "data/raw/Finbra/ExcelFiles/")


# Importing data after 2013
source("src/specifcFuntions/ImportFinbra_2013.R")

# Import raw data url. Select only raw files that uses csv format.
CsvFiles <- fread("data/dataset/FinantialRawFiles.csv", 
                  sep = ";", dec = ",", stringsAsFactors = FALSE) %>% 
  dplyr::filter(FinRawFiles_FormatRawFile == "csv")


Finbra_After2013.df <- ImportFinbra_2013(years_extract = 2016:2013,
                                       ExtractAccounts = as.character(ContasPublicas$ContasPublica_Id),
                                       CsvFiles = CsvFiles,
                                       ContasPublicas = ContasPublicas,
                                       InputFolder = "data/raw/Finbra/OriginalFiles/")

# Bind data before and after 2013
MunicFinancas <- bind_rows(Finbra_89_2012.df, Finbra_After2013.df) %>% 
  distinct(Munic_Id, MunicFinancas_Ano, ContasPublica_Id, .keep_all = TRUE)

# Write File
fwrite(MunicFinancas, 
       file = "data/dataset/MunicFinancas.csv", 
       sep = ";", dec = ",")

rm(ImportFinbra_89_2012, FormatFinbra, MuncCod6To7, PartialMatchFilter, Finbra_89_2012.df)
rm(ImportFinbra_2013, CsvFiles, Finbra_After2013.df, unzipTemp)
rm(MunicFinancas)
rm(Municipios, BDCamposFinbra, ContasPublicas, DeParaFinbra, DeParaUGCodIBGE)



############### 3 - Importing Electoral Data ###############

############### 3.1 - Importing Votes Data ###############

# Electoral zone code
UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", 
                     sep = ";", dec = ",",
                     stringsAsFactors = FALSE)

# Standarize electoral situation
DeParaSitEleitoral <- fread("data/dataset/DeParaSitEleitoral.csv", 
                            sep = ";", dec = ",")

# Importing Electoral Data
source("src/specifcFuntions/ImportElectoralData.R")
DadosCandidatos_df <- ImportElectoralData(yearsExtract = c(2016, 2012, 2008, 2004), 
                    InputFolder = "data/raw/CepespData/",
                    DeParaSitEleitoral = DeParaSitEleitoral)


source("src/specifcFuntions/ImportElectoralDataTSE.R")
ElectoralData2000_df <- ImportElectoralDataTSE(InputFolders = "data/raw/TSE/2000/",
                                          UEToCodIBGE = UEToCodIBGE, 
                                          DeParaSitEleitoral = DeParaSitEleitoral)


source("src/specifcFuntions/ElectoralData1996.R")
ElectoralData1996_df <- ElectoralData1996(InputFile1T = "data/raw/TSE/TSE-relacao-candidatos-1996/Candidatos_1996.xlsx", 
                                          InputFile2T = "data/raw/TSE/TSE-relacao-candidatos-1996/Resultado_da_Eleição_(Por_Municipio)_1996.csv", 
                                          UEToCodIBGE = UEToCodIBGE)

# Gather all data
DadosCandidatos <- DadosCandidatos_df %>% 
  bind_rows(ElectoralData2000_df) %>% 
  bind_rows(ElectoralData1996_df)

# Write File
fwrite(DadosCandidatos, 
       file = "data/dataset/CandidatoAno.csv", 
       sep = ";", dec = ",")

# Free Memory
rm(DadosCandidatos, DadosCandidatos_df, ElectoralData2000_df, ElectoralData1996_df, UEToCodIBGE)
rm(ImportElectoralData, ImportElectoralDataTSE, ElectoralData1996, DeParaSitEleitoral)

############### 3.2 - Importing Coalition Data ###############

# Electoral zone code
UEToCodIBGE <- fread("data/dataset/UEToCodIBGE.csv", 
                     sep = ";", dec = ",",
                     stringsAsFactors = FALSE)


source("src/specifcFuntions/ImportCoalitionsCEPESPdata.R")

ImportCoalitionsCEPESPdata_df <- ImportCoalitionsCEPESPdata(years = c(2016, 2012, 2008, 2004), 
                             InputFolder = "data/raw/CepespData/", 
                             UEToCodIBGE = UEToCodIBGE)

#Due flaws in 2000 election data, a extraced data directly from TSE
source("src/specifcFuntions/ImportCoalitionDataTSE.R")

ImportCoalitionDataTSE_df <- ImportCoalitionDataTSE(InputFolder = "data/raw/TSE/2000/", 
                             UEToCodIBGE = UEToCodIBGE)

# Gather data from coalitions and their parties
ImportCoalitions <- bind_rows(ImportCoalitionsCEPESPdata_df[[1]],
                              ImportCoalitionDataTSE_df[[1]])

ImportCoalitionsParties <- bind_rows(ImportCoalitionsCEPESPdata_df[[2]],
                                     ImportCoalitionDataTSE_df[[2]])

# Write File
fwrite(ImportCoalitions, 
       file = "data/dataset/Coligacoes.csv", 
       sep = ";", dec = ",")

fwrite(ImportCoalitionsParties, 
       file = "data/dataset/ColigacoesPartidos.csv", 
       sep = ";", dec = ",")

rm(ImportCoalitionDataTSE, ImportCoalitionDataTSE_df, ImportCoalitions,UEToCodIBGE)
rm(ImportCoalitionsCEPESPdata, ImportCoalitionsCEPESPdata_df, ImportCoalitionsParties)

############### 3.3 - Importing Parties Data ###############


CandidatoAno <- fread("data/dataset/CandidatoAno.csv", 
                      sep = ";", dec = ",", 
                      stringsAsFactors = FALSE)


source("src/specifcFuntions/ImportPartiesdata.R")
ImportPartiesdata_df <- ImportPartiesdata(CandidatoAno)

fwrite(ImportPartiesdata_df, 
       file = "data/dataset/Partidos.csv", 
       sep = ";", dec = ",")


rm(CandidatoAno, ImportPartiesdata, ImportPartiesdata_df)



############### 4 - Importing Socioeconomic Data ###############



source("src/specifcFuntions/ImportIPEAData.R")

ImportIPEAData_pop <- ImportIPEAData(CodeVar = "ESTIMA_PO", 
                                     VarName = "SocioMunic_Populacao", 
                                     Territory = "Municípios", 
                                     TerritoryVarName = "Munic_Id",
                                     TimeAgregattion = "year", 
                                     TimeAgregattionName = "SocioMunic_Ano")


ImportIPEAData_pib <- ImportIPEAData(CodeVar = "PIB_IBGE_5938_37", 
                                     VarName = "SocioMunic_PIBCorrente", 
                                     Territory = "Municípios", 
                                     TerritoryVarName = "Munic_Id",
                                     TimeAgregattion = "year", 
                                     TimeAgregattionName = "SocioMunic_Ano")

# Merge Datasets
ImportIPEAData <- ImportIPEAData_pop %>% 
  left_join(ImportIPEAData_pib, by = c("Munic_Id", "SocioMunic_Ano"))

fwrite(ImportIPEAData, 
       file = "data/dataset/SocioDemoEconomia.csv", 
       sep = ";", dec = ",")

rm(ImportIPEAData, ImportIPEAData_pop, ImportIPEAData_pib)

# End