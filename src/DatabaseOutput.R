
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# # This script: create a tidy relational dataset from raw data

# By Murilo Junqueira and Carla Bezerra

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

# UnzipFiles in CSV format (after 2013)
source("src/generalFunctions/unzipTemp.R")

# Unizip Files
for (i in seq_len(nrow(CsvFiles))) {
  # i <- 1
  message("Extracting ", paste0("data/raw/Finbra/OriginalFiles/", CsvFiles$FinRawFiles_FileName[i]))
  # UnzipingFile
  rawfile <- unzipTemp(paste0("data/raw/Finbra/OriginalFiles/", CsvFiles$FinRawFiles_FileName[i]))
  # Removing first 3 rows (source, data, file)
  df_temp <- fread(rawfile, skip = 3)
  fwrite(x = df_temp,
              file = paste0("data/raw/Finbra/ExcelFiles/", CsvFiles$FinRawFiles_FileXlsx[i]),
              sep = ";", dec = ",")
  rm(df_temp)
  }

rm(unzipTemp, rawfile, i)
gc()


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

ContasPublicas <- fread("data/dataset/ContasPublicas.csv",
                      sep = ";", dec = ",", stringsAsFactors = FALSE)

DeParaUGCodIBGE <- fread("data/dataset/DeParaUGCodIBGE.csv", 
                         sep = ";", dec = ",", stringsAsFactors = FALSE)

Municipios <- fread("data/dataset/Municipios.csv", 
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
                                          InputFolder = "data/raw/Finbra/ExcelFiles/", 
                                          OutputFolder = "data/dataset/")


rm(ImportFinbra_89_2012, FormatFinbra, MuncCod6To7, PartialMatchFilter)


# Importing data from 1989 to 2012
source("src/specifcFuntions/ImportFinbra_2013.R")






# Importing Participatory Budget Census
# source("src/specifcFuntions/ImportFinantialData.R")




############### Importing Electoral Data ###############

# Importing Electoral Data
# source("src/specifcFuntions/ImportElectoralData.R")



# End