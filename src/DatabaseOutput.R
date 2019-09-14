
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# # This script: create a tidy relational dataset from raw data

# By Murilo Junqueira and Carla Bezerra

# Created at 2018-05-09

################## Setup working space ##################


############### Creating tidy relational dataset ###############

# Importing Participatory Budget Census
source("src/specifcFuntions/ImportPBData.R")

############### Import public finance data ###############

# Workflow

## Import ExternalData: FinantialRawFiles.csv
## Function: download all zip Files
## Function: extrac xlsx Files from zip files
## Function: Create BDCamposFinbra File
## Import ExternalData: DeParaFinbra.csv
## Import ExternalData: ContasPublicas.csv
## Function: Extract data to MunicFinancas (according to each year)


# Import ExternalData: Raw data source (URLs)
FinantialRawFiles <- fread("data/dataset/FinantialRawFiles.csv", 
                           sep = ";", dec = ",", stringsAsFactors = FALSE)


# Select only raw files that uses Access databases (accdb)
AccessRawFiles <- FinantialRawFiles %>% 
  dplyr::filter(FinRawFiles_FormatRawFile == "accdb")

# Select only raw files that uses csv format
CsvFiles <- FinantialRawFiles %>% 
  dplyr::filter(FinRawFiles_FormatRawFile == "csv")



## Function: download all zip Files
source("src/generalFunctions/DownloadFiles.R")

DownloadFiles(urls = FinantialRawFiles$FinRawFiles_FileURL, 
              fileNames = FinantialRawFiles$FinRawFiles_FileName, 
              DirOutput = "data/raw/Finbra/OriginalFiles/", 
              Override = FALSE)

rm(DownloadFiles)



## Function: extract xlsx Files from zip files
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

# Function to list all fields in MS Excel list files
source("src/generalFunctions/ListFieldsExcel.R")

ListFieldsExcel_df <- ListFieldsExcel(paste0("data/raw/Finbra/ExcelFiles/", CsvFiles$FinRawFiles_FileXlsx))

rm(ListFieldsExcel)

BDCamposFinbra <- fread("data/dataset/BDCamposFinbra.csv", 
                        sep = ";", dec = ",", stringsAsFactors = FALSE)




# Importing Participatory Budget Census
source("src/specifcFuntions/ImportFinantialData.R")




############### Importing Electoral Data ###############

# Importing Electoral Data
source("src/specifcFuntions/ImportElectoralData.R")



# End