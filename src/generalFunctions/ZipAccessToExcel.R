
# Import a List of dataframes to Excel workbook

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-18

# Dependencies

library(openxlsx)
library(readr)

# External dependency: Microsoft Access Database Engine
# More information on "src/generalFunctions/GetMSAccessData.R"


################## External Functions ##################

source("src/generalFunctions/unzipTemp.R")
source("src/generalFunctions/GetMSAccessData.R")

################## Function ##################


# Parameters:

# ListData: List of data frames
# FileNameXlsx: Name os the xlsx file

# Debug:
# FileRaw = "data/raw/TSE/TSE-relacao-candidatos-1996/TSE-relacao-candidatos-1996.zip"
# FileNameXlsx = "data/raw/TSE/TSE-relacao-candidatos-1996/Candidatos_1996.xlsx"
# TableNameVar = "TABLE_NAME"
# tableType = "TABLE"
# Override = TRUE
 
# ZipAccessToExcel(FileRaw, FileNameXlsx, TableNameVar, tableType, TableNameFilter, Override)


ZipAccessToExcel <- function(FileRaw, FileNameXlsx, TableNameVar, tableType, TableNameFilter = ".", 
                             Override = FALSE) {
  
  
  # Avoid error "File doesn't exists".
  if(!file.exists(FileRaw)) {
    message("FileRaw ", FileRaw, " does not exists")
    return()
  }
  
  # Stops if FileNameXlsx already exists and Override = FALSE
  if(!isTRUE(Override) & file.exists(FileNameXlsx)) {
    message(FileNameXlsx, " already exists")
    return()
  }
  
  message("Processing File ", FileRaw)
  
  # Unzip file
  Rawfile <- unzipTemp(FileRaw)
  
  # Extract all information from MS Access file
  RawData <- GetMSAccessData(file = Rawfile, 
                             TableNameVar = TableNameVar, 
                             tableType = tableType)
  
  # Delete unziped file
  unlink(Rawfile)
  
  # Select only relevant tables
  DataTables <- RawData[grep(TableNameFilter, names(RawData), value = TRUE)]
  
  # Remove NA elementes (GetMSAccessData return NA for error in extract table)
  DataTables <- DataTables[!is.na(DataTables)]
  
  message("Salvando arquivo ", FileNameXlsx)
  
  # Convert sheet names to ASCII; "write.xlsx" doesn't work otherwise.
  names(DataTables) <- gsub(" ", "_", names(DataTables))
  names(DataTables) <- iconv(names(DataTables),
                             from = readr::guess_encoding(names(DataTables))[[1]], 
                             to = "ASCII//TRANSLIT")

  # names(RawData)
  # names(DataTables)
  
  # Write File with all tables
  write.xlsx(DataTables, file = FileNameXlsx)
  }
# End