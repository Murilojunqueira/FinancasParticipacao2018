
# Import a List of dataframes to Excelworkbook

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-18

# Dependencies
library(openxlsx)


################## External Functions ##################

source("src/generalFunctions/unzipTemp.R")
source("src/generalFunctions/GetMSAccessData.R")

################## Function ##################


# Parameters:

# ListData: List of data frames
# FileNameXlsx: Name os the xlsx file

# Debug:
# FileRaw = paste0("data/raw/Finbra/OriginalFiles/", FinantialRawFiles$FinRawFiles_FileName[1])
# FileNameXlsx = paste0("data/raw/Finbra/ExcelFiles/", FinantialRawFiles$FinRawFiles_FileXlsx[1])
# TableNameVar = "TABLE_NAME"
# tableType = "TABLE"
# TableNameFilter = "."
# Override = FALSE
# 
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
  
  # names(RawData)
  # names(DataTables)
  
  message("Salvando arquivo ", FileNameXlsx)

  # Write File with all tables
  write.xlsx(DataTables, file = FileNameXlsx)
  }
# End