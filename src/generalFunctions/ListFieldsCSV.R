# Function to list every field in a given CSV File List.
# Return a dataframe of names of files and fields

# Dependency: 

library(dplyr)
library(data.table)
library(stringi)


# Debug:
# FileList <- paste0("data/raw/Finbra/ExcelFiles/", CsvFiles$FinRawFiles_FileXlsx)
# teste <- ListFieldsCSV.R(FileList)



################## Function ListFieldsCSV.R  ##################

ListFieldsCSV <- function(FileList) {
  
  # Dataframe to set all information
  output <- data.frame()
  
  for(i in seq_along(FileList)) {
    
    # debug.
    # i <- 1
    
    # Stops if file doen't existis
    if(!file.exists(FileList[i])) {
      stop("File ", FileList[i], " doesn't exists")
    }
    
    message("Reading file ", FileList[i])
    
    # List to gather all information
    NewRow <- list()
    
    NewRow$FilePath <- FileList[i]
    
    # Sheet name and fields (headers) in the sheet
    NewRow$Sheet <- NA
    NewRow$Campos <- names(fread(FileList[i], nrows = 0))
    
    # Dataframe with FilePath, sheet name and fields names.
    NewRows <- as.data.frame(NewRow$Campos) %>% 
      mutate(FilePath = NewRow$FilePath) %>% 
      mutate(sheet = NewRow$Sheet) %>% 
      select(FilePath, sheet, everything()) %>% 
      rename(Campos = 3)
    
    # Separete filename from filepath
    NewRows <- NewRows$FilePath %>% 
      str_locate_all("/") %>% 
      map_int(function(x) {x[[length(x)]]}) %>% 
      substr(NewRows$FilePath, ., 1000) %>% 
      substr(2, 1000) %>% 
      as.data.frame() %>% 
      rename(FileName = 1) %>% 
      bind_cols(NewRows)
    
    # Gather all information of this file.
    output <- bind_rows(output, NewRows)
  }
  return(output)
}

# End
