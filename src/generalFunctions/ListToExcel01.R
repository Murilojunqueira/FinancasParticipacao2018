# Import a List of dataframes to Excelworkbook

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-18

# Dependencies
library(xlsx)

# Parameters:

# ListData: List of data frames
# FileNameXlsx: Name os the xlsx file

library(openxlsx)

ListToExcel01 <- function(ListData, FileNameXlsx) {
  
  # Flag to set first loop
  FirstLoop <- TRUE
  
  message("Processing ", FileNameXlsx, " data.")
  
  # Delete previous file (if exists)
  if(isTRUE(FirstLoop)) {
    try(unlink(FileNameXlsx))
    FirstLoop <- FALSE
  }
  

  
  
  # Save each table in one different sheet
  for (j in seq_along(ListData)) {
    
    # j <- 3
    
    message("Saving Table ", names(ListData)[j])
    
    # Saving data in a xlsx file.
    write.xlsx(ListData[[j]], 
               file = FileNameXlsx, 
               sheetName = names(ListData)[j], 
               col.names = TRUE, 
               row.names = FALSE, 
               append = TRUE)
  }
  return()
}
