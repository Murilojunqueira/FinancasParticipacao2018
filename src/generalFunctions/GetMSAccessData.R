# Get all information from MS Access file

# This functiom open and close concection with the file.

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-08-18

# Is necessary install MS access drives to use odbcConnectAccess2007 function. 
# Use link below to get 64bit drive: 
# https://www.microsoft.com/en-us/download/details.aspx?id=54920

# I get information about troubleshooting in odbcConnectAccess2007 in: 
# https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window


# tableType: a command to filter the type of object to return
# TableNameVar: name of the variable that shows the name of the tables after sqlTables command


# Dependencies
library(RODBC)
library(dplyr)

# Debug
# file <-  Rawfile

GetMSAccessData <- function(file, TableNameVar, tableType = NULL, verbose = TRUE) {
  
  # Concennect to AccessFile
  dta <- RODBC::odbcConnectAccess2007(file)   
  
  # get all tables names
  TablesNames <- RODBC::sqlTables(dta, tableType  = tableType)  %>% 
    select(TableNameVar) %>% 
    unlist() %>% as.character()
  
  AccessData <- list()
  
  for(i in seq_along(TablesNames)) {
    
    # i <- 1
    
    if(isTRUE(verbose)) message("Getting data from table ", TablesNames[i])
    
    AccessData[[TablesNames[i]]] <- try(RODBC::sqlFetch(channel = dta, 
                                                        sqtable = TablesNames[[i]],
                                                        as.is = TRUE))
    
    # Error Handling
    if(class(AccessData[[TablesNames[i]]]) == "try-error") {
      AccessData[[TablesNames[i]]] <- NA
      message("Table ", TablesNames[i], " cannot be accessed")
    }
    
  }
  # Close connection
  odbcClose(dta)
  
  return(AccessData)
}
# End
