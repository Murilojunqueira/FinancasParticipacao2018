
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import data about participatory budgeting

# Original data from Spada(2012)

# By Murilo Junqueira e Carla Bezerra

# Created at 2018-03-09

# Data Documentation
# https://participedia.net/en/content/brazilian-participatory-budgeting-census

############### Importing Participatory Budget Census ###############

library(readxl)


ImportPBData <- function(RawData, SheetName) {
  
  OP_Data <- read_excel(RawData, 
                        sheet = SheetName)
  
  # Check Dataset
  # names(OP_Data)
  # View(OP_Data)
  
  # Extracting and format data
  OP_DataSelect <- OP_Data %>%
    # Select only columns about municipality identification and pb
    # year data are mixed with pb data
    select(codeipea, starts_with("pb")) %>%
    mutate(pb2016= ifelse(is.na(pb2016), 0, pb2016)) %>% 
    # transform to short-style dataset (fixed number of columns)
    gather(MunicOp_Ano, MunicOP_OP, starts_with("pb")) %>% 
    # Rename variable
    rename(Munic_Id = codeipea) %>% 
    # Remove "total" lines
    dplyr::filter(!is.na("Munic_Id")) %>% 
    # Create year variable
    mutate(MunicOp_Ano = as.integer(sub("pb", "", MunicOp_Ano))) %>% 
    # Arrange dataset by city and year
    arrange(Munic_Id, MunicOp_Ano)
  
  
  # Complete message
  message("Script Finished \n\nPB data imported\n\n")
  
  return(OP_DataSelect)
  
}

# End