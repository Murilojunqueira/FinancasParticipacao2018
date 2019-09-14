# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Create table that show all variables in each annual 
# local govermnent finantial dataset.

# Source: Secretaria do Tesouro Nacional (National Treasure Department), Brazil.

# By Murilo Junqueira

# Created at 2019-08-13


################## Build BDCamposFinbra.csv  ##################

# List every field name in all files in "data/raw/FinbraExcel" folder

# List all files in "data/raw/FinbraExcel" folder
FileList <- list.files("data/raw/FinbraExcel", pattern = "^Finbra*") %>% 
  paste0("data/raw/FinbraExcel/", .)

# Function to list all fields in MS Excel list files
source("src/generalFunctions/ListFieldsExcel.R")

# List columns in "data/raw/FinbraExcel" folder's files.
BDCamposFinbra <- ListFieldsExcel(FileList)

write.table(BDCamposFinbra, 
            file = "data/dataset/BDCamposFinbra.csv", 
            sep = ";", dec = ",",
            row.names = FALSE,
            fileEncoding = "UTF-8")



# Clean memory
rm(FileList, ListFieldsExcel)


# End