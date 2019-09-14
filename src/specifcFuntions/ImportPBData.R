
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import data about participatory budgeting

# Original data from Spada(2012)

# By Murilo Junqueira e Carla Bezerra

# Created at 2018-03-09

# Data Documentation
# https://participedia.net/en/content/brazilian-participatory-budgeting-census

############### Importing Participatory Budget Census ###############

library(readxl)

OP.Data <- read_excel("data/raw/PBCENSUS Spada/05112017_PB CENSUS 2016.xlsx", 
                      sheet = "Final census")

# Check Dataset
# names(OP.Data)
# View(OP.Data)


# Extracting and format data
OP.Data.Select <- OP.Data %>%
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

# Check dataset
# names(OP.Data.Select)
# head(OP.Data.Select)
# View(OP.Data.Select)

# Write file  
write.table(OP.Data.Select, 
            file = "data/dataset/MunicOp.csv", 
            sep = ";", dec = ",", 
            row.names = FALSE, append = FALSE)

# clean memory
rm(OP.Data.Select, OP.Data)

# Complete message
message("Script Finished \n\nPB data imported\n\n")


# End