
# Script para criar algumas variáveis simples do paper

# Criado por Murilo Junqueira

# Data criação: 2018-06-11
# Ultima modificação: 2018-06-11

################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()



# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "C:/Users/Murilo Junqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
OutputFolder <- "C:/Users/Murilo Junqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "C:/Users/Murilo Junqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


# install.packages("tidyverse")
# install.packages("data.table")

# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)



################## Load Data ##################

# Basic Municipal Data
Municipios <- fread(paste0(InputFolder, "Municipios.csv"), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE)

# Data about candidates
CandidatoAno <- fread(paste0(InputFolder, "CandidatoAno.csv"), 
                      sep = ";", dec = ",",
                      stringsAsFactors = FALSE)



################## Extract vars ##################


# Council members by party
CouncilParty <- CandidatoAno %>%
  # filter only council candidates
  filter(CandAno_Cargo == "V") %>%
  # Filter only elected candidates
  filter(CandAno_SituacaoElec == 1 | CandAno_SituacaoElec == 5) %>%
  # Select relevant variables
  select(Munic_Id, CandAno_Ano, Partido_Sigla) %>%
  # Find number of seats by party
  group_by(Munic_Id, CandAno_Ano, Partido_Sigla) %>%
  summarise(SeatsNumber = n()) %>% 
  # Spread the results, in order to show each party in a different column
  group_by(Munic_Id, CandAno_Ano) %>%
  spread(Partido_Sigla, SeatsNumber) %>%
  # Remove NAs column
  select(-matches("<NA>"))

# Replace NAs by zeros.
CouncilParty[is.na(CouncilParty)] <- 0


# check data
names(CouncilParty)
head(CouncilParty)
table(CouncilParty$CandAno_Ano)


# Write dataset file
write.table(CouncilParty, file = paste0(InputFolder, "CouncilParty.csv"),
            sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)             
