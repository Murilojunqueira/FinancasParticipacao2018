
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Create tables Partidos "Parties".

# Documentação do pacote de extração dos dados do IPEA Data:
# https://cran.r-project.org/web/packages/ipeadatar/ipeadatar.pdf

# Site do IPEA Data:
# http://www.ipeadata.gov.br/Default.aspx

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-03-15


# Dependencies
library(ipeadatar) # IPEAData API
library(stringr)
library(lubridate)

############### Searching for series ###############

# Series <- available_series(language = c("br"))
# View(Series)
# names(Series)
# table(Series$theme)
# 
# names(Filtra)
# table(Filtra$theme)
# 
# Filtra <- Series %>%
#   mutate(flag = str_detect(name, "PIB")) %>%
#   dplyr::filter(flag == TRUE) %>% 
#   dplyr::filter(freq == "Anual") %>% 
#   dplyr::filter(theme == "Regional") %>% 
#   select(everything())


############### Importing Socioeconomic Data from IPEAData ###############

# Debug:
# CodeVar <- "ESTIMA_PO"
# VarName <- "SocioMunic_Populacao"
# Territory <- "Municípios"
# TerritoryVarName <- "Munic_Id"
# TimeAgregattion <- "year"
# TimeAgregattionName <- "SocioMunic_Ano"

ImportIPEAData <- function(CodeVar, VarName, Territory, TerritoryVarName,
                           TimeAgregattion = c("year"), TimeAgregattionName) {
  
  message("Importing data from IPEAData")
  
  RawData <- ipeadata(CodeVar, language = "br")
  
  message("Processing Data")
  
  Output_df <- RawData %>% 
    mutate(uname2 = as.character(uname)) %>% 
    dplyr::filter(uname2 == "Municípios") %>% 
    rename(Place_TerritoryVarName = tcode) %>%
    rename(Place_VarName = value) %>%
    mutate(temp = ymd(date)) %>% 
    # dplyr::filter(SocioMunic_Ano >= 1996) %>% 
    select(Place_TerritoryVarName, temp, Place_VarName)
  
  if(TimeAgregattion == "year") {
    Output_df <- Output_df %>% 
      mutate(temp = lubridate::year(temp))
  }
  
  names(Output_df) <- c(TerritoryVarName, TimeAgregattionName, VarName)
  
  return(Output_df)
}