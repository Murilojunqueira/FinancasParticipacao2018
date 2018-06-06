
# Script para extrair dados do BD para a análise das finanças municipais
# e do probabilidade de sobrevivência do Orçamento Participativo.

# Criado por Murilo Junqueira

# Data criação: 2018-05-09
# Ultima modificação: 2018-06-06

################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"



# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)
library(readstata13)
library(readxl)
library(scales)


################## Load Data ##################


Data.Analisys <- fread(paste0(InputFolder, "Data.Analisys.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)



################## Models ##################


# Codebook for papers variables:

## Adopt.pb = Adoption of pb,  Dependent Variable of models 1 and 2
## Abandon.pb = Adandon of pb, Dependent Variable of models 3 and 4
## VictoryPTAfter202 = Victory of the PT before 2002 (discrete) * -1
## ChangeEffect2002 = Change in effect after 2002 (discrete)
## mindist == Minimum Distance
## ptwin = Change in effect after 2002 (?)
## taxrevenues == Tax share of revenues
## balsheetrev == Financial viability index
## continuitypartpref == City government continuity (discrete)
## MayorsVulnerability = Mayor's vulnerability
## MayorControlCouncil = Mayor controls the council (discrete)
## legprefpower == Mayor's share of council seats
## YearDummies1996 = Period 3 (1996-2000)
## YearDummies2000 = Period4 (2001-2004)
## YearDummies2004 = Period5 (2005-2008)
## YearDummies2008 = Period 6 (2009-2012)


## avoid city duplication
Data.Analisys <- Data.Analisys %>% 
  distinct(Munic_Id, year, .keep_all = TRUE)

names(Data.Analisys)

# Filter only municipalities with more than 50k pop.
Data.Analisys <- Data.Analisys %>% 
  filter(population > 50000)
  

# Check complete cases
Data.Analisys.Complete <- Data.Analisys %>% 
  select(Munic_Id, UF_Sigla, UF_Regiao, year, MunicOP_OP, Adopt.pb,
         Abandon.pb, MayorName, MayorElecNumber, MayorParty, ptwin, 
         ChangeEffect2002, VictoryPTAfter202, taxrevenues, balsheetrev, 
         continuitypartpref, MayorsVulnerability, legprefpower, 
         MayorControlCouncil, YearDummies1996, YearDummies2000, YearDummies2004, 
         YearDummies2008, YearDummies2012, population) %>% na.omit
  

# Cases by year  
table(Data.Analisys.Complete$year)


# I will ommit the variable mindist for now in order to include the 2012 election

# The first models is about the chance of exist pb in a given year (dependent variable is the existence of pb)

# Run basic model
LPM.pb <- lm(MunicOP_OP ~ VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
               YearDummies2012, 
             data = Data.Analisys)

summary(LPM.pb)


# Now with population (log) as explanatory variable

LPM.pb <- lm(MunicOP_OP ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
               YearDummies2012, 
             data = Data.Analisys)

summary(LPM.pb)


# With lag dependent variable

Data.Analisys.lag <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>% 
  mutate(lag.pb = lag(MunicOP_OP))


LPM.pb <- lm(MunicOP_OP ~ lag.pb + log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
               YearDummies2012, 
             data = Data.Analisys.lag)

summary(LPM.pb)


# With investment variables

LPM.pb <- lm(MunicOP_OP ~ lag.pb + log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + InvestPer + Investpp +
               YearDummies2000 + YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.lag)

summary(LPM.pb)




# Now the chance of adoption of pb

## Logicaly it is necessary exclude the cases that the municipality have pb in t-1
## (it is not possible adopt pb if it is already adopted).

Data.Analisys.adopt <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>%
  mutate(AlreadyAdopted = ifelse(Munic_Id == lag(Munic_Id) & MunicOP_OP == 1 & lag(MunicOP_OP) == 1, 1, 0)) %>% 
           filter(AlreadyAdopted == 0)

LPM.Adopt <- lm(Adopt.pb ~ VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                  YearDummies2012, 
                data = Data.Analisys.adopt)

summary(LPM.Adopt)


# Now with population (log) as explanatory variable

LPM.Adopt <- lm(Adopt.pb ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                  YearDummies2012, 
                data = Data.Analisys.adopt)

summary(LPM.Adopt)



# With investment variables

LPM.Adopt <- lm(Adopt.pb ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + InvestPer + Investpp +
               YearDummies2000 + YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.adopt)

summary(LPM.Adopt)




# The Chance of abandon of pb (version 1)

LPM.Abandon <- lm(Abandon.pb ~ + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                    taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                    YearDummies2012, 
                  data = Data.Analisys)

summary(LPM.Abandon)


# Now with population (log) as explanatory variable:

LPM.Abandon <- lm(Abandon.pb ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                    taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                    YearDummies2012, 
                  data = Data.Analisys)

summary(LPM.Abandon)

# The Chance of abandon of pb (version 2)

## Logicaly it is not possible abandon pb if the municipality doens't previously adopted pb

# Delete municipalities that doens't previously adopted pb
Data.Analisys.abandon <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>%
  filter(Abandon.pb == 1 | MunicOP_OP == 1)

  
table(Data.Analisys.abandon$year)


LPM.Abandon <- lm(Abandon.pb ~ VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                    taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                    YearDummies2012, 
                  data = Data.Analisys.abandon)

summary(LPM.Abandon)


# Now with population (log) as explanatory variable:

LPM.Abandon <- lm(Abandon.pb ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                    taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2008 + 
                    YearDummies2012, 
                  data = Data.Analisys.abandon)


summary(LPM.Abandon) # This model have the best fit of all above



# With investment variables

LPM.Abandon <- lm(Abandon.pb ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + InvestPer + Investpp +
                  YearDummies2000 + YearDummies2008 + YearDummies2012, 
                data = Data.Analisys.abandon)

summary(LPM.Abandon)



# Clear memory
rm(LPM.Abandon, LPM.Adopt)


################## Tabelas ##################




################## Sobras ##################





# End