
# Script para extrair dados do BD para a análise das finanças municipais
# e do probabilidade de sobrevivência do Orçamento Participativo.

# Criado por Murilo Junqueira

# Data criação: 2018-05-09
# Ultima modificação: 2018-06-06

################## Setup Working Space ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
# InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
# OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
# ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


InputFolder <- "C:/Users/Murilo Junqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
OutputFolder <- "C:/Users/Murilo Junqueira/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "C:/Users/Murilo Junqueira/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)
library(zeligverse)
library(scales)


################## Load Data ##################


Data.Analisys <- fread(paste0(InputFolder, "Data.Analisys.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


################## Codebook ##################


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



################## Select and check data ##################


## avoid city duplication
Data.Analisys <- Data.Analisys %>% 
  distinct(Munic_Id, year, .keep_all = TRUE)

names(Data.Analisys)

# Filter only municipalities with more than 50k pop.
Data.Analisys <- Data.Analisys %>% 
  filter(population > 50000)


# Check complete cases
Data.Analisys.Complete <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>% 
  mutate(lag.pb = lag(MunicOP_OP)) %>% 
  filter(year > 1992) %>% 
  dplyr::select(-mindist, -GDP, -GDPpp) %>%
  na.omit()


map_int(Data.Analisys.Complete, function(x) sum(is.na(x), na.rm = TRUE))


# Cases by year  
table(Data.Analisys.Complete$year)


################## Models ##################



# I will ommit the variable mindist for now in order to include the 2012 election

# The first models is about the chance of exist pb in a given year (dependent variable is the existence of pb)

# Run basic model
LPM.pb <- lm(MunicOP_OP ~ VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2004 + 
               YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.Complete)

summary(LPM.pb)


# Now with population (log) as explanatory variable

LPM.pb <- lm(MunicOP_OP ~ log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2004 + 
               YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.Complete)

summary(LPM.pb)


# With lag dependent variable



LPM.pb <- lm(MunicOP_OP ~ lag.pb + log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + YearDummies2000 + YearDummies2004 + 
               YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.Complete)

summary(LPM.pb)


# With investment variables

LPM.pb <- lm(MunicOP_OP ~ lag.pb + log(population) + VictoryPTAfter202 + ChangeEffect2002 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
               MayorControlCouncil + legprefpower + InvestPer + Investpp +
               YearDummies2000 + YearDummies2004 + 
               YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.Complete)

summary(LPM.pb)



# Without after 2002 variables

LPM.pb <- lm(MunicOP_OP ~ lag.pb + log(population) + VictoryPTAfter202 + ptwin +
               taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
              legprefpower + InvestPer +
               YearDummies2000 + YearDummies2004 + YearDummies2008 + YearDummies2012, 
             data = Data.Analisys.Complete)

summary(LPM.pb)



################## check heteroscedasticity - Models ##################



lmtest::bptest(LPM.pb)  # Breusch-Pagan test
Data.Analisys.Complete$LPM.pb.residuals <- summary(LPM.pb)[["residuals"]]





x[["residuals"]]


# End