
# Script para extrair dados do BD para a análise das finanças municipais
# e do probabilidade de sobrevivência do Orçamento Participativo.

# Criado por Murilo Junqueira

# Data criação: 2018-05-09
# Ultima modificação: 2018-05-13

################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"
SpadaFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/PBCENSUS Spada/"


# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)
library(readstata13)
library(readxl)
library(scales)



################## Funções úteis ##################

CharaterToNumeric <- function(x) {
  x <- sub(",", ".", x)
  x <- as.numeric(x)
  return(x)
}

CharaterToInteger <- function(x) {
  x <- sub(",", ".", x)
  x <- as.integer(x)
  return(x)
}

################## Load Data ##################


# Spada's Data
Spada.Data <- read.dta13(paste0(SpadaFolder, "PBCENSUS1989_2012data.dta"))


# Basic Municipal Data
Municipios <- fread(paste0(InputFolder, "Municipios.csv"), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE)

# Basic Data about Brazilian States
UFs <- fread(paste0(InputFolder, "UFs.csv"), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE)

# Data about Participatory Budget
MunicOp <- fread(paste0(InputFolder, "MunicOp.csv"), 
                 sep = ";", dec = ",",
                 stringsAsFactors = FALSE)


# Data about candidates
CandidatoAno <- fread(paste0(InputFolder, "CandidatoAno.csv"), 
                      sep = ";", dec = ",",
                      stringsAsFactors = FALSE)


# Data about Brazilian political parties
Partidos <- fread(paste0(InputFolder, "Partidos.csv"), 
                  sep = ";", dec = ",",
                  stringsAsFactors = FALSE)


# Finantial Data
MunicFinancas <- fread(paste0(InputFolder, "MunicFinancas.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


# Socioeconomic Data
SocioDemoEconomia <- fread(paste0(InputFolder, "SocioDemoEconomia.csv"), 
                           sep = ";", dec = ",",
                           stringsAsFactors = FALSE)


################## Contrução de Variáveis a partir de banco próprio ##################

# Empty data frame to gather all variables
Data.Analisys <- data.frame()

# Municipalities basic information (name, state and region)
Data.Analisys <- Municipios %>% 
  left_join(UFs, by = "UF_Id") %>% 
  select(Munic_Id, Munic_Nome, UF_Sigla, UF_Regiao)


# Free memory
rm(Municipios, UFs)

# De/Para das variáveis do banco e do paper:

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


#### Adopt.pb = Adoption of pb,  Dependent Variable of models 1 and 2
#### Abandon.pb = Adandon of pb, Dependent Variable of models 3 and 4

Data.Analisys <- Data.Analisys %>% 
  # Join basic municipal data
  right_join(MunicOp, by = "Munic_Id") %>% 
  # Translate variable to English
  rename(year = MunicOp_Ano) %>% 
  # Filter year range
  filter(year >= 1992 & year <= 2012) %>% 
  # Order the rows by muncipality and year
  arrange(Munic_Id, year) %>% 
  # Create the variable of adoption of participatory budget
  mutate(Adopt.pb = ifelse(Munic_Id == lag(Munic_Id) & MunicOP_OP == 1 & lag(MunicOP_OP) == 0, 1, 0)) %>% 
  # Corret the case of the municipalities the adopted pb in the first year of the series.
  group_by(Munic_Id) %>% 
  mutate(Adopt.pb = ifelse(year == min(year) & MunicOP_OP == 1, 1, Adopt.pb)) %>% 
  ungroup() %>% 
  # Create the variable of abandon of participatory budget
  mutate(Abandon.pb = ifelse(Munic_Id == lag(Munic_Id) & MunicOP_OP == 0 & lag(MunicOP_OP) == 1, 1, 0)) %>% 
  # Corret the caso of the first municipality of the dataset (that doesn't have lag)
  mutate(Abandon.pb = ifelse(is.na(Abandon.pb), 0, Abandon.pb))
  

# Free memory
rm(MunicOp)


#### VictoryPTAfter202 = Victory of the PT before 2002 (discrete) * -1

# Script to check the name and number of the party in each year
Parties.year <- data.frame()  

for(i in seq_len(nrow(Partidos))) {
  # i <- 1
  timeRange <- Partidos$Partido_AnoInicial[i]:Partidos$Partido_AnoFinal[i]
  
  for(j in timeRange) {
    # j <- timeRange[1]
    newRow <- list(Partido_Numero = Partidos$Partido_Numero[i],
                   year = j,
                   Partido_Sigla = Partidos$Partido_Sigla[i])
    
    Parties.year <- rbind(Parties.year, as.data.frame(newRow))
  }
}
rm(i, j, timeRange, newRow)


# Filtering only the elected mayors among all candidates
ElectedMayors <- CandidatoAno %>% 
  # Select mayor candidates
  filter(CandAno_Cargo == "PREFEITO") %>% 
  # In variable CandAno_SituacaoElec (electoral situation) 1 means elected candidate 
  filter(CandAno_SituacaoElec == 1) %>%
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  rename(MayorName = CandAno_Nome) %>%
  mutate(Partido_Id = as.integer(Partido_Id)) %>% 
  left_join(Parties.year, by = c("Partido_Id" = "Partido_Numero", "year" = "year")) %>% 
  # Translate variables
  rename(MayorParty = Partido_Sigla) %>% 
  rename(MayorElecNumber = CandAno_Numero) %>% 
  # Select relevant variables
  select(Munic_Id, year, MayorName, MayorElecNumber, MayorParty)



# Create the variable ptwin
Data.Analisys <- Data.Analisys %>% 
  left_join(ElectedMayors, by = c("Munic_Id", "year")) %>% 
  mutate(ptwin = ifelse(MayorParty == "PT", 1, 0))


# Free memory
rm(Partidos)


# ChangeEffect2002 = Change in effect after 2002 (discrete)

Data.Analisys <- Data.Analisys %>% 
  mutate(ChangeEffect2002 = ifelse(year > 2002, 1, 0))


# mindist == Minimum Distance

Distance.Data <- Spada.Data %>% 
  select(codeipea, year, mindist) %>%
  rename(Munic_Id = codeipea)
  

# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  left_join(Distance.Data, by = c("Munic_Id", "year")) 

rm(Distance.Data)



# ptwin = Change in effect after 2002 (?)
  ## I will assume that this variable is the interaction term between ptwin and ChangeEffect2002

Data.Analisys <- Data.Analisys %>% 
  mutate(VictoryPTAfter202 = ptwin * ChangeEffect2002)



# taxrevenues == Tax share of revenues
  ## taxrevenues == Tax revenue / (current revenue - current revenue deductions)
  ## taxrevenues == Receita Tributária / (Receitas Correntes - deduções de receitas correntes)

TaxShareRevenues <- MunicFinancas %>% 
  select(-MunicFinancas_Id) %>% 
  # Translade variable
  rename(year = MunicFinancas_Ano) %>% 
  # Prevent character/numeric intepretation problems
  mutate(ContasPublica_Id = as.integer(ContasPublica_Id)) %>%
  mutate(MunicFinancas_ContaValor = sub(",", ".", MunicFinancas_ContaValor)) %>% 
  mutate(MunicFinancas_ContaValor = as.numeric(MunicFinancas_ContaValor)) %>% 
  # Select the used accounts (Tax revenue, current revenue, current revenue deductions)
  filter(ContasPublica_Id == 10000000 | ContasPublica_Id == 11000000 | 
           ContasPublica_Id == 900000000) %>% 
  # Spread account variables
  spread(ContasPublica_Id, MunicFinancas_ContaValor) %>% 
  # Set friendly variable's names.
  rename(CurrentRevenue = "10000000", 
         TaxRevenues = "11000000",
         RevenueDeductions = "900000000") %>% 
  # Create taxrevenues variables
  mutate(taxrevenues = TaxRevenues / (CurrentRevenue - RevenueDeductions)) %>% 
  # Select relevant variables
  select(Munic_Id, year, taxrevenues)
  

# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  left_join(TaxShareRevenues, by = c("Munic_Id", "year")) 

# Free memory
rm(TaxShareRevenues)


# balsheetrev == Financial viability index
  ## balsheetrev == (Current Spending + Capital Spending) / (Current revenue - current revenue deductions + Capital Revenue)
  ## balsheetrev == (Receita Tributária + Receita de Capital) / (Receitas Correntes - deduções de receitas correntes + Receita de Capital)

BalanceBudget <- MunicFinancas %>% 
  select(-MunicFinancas_Id) %>% 
  # Translade variable
  rename(year = MunicFinancas_Ano) %>% 
  # Prevent character/numeric intepretation problems
  mutate(ContasPublica_Id = as.integer(ContasPublica_Id)) %>%
  mutate(MunicFinancas_ContaValor = sub(",", ".", MunicFinancas_ContaValor)) %>% 
  mutate(MunicFinancas_ContaValor = as.numeric(MunicFinancas_ContaValor)) %>% 
  # Select the used accounts (Current Spending, Capital Spending, Current revenue, current revenue deductions, Capital Revenue)
  filter(ContasPublica_Id == 10000000 | ContasPublica_Id == 20000000 |
           ContasPublica_Id == 900000000 | ContasPublica_Id == 30000000 |
           ContasPublica_Id == 40000000) %>% 
  # Spread account variables
  spread(ContasPublica_Id, MunicFinancas_ContaValor) %>% 
  # Set friendly variable's names.
  rename(CurrentRevenue = "10000000", 
         CapitalRevenue = "20000000",
         RevenueDeductions = "900000000",
         CurrentSpending = "30000000", 
         CapitalSpending = "40000000") %>% 
  # Create balsheetrev variable
  mutate(balsheetrev = (CurrentSpending + CapitalSpending) / (CurrentRevenue - RevenueDeductions + CapitalRevenue)) %>% 
  # Select relevant variables
  select(Munic_Id, year, balsheetrev)

# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  left_join(BalanceBudget, by = c("Munic_Id", "year")) 

# Free memory
rm(BalanceBudget)


# continuitypartpref == City government continuity (discrete)


ContinuityMayor <- CandidatoAno %>% 
  # Select mayor candidates
  filter(CandAno_Cargo == "PREFEITO") %>% 
  # In variable CandAno_SituacaoElec (electoral situation) 1 means elected candidate 
  filter(CandAno_SituacaoElec == 1) %>%
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  rename(MayorName = CandAno_Nome) %>%
  rename(MayorElecNumber = CandAno_Numero) %>% 
  # Prevent character/numeric intepretation problems
  mutate(Partido_Id = as.integer(Partido_Id)) %>% 
  # Order the dataset rows by municipality and year
  arrange(Munic_Id, year) %>% 
  # Create the continuitypartpref variable
  mutate(continuitypartpref = ifelse(Partido_Id == lag(Partido_Id) & Munic_Id == lag(Munic_Id), 1, 0)) %>% 
  # In the first year of the series, there is no continuity
  mutate(continuitypartpref = ifelse(Munic_Id != lag(Munic_Id), 0, continuitypartpref)) %>% 
  # Select relevant variables
  select(Munic_Id, year, continuitypartpref)


# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  left_join(ContinuityMayor, by = c("Munic_Id", "year")) 

# Free memory
rm(ContinuityMayor)


# MayorsVulnerability = Mayor's vulnerability

# Recicle the ElectedMayors dataset (above)

MayorsVul <- CandidatoAno %>% 
  # Select mayor candidates
  filter(CandAno_Cargo == "PREFEITO") %>% 
  # Filter only first round
  filter(CandAno_Turno == 1) %>%
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  rename(CandidateName = CandAno_Nome) %>%
  rename(CandidateNumber = CandAno_Numero) %>% 
  # Find the top two candidates
  group_by(Munic_Id, year) %>%
  arrange(Munic_Id, desc(CandAno_QtVotos)) %>% 
  slice(1:2) %>% 
  # Join the elected mayors dataset. Recicle the ElectedMayors dataset (above).
  left_join(ElectedMayors, by = c("Munic_Id", "year")) %>% 
  #filter(Munic_Id == 3550308) %>% 
  mutate(ElectedVotes.temp = ifelse(CandidateNumber == MayorElecNumber, CandAno_QtVotos, NA)) %>% 
  mutate(RunnerUpVotes.temp = ifelse(CandidateNumber != MayorElecNumber, CandAno_QtVotos, NA)) %>% 
  summarise(ElectedVotes = mean(ElectedVotes.temp, na.rm = TRUE),
            RunnerUpVotes = mean(RunnerUpVotes.temp, na.rm = TRUE)) %>% 
  mutate(MayorsVulnerability = RunnerUpVotes/ElectedVotes) %>% 
  # Select relevant variables
  select(Munic_Id, year, MayorsVulnerability)


# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  left_join(MayorsVul, by = c("Munic_Id", "year")) 

# Free memory
rm(MayorsVul)


# legprefpower == Mayor's share of council seats
# MayorControlCouncil = Mayor controls the council (discrete)

# Find the party that have the bigger number of seats in each election.
MajorCouncilParty <- CandidatoAno %>% 
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  # Test Municipality
  # filter(Munic_Id == 3516200) %>% 
  # Select Council candidates.
  filter(CandAno_Cargo == "VEREADOR") %>% 
  ## In 2012 the electoral situation code changed. The lines below revert this.
  mutate(CandAno_SituacaoElec = ifelse(year == 2012 & CandAno_SituacaoElec == 2, 1, CandAno_SituacaoElec)) %>% 
  mutate(CandAno_SituacaoElec = ifelse(year == 2012 & CandAno_SituacaoElec == 5, 2, CandAno_SituacaoElec)) %>%
  mutate(CandAno_SituacaoElec = ifelse(year == 2012 & CandAno_SituacaoElec == 3, 5, CandAno_SituacaoElec)) %>%
  # Select elected council members.
  ## 1 means elected by her own votes.
  ## 5 means elected by party votes.
  filter(CandAno_SituacaoElec == 1 | CandAno_SituacaoElec == 5) %>% 
  # Find the nunber of seats for each party
  group_by(Munic_Id, year, Partido_Id) %>% 
  summarise(PartySeats = n()) %>%
  # Find total number of seats
  group_by(Munic_Id, year) %>% 
  mutate(CityTotalSeats = sum(PartySeats)) %>% 
  # Party share of seats.
  mutate(PartyShareSeats = PartySeats/CityTotalSeats) %>% 
  # Find the share of mayors party. Recicle elected mayors dataset.
  left_join(ElectedMayors, by = c("Munic_Id", "year")) %>%
  mutate(legprefpower.temp = ifelse(Partido_Id == MayorElecNumber, PartyShareSeats, NA)) %>% 
  # Create the variables legprefpower and MayorControlCouncil
  summarise(legprefpower = mean(legprefpower.temp, na.rm = TRUE)) %>% 
  ## This line is for the case that mayors party doesn't have any seat in the council
  mutate(legprefpower = ifelse(is.nan(legprefpower), 0, legprefpower)) %>% 
  mutate(MayorControlCouncil = ifelse(legprefpower >= .5, 1, 0))
  
  
# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  select(-starts_with("MayorControlCouncil"), -starts_with("legprefpower")) %>% 
  left_join(MajorCouncilParty, by = c("Munic_Id", "year")) 

names(Data.Analisys)

# Free memory
rm(MajorCouncilParty)


# YearDummies*


YearDummies <-  factor(Data.Analisys$year)
YearDummies <- model.matrix(~YearDummies) %>% 
  as.data.frame() %>% 
  select(-matches("(Intercept)")) %>% 
  cbind(Data.Analisys) %>% 
  select(Munic_Id, year, starts_with("YearDummies"))

# Check data
names(YearDummies)

# Join data in the main dataset
Data.Analisys <- Data.Analisys %>% 
  select(-starts_with("YearDummies")) %>% 
  left_join(YearDummies, by = c("Munic_Id", "year")) 

# Check data
names(Data.Analisys)

rm(YearDummies)


# Population and GDP per capita

Economics.Data <- SocioDemoEconomia %>% 
  # Translate variables
  rename(year = SocioMunic_Ano) %>% 
  rename(population = SocioMunic_Populacao) %>% 
  rename(GDP = SocioMunic_PIB) %>% 
  #Prevent data type
  mutate(Munic_Id = CharaterToInteger(Munic_Id)) %>% 
  mutate(year = CharaterToInteger(year)) %>% 
  mutate(population = CharaterToInteger(population)) %>% 
  mutate(GDP = CharaterToNumeric(GDP)) %>% 
  # GDP per capita
  mutate(GDPpp = GDP / population) 

Data.Analisys <- Data.Analisys %>% 
  select(-starts_with("population"), -starts_with("GDP")) %>% 
  left_join(Economics.Data, by = c("Munic_Id", "year"))


# Check data
names(Data.Analisys)


# Write dataset file
write.table(Data.Analisys, file = paste0(OutputFolder, "Data.Analisys.csv"),
            sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

rm(Parties.year, ElectedMayors)


################## Regressões utilizando o banco de dados próprio ##################


# De/Para das variáveis do banco e do paper:

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

# Check complete cases

Data.Analisys.Complete <- Data.Analisys %>% 
  select(Munic_Id, UF_Sigla, UF_Regiao, year, MunicOP_OP, Adopt.pb,
         Abandon.pb, MayorName, MayorElecNumber, MayorParty, ptwin, 
         ChangeEffect2002, VictoryPTAfter202, taxrevenues, balsheetrev, 
         continuitypartpref, MayorsVulnerability, legprefpower, 
         MayorControlCouncil, YearDummies1996, YearDummies2000, YearDummies2004, 
         YearDummies2008, YearDummies2012, population) %>% na.omit
  
  
table(Data.Analisys.Complete$year)
View(Data.Analisys)

# I will ommit the variable mindist for now in order to include the 2012 election



# The first models is about the chance of exist pb in a given year (dependent variable is the existence of pb)

# Roda o modelo
LPM.pb <- lm(MunicOP_OP ~ VictoryPTAfter202 + ChangeEffect2002 + ptwin +
                  taxrevenues + balsheetrev + continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2008 + YearDummies2012, 
             data = Data.Analisys)

summary(LPM.pb)


# A Variável ChangeEffect2002 apresenta somente valores zero, dado que só temos dados de 2004 a 2012.
# Dado o acima, variáveis VictoryPTAfter202 e ptwin apresenta multicolineariedade perfeita, vou remover VictoryPTAfter202

LPM.pb <- lm(MunicOP_OP ~ ptwin + taxrevenues + balsheetrev + 
                  continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2008 + 
               YearDummies2012, data = Data.Analisys)

summary(LPM.pb)


# Now with population as explanatory variable

LPM.pb <- lm(MunicOP_OP ~ population + ptwin + taxrevenues + balsheetrev + 
                  continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2008 + 
               YearDummies2012, data = Data.Analisys)

summary(LPM.pb)


# Now the chance of adoption of pb

## Logicaly it is necessary exclude the cases that the municipality have pb in t-1
## (it is not possible adopt pb if it is already adopted).

Data.Analisys.adopt <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>%
  mutate(AlreadyAdopted = ifelse(Munic_Id == lag(Munic_Id) & MunicOP_OP == 1 & lag(MunicOP_OP) == 1, 1, 0)) %>% 
           filter(AlreadyAdopted == 0)

LPM.Adopt <- lm(Adopt.pb ~ ptwin + taxrevenues + balsheetrev + 
                  continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2008 + 
                  YearDummies2012, data = Data.Analisys.adopt)

summary(LPM.Adopt)


# Colocando a população como variável explicativa:

LPM.Adopt <- lm(Adopt.pb ~ population + ptwin + taxrevenues + balsheetrev + 
                  continuitypartpref + MayorsVulnerability + 
                  MayorControlCouncil + legprefpower + YearDummies2008 + 
                  YearDummies2012, data = Data.Analisys.adopt)

summary(LPM.Adopt)



# The Chance of abandon of pb (version 1)

LPM.Abandon <- lm(Abandon.pb ~ ptwin + taxrevenues + balsheetrev + 
                    continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2008 + 
                    YearDummies2012, data = Data.Analisys)

summary(LPM.Abandon)


# Colocando a população como variável explicativa:

LPM.Abandon <- lm(Abandon.pb ~ population + ptwin + taxrevenues + balsheetrev + 
                    continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2008 + 
                    YearDummies2012, data = Data.Analisys)

summary(LPM.Abandon)

# The Chance of abandon of pb (version 2)

## Logicaly it is not possible abandon pb if the municipality doens't previously adopted pb

# Delete municipalities that doens't previously adopted pb
Data.Analisys.abandon <- Data.Analisys %>% 
  arrange(Munic_Id, year) %>%
  mutate(Alreadypreviously = ifelse(Munic_Id == lag(Munic_Id) & lag(MunicOP_OP) == 1, 
                                    1, 0)) %>% 
  filter(Alreadypreviously == 1)



LPM.Abandon <- lm(Abandon.pb ~ ptwin + taxrevenues + balsheetrev + 
                    continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2008 + 
                    YearDummies2012, data = Data.Analisys.abandon)

summary(LPM.Abandon)


# Colocando a população como variável explicativa:

LPM.Abandon <- lm(Abandon.pb ~ population + ptwin + taxrevenues + balsheetrev + 
                    continuitypartpref + MayorsVulnerability + 
                    MayorControlCouncil + legprefpower + YearDummies2008 + 
                    YearDummies2012, data = Data.Analisys.abandon)


summary(LPM.Abandon) # This model have the best fit of all above


# Clear memory
rm(LPM.Abandon, LPM.Adopt)


################## Tabelas ##################




################## Sobras ##################





# End