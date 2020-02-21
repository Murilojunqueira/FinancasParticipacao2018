
# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script creates the variables used in the paper

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2018-03-15

# Dependencies:


################## Codebook ##################

# Codebook for papers variables:

## Munic_Id = municipality code
## year = year (some variables are 4 years period)
## MunicOP_OP = If the municipality have or not participatory budget in one year
## UF_Id = State initials
## Munic_Id6 = municipality code (6 digits) 
## lag.pb = lag of MunicOP_OP
## log (population) = Log of population
## MunicOP_OP.Acum = accumulated years number of continuous adoption of pb
## MayorElecNumber = Mayor electoram number
## MayorParty = Mayor party initials
## MayorName = Mayor's full name
## continuitypartpref == City government continuity (discrete)
## ContinuityMayor = Mayor reelection (discrete)
## LeftParty = victory of left parties (PT, PDT, PSB and PCdoB)
## ptwin = Victory of the PT (discrete)
## VictoryPTAfter2002 = Victory of the PT before 2002 (discrete) * -1
## ChangeEffect2002 = Change in effect after 2002 (discrete)
## MayorsVulnerability = Mayor's vulnerability
## MayorControlCouncil = Mayor controls the council (discrete)
## legprefpower = Mayor's share of council seats
## CoalitionPref = Share of seats won by mayor electoral coalition
## LeftShare = Share of the seats of leftist parties
## BudgetPP.log = Anual budget per capita.
## InvestPer = Percentual of investiments of anual budget
## taxrevenues == Tax share of revenues
## balsheetrev == Financial viability index
## InvestPer = Percentual of investiments (investiments/total Budget)
## Investpp = Investiment per capita
## YearDummies1996 = Period 3 (1996-2000)
## YearDummies2000 = Period4 (2001-2004)
## YearDummies2004 = Period5 (2005-2008)
## YearDummies2008 = Period 6 (2009-2012)



################## 1 - PB Variables ##################


## MunicOP_OP = If the municipality have or not participatory budget in one year
## lag.pb = lag of MunicOP_OP
## MunicOP_OP.Acum = accumulated years number of continuous adoption of pb

DataAnalysis <- fread("data/dataset/MunicOp.csv",
                      sep = ";", dec = ",")

Municipios <- fread("data/dataset/Municipios.csv", 
                      sep = ";", dec = ",")

DataAnalysis <- DataAnalysis %>% 
  # translate variable
  rename(year = MunicOp_Ano) %>%
  # remove rows without ID (problably sum rows)
  dplyr::filter(!is.na(Munic_Id)) %>%
  # add municipalities data
  left_join(Municipios, by = "Munic_Id") %>% 
  arrange(Munic_Id, year) %>%
  # Var lag.pb
  group_by(Munic_Id) %>% 
  mutate(lag.pb = dplyr::lag(MunicOP_OP)) %>% 
  mutate(lag.pb = ifelse(year == min(year), 0, lag.pb)) %>% 
  # MunicOP_OP.Acum
  mutate(MunicOP_OP.Acum = ifelse(lag.pb == 1, 1, 0)) %>%
  mutate(MunicOP_OP.Acum = ifelse(dplyr::lag(MunicOP_OP.Acum) == 1 & lag.pb == 1, MunicOP_OP.Acum + 1, MunicOP_OP.Acum)) %>% 
  mutate(MunicOP_OP.Acum = ifelse(dplyr::lag(MunicOP_OP.Acum) == 2 & lag.pb == 1, MunicOP_OP.Acum + 1, MunicOP_OP.Acum)) %>% 
  mutate(MunicOP_OP.Acum = ifelse(dplyr::lag(MunicOP_OP.Acum) == 3 & lag.pb == 1, MunicOP_OP.Acum + 1, MunicOP_OP.Acum)) %>% 
  mutate(MunicOP_OP.Acum = ifelse(dplyr::lag(MunicOP_OP.Acum) == 4 & lag.pb == 1, MunicOP_OP.Acum + 1, MunicOP_OP.Acum)) %>%
  mutate(MunicOP_OP.Acum = ifelse(dplyr::lag(MunicOP_OP.Acum) == 5 & lag.pb == 1, MunicOP_OP.Acum + 1, MunicOP_OP.Acum)) %>% 
  ungroup()
  
rm(Municipios)

################## 2 - Social and Economic Variables ##################

## log (population) = Log of population

SocioDemoEconomia <- fread("data/dataset/SocioDemoEconomia.csv", 
                    sep = ";", dec = ",")

# Imputate 1996 year by 1995 and 1997 mean
SocioDemoEconomia_1996 <- SocioDemoEconomia %>% 
  dplyr::filter(SocioMunic_Ano %in% c(1995, 1997)) %>% 
  group_by(Munic_Id) %>% 
  summarise(SocioMunic_Populacao = mean(SocioMunic_Populacao, na.rm = TRUE),
            SocioMunic_PIBCorrente = mean(SocioMunic_PIBCorrente, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SocioMunic_Ano = 1996) %>% 
  select(Munic_Id, SocioMunic_Ano, SocioMunic_Populacao, SocioMunic_PIBCorrente)

SocioDemoEconomia_Join <- bind_rows(SocioDemoEconomia, SocioDemoEconomia_1996) %>% 
  # translate variable
  rename(year = SocioMunic_Ano) %>%
  arrange(Munic_Id, year)


# Join data in the main dataset
DataAnalysis <- DataAnalysis %>% 
  # Avoid variable conflits
  select(-matches("SocioMunic_Populacao"), -matches("SocioMunic_PIBCorrente")) %>% 
  left_join(SocioDemoEconomia_Join, 
            by = c("Munic_Id", "year")) %>% 
  select(-SocioMunic_PIBCorrente) %>% 
  rename(population = SocioMunic_Populacao)


rm(SocioDemoEconomia, SocioDemoEconomia_1996)

################## 3 - Political Variables ##################

## ptwin = Victory of the PT (discrete)
## ChangeEffect2002 = Change in effect after 2002 (discrete)
## VictoryPTAfter2002 = Victory of the PT after 2002 (discrete)
## LeftParty = victory of left parties (PT, PDT, PSB and PCdoB, PSOL, Rede)
## ContinuityMayor = Mayor reelection (discrete)
## continuitypartpref == City government continuity (discrete)
## MayorsVulnerability = Mayor's vulnerability
## MayorControlCouncil = Mayor controls the council (discrete)
## legprefpower == Mayor's share of council seats


CandidatoAno <- fread("data/dataset/CandidatoAno.csv", 
                           sep = ";", dec = ",")

# Array of left parties numbers
LeftParties <- c(12,	# PDT
                 13,	#	PT
                 16,	#	PSTU
                 18,	#	REDE
                 21,	#	PCB
                 29,	#	PCO
                 40,	#	PSB
                 50,	#	PSOL
                 65)	#	PC do B)

MayorParties <- CandidatoAno %>% 
  # Filter mayors data
  dplyr::filter(CandAno_Cargo == "P") %>% 
  group_by(Munic_Id, CandAno_Ano) %>% 
  # Filter last runoff
  dplyr::filter(CandAno_Turno == max(CandAno_Turno)) %>% 
  ungroup() %>% 
  # In variable CandAno_SituacaoElec (electoral situation) 1 means elected candidate 
  dplyr::filter(CandAno_SituacaoElec == 1) %>%
  # Select relevant variables
  select(Munic_Id, CandAno_Ano, Partido_Sigla, CandAno_Nome, CandAno_Numero) %>% 
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  rename(MayorName = CandAno_Nome) %>%
  rename(MayorElecNumber = CandAno_Numero) %>% 
  rename(MayorParty = Partido_Sigla) %>% 
  arrange(Munic_Id, year) %>% 
  # Ajust year to normalize with Participatory budget data
  mutate(year = year + 4) %>% 
  # Order the dataset rows by municipality and year
  arrange(Munic_Id, year) %>% 
  
  # Var ptwin
  mutate(ptwin = ifelse(MayorParty == "PT", 1, 0)) %>% 
  
  # Var ChangeEffect2002
  mutate(ChangeEffect2002 = ifelse(year > 2002, 1, 0)) %>% 
  
  # Var VictoryPTAfter2002
  mutate(VictoryPTAfter2002 = ptwin * ChangeEffect2002) %>% 
  
  # Var LeftParty
  mutate(LeftParty = ifelse(MayorElecNumber %in% LeftParties, 1, 0)) %>% 
  # Var continuitypartpref
  group_by(Munic_Id) %>% 
  mutate(continuitypartpref = ifelse(MayorElecNumber == dplyr::lag(MayorElecNumber), 1, 0)) %>% 
  mutate(continuitypartpref = ifelse(is.na(continuitypartpref), 0, continuitypartpref)) %>% 
  
  # Var ContinuityMayor
  ## Remove accents
  mutate(MayorName.norm = iconv(MayorName, to = "ASCII//TRANSLIT")) %>% 
  mutate(ContinuityMayor = ifelse(MayorName.norm == dplyr::lag(MayorName.norm), 1, 0)) %>% 
  mutate(ContinuityMayor = ifelse(is.na(ContinuityMayor), 0, ContinuityMayor)) %>% 
  ungroup() %>% 
  
  # Select variables
  select(Munic_Id, year, MayorElecNumber, MayorParty, MayorName, ptwin, ChangeEffect2002,
         VictoryPTAfter2002, LeftParty, continuitypartpref, ContinuityMayor)


# Delete municipalities-year with duplicated rows (most of them had supplementary elections)
MayorParties$duplicated <- MayorParties %>% select(Munic_Id, year) %>% duplicated()

MayorParties2 <- MayorParties %>% 
  group_by(Munic_Id) %>% 
  mutate(duplicated_flag = max(duplicated)) %>% 
  mutate(duplicated_flag2 = max(duplicated_flag)) %>% 
  ungroup() %>% 
  dplyr::filter(duplicated_flag2 == 0) %>% 
  select(-duplicated, -duplicated_flag, -duplicated_flag2)

# View(MayorParties2[MayorParties2$Munic_Id == "5003702",]) # Dourados
# View(MayorParties2[MayorParties2$Munic_Id == "3516200",]) # Franca

# Join data in the main dataset

DataAnalysis <- DataAnalysis %>% 
  # Avoid variable conflits
  select(-matches("MayorElecNumber"), -matches("MayorParty"), -matches("MayorName"),
         -matches("ptwin"), -matches("ChangeEffect2002"), -matches("VictoryPTAfter2002"),
         -matches("LeftParty"), -matches("continuitypartpref"), 
         -matches("ContinuityMayor")) %>% 
  left_join(MayorParties2, by = c("Munic_Id", "year"))

# names(DataAnalysis)

rm(MayorParties)

# MayorsVulnerability = Mayor's vulnerability

MayorsVul <- CandidatoAno %>% 
  # Debug line:
  # dplyr::filter(Munic_Id == 3550308) %>% 
  # Select mayor candidates
  dplyr::filter(CandAno_Cargo == "P") %>% 
  # Avoid character/number problems
  mutate(CandAno_QtVotos = as.integer(CandAno_QtVotos)) %>% 
  mutate(CandAno_SituacaoElec = as.integer(CandAno_SituacaoElec)) %>% 
  # Filter only first round
  dplyr::filter(CandAno_Turno == 1) %>%
  # Filter null and blank vontes
  dplyr::filter(CandAno_SituacaoElec > -6) %>% 
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  rename(CandidateName = CandAno_Nome) %>%
  rename(CandidateNumber = CandAno_Numero) %>% 
  # Ajust year to normalize with Participatory budget data
  mutate(year = year + 4) %>% 
  # Find the top two candidates
  group_by(Munic_Id, year) %>%
  arrange(Munic_Id, desc(CandAno_QtVotos)) %>% 
  slice(1:2) %>% 
  # Join the elected mayors dataset. Recicle the ElectedMayors dataset (above).
  left_join(select(MayorParties2, Munic_Id, year, MayorElecNumber), 
            by = c("Munic_Id", "year")) %>% 
  mutate(ElectedVotes.temp = ifelse(CandidateNumber == MayorElecNumber, CandAno_QtVotos, NA)) %>% 
  mutate(RunnerUpVotes.temp = ifelse(CandidateNumber != MayorElecNumber, CandAno_QtVotos, NA)) %>% 
  summarise(ElectedVotes = mean(ElectedVotes.temp, na.rm = TRUE),
            RunnerUpVotes = mean(RunnerUpVotes.temp, na.rm = TRUE)) %>% 
  mutate(MayorsVulnerability = RunnerUpVotes/ElectedVotes) %>% 
  # Select relevant variables
  select(Munic_Id, year, MayorsVulnerability)

# Join data in the main dataset
DataAnalysis <- DataAnalysis %>% 
  # Avoid variable conflits
  select(-matches("MayorsVulnerability")) %>% 
  left_join(MayorsVul, by = c("Munic_Id", "year"))

# names(DataAnalysis)

rm(MayorsVul)

## legprefpower == Mayor's share of council seats
## MayorControlCouncil = Mayor controls the council (discrete)

# Find the the mayor's share of seats in city council.
MajorCouncilParty <- CandidatoAno %>% 
  # Debug line:
  # dplyr::filter(Munic_Id == 3550308) %>% 
  # Translate variables
  rename(year = CandAno_Ano) %>% 
  # Ajust year to normalize with Participatory budget data
  mutate(year = year + 4) %>% 
  # Select Council candidates.
  dplyr::filter(CandAno_Cargo == "V") %>% 
  # Select elected council members.
  ## 1 means elected by her own votes (code varies acording to election).
  ## 5 and 12 means elected by party votes (code varies acording to election).
  dplyr::filter(CandAno_SituacaoElec %in% c(1, 5, 12)) %>%  
  # Find Party number
  mutate(PartyNumber = substr(as.character(CandAno_Numero), 1, 2)) %>%
  mutate(PartyNumber = as.integer(PartyNumber)) %>% 
  # Find the nunber of seats for each party
  group_by(Munic_Id, year, PartyNumber) %>% 
  summarise(PartySeats = n()) %>%
  # Find total number of seats
  group_by(Munic_Id, year) %>% 
  mutate(CityTotalSeats = sum(PartySeats)) %>% 
  # Party share of seats.
  mutate(PartyShareSeats = PartySeats/CityTotalSeats) %>% 
  # Find the share of mayors party. Recicle elected mayors dataset.
  left_join(select(MayorParties2, Munic_Id, year, MayorElecNumber), 
            by = c("Munic_Id", "year")) %>% 
  mutate(legprefpower.temp = ifelse(PartyNumber == MayorElecNumber, PartyShareSeats, NA)) %>% 
  # Create the variables legprefpower and MayorControlCouncil
  summarise(legprefpower = mean(legprefpower.temp, na.rm = TRUE)) %>% 
  ## This line is for the case that mayors party doesn't have any seat in the council
  mutate(legprefpower = ifelse(is.nan(legprefpower), 0, legprefpower)) %>% 
  mutate(MayorControlCouncil = ifelse(legprefpower >= .5, 1, 0)) %>% 
  select(Munic_Id, year, legprefpower, MayorControlCouncil) %>% 
  ungroup()

# Join data in the main dataset
DataAnalysis <- DataAnalysis %>% 
  # Avoid variable conflits
  select(-matches("legprefpower"), -matches("MayorControlCouncil")) %>% 
  left_join(MajorCouncilParty, by = c("Munic_Id", "year"))

# names(DataAnalysis)

rm(MajorCouncilParty)


# Coalizão do prefeito domina a Câmara


PartiesShare <- CandidatoAno %>%
  # Debug line, Franca:
  # dplyr::filter(Munic_Id == 3516200) %>%
  # Translate variables
  rename(year = CandAno_Ano) %>%
  # Ajust year to normalize with Participatory budget data
  mutate(year = year + 4) %>%
  # Select Council candidates.
  dplyr::filter(CandAno_Cargo == "V") %>%
  # Select elected council members.
  ## 1 means elected by her own votes.
  ## 5 and 12 means elected by party votes.
  dplyr::filter(CandAno_SituacaoElec %in% c(1, 5, 12)) %>%
  # Find Party number
  mutate(Partido_Numero = substr(as.character(CandAno_Numero), 1, 2)) %>%
  mutate(Partido_Numero = as.integer(Partido_Numero)) %>%
  # Find the nunber of seats for each party
  group_by(Munic_Id, year, Partido_Numero) %>%
  summarise(PartySeats = n()) %>%
  # Find total number of seats
  group_by(Munic_Id, year) %>%
  mutate(CityTotalSeats = sum(PartySeats)) %>%
  # Party share of seats.
  mutate(PartyShareSeats = PartySeats/CityTotalSeats) %>%
  ungroup()

Coligacoes <- fread("data/dataset/Coligacoes.csv",
                    sep = ";", dec = ",")

ColigacoesPartidos <- fread("data/dataset/ColigacoesPartidos.csv",
                            sep = ";", dec = ",")

Partidos <- fread("data/dataset/Partidos.csv",
                  sep = ";", dec = ",")

# Create a table that integrate party initials and number
ExpandParty <- tibble()

for (i in seq_len(nrow(Partidos))) {
  
  # i <- 1
  message("Formating ", Partidos$Partido_Sigla[i])
  
  newRow <- seq(from = Partidos$Partido_AnoInicial[i], 
                to = Partidos$Partido_AnoFinal[i], 
                by = 4) %>% 
    list() %>%
    magrittr::set_names("year") %>% 
    as.tibble() %>%
    mutate(Partido_Numero = Partidos$Partido_Numero[i]) %>% 
    mutate(Partido_Sigla = Partidos$Partido_Sigla[i]) 
  
  ExpandParty <- bind_rows(ExpandParty, newRow)
}
rm(i, newRow)


CoalitionPref <- ColigacoesPartidos %>%
  left_join(ExpandParty, by = c("Partido_Sigla" = "Partido_Sigla", 
                                "Coligacao_Ano" = "year")) %>%
  left_join(Coligacoes, by = c("Coligacao_Id", "Coligacao_Ano", "Coligacao_Cargo")) %>%
  rename(year = Coligacao_Ano) %>%
  # Debug line:
  # dplyr::filter(Munic_Id == 3516200) %>%
  # Filter position and runoff
  dplyr::filter(Coligacao_Cargo == "P") %>%
  dplyr::filter(Coligacao_Turno == 1) %>%
  # Ajust year to normalize with Participatory budget data
  mutate(year = year + 4) %>%
  select(Coligacao_Id, Munic_Id, year, Partido_Sigla, Partido_Numero) %>%
  # Find the share of mayors party. Recicle elected mayors dataset.
  left_join(select(MayorParties2, Munic_Id, year, MayorElecNumber),
            by = c("Munic_Id", "year")) %>%
  mutate(CoalitionPref_flag = ifelse(Partido_Numero == MayorElecNumber, 1, 0)) %>%
  group_by(Coligacao_Id) %>%
  mutate(CoalitionPref = max(CoalitionPref_flag)) %>%
  ungroup() %>%
  dplyr::filter(CoalitionPref == 1) %>% 
  left_join(PartiesShare, by = c("Munic_Id", "year", "Partido_Numero"))%>%
  group_by(Munic_Id, year) %>%
  summarise(CoalitionPref = sum(PartyShareSeats, na.rm = TRUE)) %>%
  ungroup()

# Share of left parties
ShareLeft <- PartiesShare %>%
  mutate(Partido_Numero = as.integer(Partido_Numero)) %>%
  dplyr::filter(Partido_Numero %in% LeftParties) %>%
  group_by(Munic_Id, year) %>%
  summarise(LeftShare = sum(PartyShareSeats, na.rm = TRUE))



# Join data in the main dataset
DataAnalysis <- DataAnalysis %>%
  select(-matches("CoalitionPref"), -matches("LeftShare")) %>% 
  left_join(CoalitionPref, by = c("Munic_Id", "year")) %>%
  left_join(ShareLeft, by = c("Munic_Id", "year"))


# names(DataAnalysis)

# Free memory
rm(CandidatoAno, MayorParties2, CoalitionPref, Coligacoes, ColigacoesPartidos)
rm(Partidos, ExpandParty, PartiesShare, LeftParties, ShareLeft)


################## 4 - Finantial Variables ##################

MunicFinancas <- fread("data/dataset/MunicFinancas.csv", 
                       sep = ";", dec = ",")

# Group years

# Function to group years in municipalities terms
# In Brazil, all mayors and council members terms starts and end at same time.
GroupYears <- function(x) {
  
  periods <- list(t1996 = c(1993:1996),
                  t2000 = c(1997:2000),
                  t2004 = c(2001:2004),
                  t2008 = c(2005:2008),
                  t2012 = c(2009:2012),
                  t2016 = c(2013:2016))
  
  Output <- character(length(x))
  
  for(i in seq_len(length(periods))) {
    Output[which(x %in% periods[[i]])] <- names(periods)[i]
  }
  
  return(Output)
}

# Create terms group 
MunicFinancas$GroupTerms <- GroupYears(MunicFinancas$MunicFinancas_Ano)

rm(GroupYears)


# Insert inflation indicator
Inflacao <- fread("data/dataset/Inflacao.csv", 
                       sep = ";", dec = ",")

# Select year range and inflation index
Inflacao <- Inflacao %>%
  dplyr::filter(IndiceNome == "InflacaoIPCAaa") %>%
  dplyr::filter(ano >= 1995) %>%
  arrange(ano)

# Loop to cread inflation index with 1995 base = 100
Inflacao$indicador100[Inflacao$ano == 1995] <- 100

for (i in 2:nrow(Inflacao)) {
  Inflacao$indicador100[i] <- (1 + Inflacao$inflacaoValor[i]/100) * Inflacao$indicador100[i-1]
  # message(Inflacao$ano[i])
}
rm(i)


# Create index to bring all finantial data to 2015 value.
Inflacao <- Inflacao %>%
  mutate(indicador2015 = Inflacao$indicador100[ano == 2015]/indicador100) %>%
  # Translate variables
  mutate(year = ano) %>% 
  select(year, indicador2015)


## BudgetPP = Anual budget per capita.
## InvestPer = Percentual of investiments (investiments/total Budget)
## Investpp = Investiment per capita


# Public investment
Investment <- MunicFinancas %>% 
  # Debug
  # dplyr::filter(Munic_Id == 3516200) %>% 
  # Translade variable
  rename(year = MunicFinancas_Ano) %>% 
  # Filter missing municipality codes.
  dplyr::filter(!is.na(Munic_Id))%>% 
  # Prevent character/numeric intepretation problems
  mutate(ContasPublica_Id = as.numeric(ContasPublica_Id)) %>%
  mutate(MunicFinancas_ContaValor = sub(",", ".", MunicFinancas_ContaValor)) %>% 
  mutate(MunicFinancas_ContaValor = as.numeric(MunicFinancas_ContaValor)) %>% 
  # Select the used accounts (Current Spending, Capital Spending, Current revenue, current revenue deductions, Capital Revenue)
  dplyr::filter(ContasPublica_Id == 73400000000 | ContasPublica_Id == 44000000) %>% 
  # Spread account variables
  spread(ContasPublica_Id, MunicFinancas_ContaValor) %>% 
  # Set friendly variable's names.
  rename(TotalSpending = "7.34e+10",
         InvestimentTotal = "4.4e+07") %>% 
  left_join(Inflacao, by = "year") %>% 
  left_join(SocioDemoEconomia_Join, by = c("Munic_Id", "year")) %>% 
  rename(population = SocioMunic_Populacao) %>% 
  # Create main variables
  ## Budget per capita
  mutate(Budget = TotalSpending * indicador2015 ) %>% 
  mutate(BudgetPP = Budget / population) %>% 
  ## Investiment per capita
  mutate(Investiment = InvestimentTotal * indicador2015) %>% 
  mutate(Investpp = Investiment / population) %>% 
  ## Investiment as share of total spending
  mutate(InvestPer = InvestimentTotal / TotalSpending) %>% 
  # Group Variables by municipal term's period
  group_by(Munic_Id, GroupTerms) %>% 
  summarise(BudgetPP = mean(BudgetPP, na.rm = TRUE),
            Investpp = mean(Investpp, na.rm = TRUE),
            InvestPer = mean(InvestPer, na.rm = TRUE)) %>% 
    ungroup() %>% 
  # Return GroupTerms to years
  mutate(year = as.integer(sub("t", "", GroupTerms))) %>% 
  select(Munic_Id, year, BudgetPP, Investpp, InvestPer)


# Join data in the main dataset
DataAnalysis <- DataAnalysis %>% 
  # Avoid duplicated variables
  select(-starts_with("BudgetPP"),
         -starts_with("Investpp"), 
         -starts_with("InvestPer"), 
         -starts_with("indicador2015")) %>% 
  # add new variable
  left_join(Investment, by = c("Munic_Id", "year"))

rm(Investment)

# taxrevenues == Tax share of revenues
  ## taxrevenues == Tax revenue / (current revenue - current revenue deductions)
  ## taxrevenues == Receita Tributária / (Receitas Correntes - dedu??es de receitas correntes)

TaxShareRevenues <- MunicFinancas %>% 
  # filter(Munic_Id == "3550308") %>% 
  # Translade variable
  rename(year = MunicFinancas_Ano) %>% 
  # Filter missing municipality codes.
  dplyr::filter(!is.na(Munic_Id)) %>% 
  # Prevent character/numeric intepretation problems
  mutate(ContasPublica_Id = as.numeric(ContasPublica_Id)) %>%
  mutate(MunicFinancas_ContaValor = sub(",", ".", MunicFinancas_ContaValor)) %>% 
  mutate(MunicFinancas_ContaValor = as.numeric(MunicFinancas_ContaValor)) %>% 
  # Select the used accounts (Tax revenue, current revenue, current revenue deductions)
  dplyr::filter(ContasPublica_Id %in% c(71200000000, 11000000, 17000000)) %>% 
  # Spread account variables
  spread(ContasPublica_Id, MunicFinancas_ContaValor) %>% 
  # Set friendly variable's names.
  rename(TotalRevenue = "7.12e+10", 
         TaxRevenues = "1.1e+07",
         CurrentTransfers = "1.7e+07") %>% 
  # remove NA from deductions before 2002
  # mutate(RevenueDeductions = ifelse(is.na(RevenueDeductions), 0, RevenueDeductions)) %>% 
  # Create taxrevenues variables
  mutate(taxrevenues = TaxRevenues / TotalRevenue) %>%
  # Create taxrevenues variables
  mutate(TransferDep = CurrentTransfers / TotalRevenue) %>%
  # Group Variables by municipal term's period  
  group_by(Munic_Id, GroupTerms) %>% 
  summarise(taxrevenues = mean(taxrevenues, na.rm = TRUE),
            TransferDep = mean(TransferDep, na.rm = TRUE)) %>% 
  # Return GroupTerms to years
  mutate(year = as.integer(sub("t", "", GroupTerms))) %>% 
  # Select relevant variables
  select(Munic_Id, year, taxrevenues, TransferDep)


# Join data in the main dataset
DataAnalysis <- DataAnalysis %>% 
  # Avoid duplicated variables
  select(-starts_with("taxrevenues") , 
         -starts_with("TransferDep") ) %>% 
  # add new variable
  left_join(TaxShareRevenues, by = c("Munic_Id", "year")) 

rm(TaxShareRevenues)


## balsheetrev == Financial viability index
  ### balsheetrev == (Current Spending + Capital Spending) / (Current revenue - current revenue deductions + Capital Revenue)
  ### balsheetrev == (Receita Tributária + Receita de Capital) / (Receitas Correntes - deduções de receitas correntes + Receita de Capital)

BalanceBudget <- MunicFinancas %>% 
  # Translade variable
  rename(year = MunicFinancas_Ano) %>% 
  # Filter missing municipality codes.
  dplyr::filter(!is.na(Munic_Id)) %>% 
  # Prevent character/numeric intepretation problems
  mutate(ContasPublica_Id = as.numeric(ContasPublica_Id)) %>%
  mutate(MunicFinancas_ContaValor = sub(",", ".", MunicFinancas_ContaValor)) %>% 
  mutate(MunicFinancas_ContaValor = as.numeric(MunicFinancas_ContaValor)) %>% 
  # Select the used accounts (Current Spending, Capital Spending, Current revenue, current revenue deductions, Capital Revenue)
  dplyr::filter(ContasPublica_Id %in% c(71200000000, 73400000000)) %>% 
  # Spread account variables
  spread(ContasPublica_Id, MunicFinancas_ContaValor) %>% 
  # Set friendly variable's names.
  rename(TotalRevenue = "7.12e+10", 
         TotalSpending = "7.34e+10") %>% 
  # Create balsheetrev variable
  mutate(balsheetrev = TotalSpending / TotalRevenue) %>% 
  # Group Variables by municipal term's period
  group_by(Munic_Id, GroupTerms) %>% 
  summarise(balsheetrev = mean(balsheetrev, na.rm = TRUE)) %>% 
  # Return GroupTerms to years
  mutate(year = as.integer(sub("t", "", GroupTerms))) %>% 
  # Select relevant variables
  select(Munic_Id, year, balsheetrev)

# Join data in the main dataset
DataAnalysis <- DataAnalysis %>%
  # Avoid duplicated variables
  select(-starts_with("balsheetrev") ) %>% 
  # add new variable
  left_join(BalanceBudget, by = c("Munic_Id", "year")) 

rm(BalanceBudget)
rm(MunicFinancas, SocioDemoEconomia_Join, Inflacao)


################## 5 - Time Variables ##################


# YearDummies*

YearDummies <-  factor(DataAnalysis$year)

YearDummies <- model.matrix(~YearDummies) %>% 
  as.data.frame() %>% 
  select(-matches("(Intercept)")) %>% 
  bind_cols(DataAnalysis) %>% 
  select(Munic_Id, year, starts_with("YearDummies"))

# Check data
names(YearDummies)

# Join data in the main dataset
DataAnalysis <- DataAnalysis %>%
  # Avoid duplicated variables
  select(-starts_with("YearDummies")) %>% 
  left_join(YearDummies, by = c("Munic_Id", "year")) 

# Check data
names(DataAnalysis)

rm(YearDummies)


## YearDummies1996 = Period 3 (1996-2000)
## YearDummies2000 = Period 4 (2001-2004)
## YearDummies2004 = Period 5 (2005-2008)
## YearDummies2008 = Period 6 (2009-2012)


# 3516200 - Franca
# 3550308 - São Paulo
# 2211001 - Teresina
# 1501402 - Belém

# View(DataAnalysis[DataAnalysis$Munic_Nome == "FRANCA",])
# View(DataAnalysis[DataAnalysis$Munic_Nome == "SÃO PAULO",])
# View(DataAnalysis[DataAnalysis$Munic_Nome == "TERESINA",])
# View(DataAnalysis[DataAnalysis$Munic_Nome == "BELÉM",])


# Write File
fwrite(DataAnalysis, 
       file = "data/analysisdata/DataAnalysis.csv", 
       sep = ";", dec = ",")

rm(DataAnalysis)


# End