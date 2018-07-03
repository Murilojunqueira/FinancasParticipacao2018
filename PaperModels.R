
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
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


# InputFolder <- "C:/Users/Murilo Junqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
# OutputFolder <- "C:/Users/Murilo Junqueira/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
# ScriptFolder <- "C:/Users/Murilo Junqueira/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


# Check folders
dir.exists(c(InputFolder, OutputFolder, ScriptFolder))


# instala o pacote de extração dos dados, se necessário
library(data.table)
#library(zeligverse)
library(scales)
library(tidyverse)


# Script with functions
source(paste0(ScriptFolder, "PaperFunctions.R"))


################## Load Data ##################


Data.Analysis <- fread(paste0(InputFolder, "Data.Analysis.csv"), 
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
Data.Analysis <- Data.Analysis %>% 
  distinct(Munic_Id, year, .keep_all = TRUE)


names(Data.Analysis)

# Filter only municipalities with more than 50k pop in 1996
Data.Analysis <- Data.Analysis %>% 
  # Filtering in 1996 because we don't have data for 1992
  mutate(Sample.flag = ifelse(year == 1996 & population > 50000, 1, 0)) %>% 
  group_by(Munic_Id) %>% 
  mutate(Sample.Selection = max(Sample.flag, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Sample.Selection == 1) %>% 
  select(-Sample.flag, -Sample.Selection)


# Check complete cases
Data.Analysis.Complete <- Data.Analysis %>% 
  filter(year > 1992) %>% 
  select(-mindist, -GDP, -GDPpp) %>%
  na.omit()

# Check complete cases
map_int(Data.Analysis.Complete, function(x) sum(is.na(x), na.rm = TRUE))


# Prevent type data problems
Data.Analysis.Complete <- Data.Analysis.Complete %>% 
  mutate(taxrevenues = as.numeric(sub(",", ".", taxrevenues))) %>% 
  mutate(balsheetrev = as.numeric(sub(",", ".", balsheetrev))) %>% 
  mutate(MayorsVulnerability = as.numeric(sub(",", ".", MayorsVulnerability))) %>% 
  mutate(legprefpower = as.numeric(sub(",", ".", legprefpower))) %>% 
  mutate(Investpp = as.numeric(sub(",", ".", Investpp))) %>% 
  mutate(InvestPer = as.numeric(sub(",", ".", InvestPer))) %>% 
  mutate(BudgetPP = as.numeric(sub(",", ".", BudgetPP))) %>% 
  mutate(FiscalSpacePer = as.numeric(sub(",", ".", FiscalSpacePer))) %>% 
  mutate(DebtPer = as.numeric(sub(",", ".", DebtPer))) 


map_chr(Data.Analysis.Complete, class)

# Cases by year  
table(Data.Analysis.Complete$year)



DebtData <- Data.Analysis.Complete %>% 
  select(year, InvestPer,DebtPer, FiscalSpacePer) %>% 
  group_by(year) %>% 
  summarise(InvestPer = mean(InvestPer, na.rm = TRUE),
            DebtPer = mean(DebtPer, na.rm = TRUE),
            FiscalSpacePer = mean(FiscalSpacePer, na.rm = TRUE))

################## Alternative Samples ##################

# Only PT mayors
Data.Analysis.Complete.PT <- Data.Analysis.Complete %>% 
  filter(ptwin == 1)


# Only Left mayors
Data.Analysis.Complete.Left <- Data.Analysis.Complete %>% 
  filter(LeftParty == 1)

# Adoption of PB (it's not possible adopt pb if it is already adopted)
Data.Analysis.Adoption <- Data.Analysis.Complete %>% 
  filter(lag.pb == 0)


# Adoption of PB only Left mayors
Data.Analysis.Adoption.PT <- Data.Analysis.Complete %>% 
  filter(lag.pb == 0) %>% 
  filter(ptwin == 1)


# Delete municipalities that doens't previously adopted pb
Data.Analysis.abandon <- Data.Analysis.Complete %>% 
  filter(lag.pb == 1)


# Delete municipalities that doens't previously adopted pb
# Only PT mayors
Data.Analysis.abandon.PT <- Data.Analysis.Complete %>% 
  filter(lag.pb == 1) %>% 
  filter(ptwin == 1)

# Delete municipalities that doens't previously adopted pb
# Only Left mayors
Data.Analysis.abandon.Left <- Data.Analysis.Complete %>% 
  filter(lag.pb == 1) %>% 
  filter(LeftParty == 1)


################## Existence of pb ##################

# Linear predicted model
LPM.pb <- lm(MunicOP_OP ~ 
               # Lag dependent variable (LDV)
               lag.pb + MunicOP_OP.Acum + 
               # population
               log(population) + Capital + log(population):LeftParty  + log(population):ptwin +
               # PT variables
               ptwin + VictoryPTAfter2002 + LeftParty + ptwin:ContinuityMayor + lag.pb:LeftParty +
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
               ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
               # Finantial variables
               log(BudgetPP) + taxrevenues + balsheetrev + InvestPer + log(Investpp) + DebtPer + 
               lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer + taxrevenues:ptwin + 
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012 + YearDummies2004:LeftParty
             ,  
             # Dataset
             data = Data.Analysis.Complete)


# Check model results
checkModel(LPM.pb)

# Compare to stepwise
s <- step(LPM.pb, trace = FALSE, direction = "both")
checkModel(s)


# Anova test
anova(LPM.pb, test="Chisq")


# Logit model
Logit.pb <- glm(MunicOP_OP ~ 
                  # Lag dependent variable (LDV)
                  lag.pb + MunicOP_OP.Acum + 
                  # population
                  log(population) + Capital + log(population):LeftParty  + log(population):ptwin +
                  # PT variables
                  ptwin + VictoryPTAfter2002 + LeftParty + ptwin:ContinuityMayor + lag.pb:LeftParty +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
                  # Finantial variables
                  log(BudgetPP) + taxrevenues + balsheetrev + InvestPer + log(Investpp) + DebtPer + 
                  lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer + taxrevenues:ptwin + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012 + YearDummies2004:LeftParty
                ,
              # Dataset
              data = Data.Analysis.Complete,
              # Model
              family=binomial(link='logit'))


checkModel(Logit.pb)

s <- step(Logit.pb, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)

# Anova test
anova(Logit.pb, test="Chisq")


hist(log(Data.Analysis.Complete$population))

LPM.pb.Min <- lm(MunicOP_OP ~ 
                   # Lag dependent variable (LDV)
                   lag.pb + MunicOP_OP.Acum + 
                   # population
                   log(population) + log(population):LeftParty  + log(population):ptwin +
                   # PT variables
                   ptwin + VictoryPTAfter2002 + LeftParty + # ptwin:ContinuityMayor +
                   VictoryPTAfter2002:log(population) + 
                   # Political Variables
                   continuitypartpref + MayorsVulnerability + legprefpower + 
                   ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
                   # Finantial variables
                   log(BudgetPP) + InvestPer + lag.pb:InvestPer + log(BudgetPP):InvestPer + 
                   # Time Variables
                   YearDummies2004 + YearDummies2008 + YearDummies2012
             ,  
             # Dataset
             data = Data.Analysis.Complete)


# Check model results
checkModel(LPM.pb.Min)

# Anova test
anova(LPM.pb.Min, test="Chisq")

# Compare to stepwise
s <- step(LPM.pb.Min, trace = FALSE, direction = "both")
checkModel(s)


# Logit model
Logit.pb.Min <- glm(MunicOP_OP ~ 
                      # Lag dependent variable (LDV)
                      lag.pb + MunicOP_OP.Acum + 
                      # population
                      log(population) + log(population):LeftParty  + log(population):ptwin +
                      # PT variables
                      ptwin + VictoryPTAfter2002 + LeftParty + ptwin:ContinuityMayor +
                      # Political Variables
                      continuitypartpref + MayorsVulnerability + legprefpower + 
                      ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
                      # Finantial variables
                      log(BudgetPP) + InvestPer + lag.pb:InvestPer + log(BudgetPP):InvestPer + 
                      # Time Variables
                      YearDummies2004 + YearDummies2008 + YearDummies2012
                    ,
                    # Dataset
                    data = Data.Analysis.Complete,
                    # Model
                    family=binomial(link='logit')
                    )



checkModel(Logit.pb.Min)


# Anova test
anova(Logit.pb.Min, test="Chisq")

# Compare to stepwise
s <- step(Logit.pb.Min, trace = FALSE, direction = "both")
checkModel(s)



# Only PT mayors

# Linear predicted model
LPM.pb.PT <- lm(MunicOP_OP ~ 
               # Lag dependent variable (LDV)
               lag.pb + MunicOP_OP.Acum +
               # population
               log(population) + 
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                 ContinuityMayor + ContinuityMayor:lag.pb + 
               # Finantial variables
               log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer +
               lag.pb:InvestPer + log(BudgetPP):InvestPer + 
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012,  
             # Dataset
             data = Data.Analysis.Complete.PT)


# Check model results
checkModel(LPM.pb.PT)

# Compare to stepwise
s <- step(LPM.pb.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)



# Linear predicted model
LPM.pb.PT.min <- lm(MunicOP_OP ~ 
                  # Lag dependent variable (LDV)
                  lag.pb + MunicOP_OP.Acum +
                  # population
                  log(population) + 
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + legprefpower + 
                  ContinuityMayor + ContinuityMayor:lag.pb + continuitypartpref:lag.pb + 
                  # Finantial variables
                  log(BudgetPP) + taxrevenues + + InvestPer + 
                  lag.pb:InvestPer + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
                data = Data.Analysis.Complete.PT)


# Check model results
checkModel(LPM.pb.PT.min)

# Anova test
anova(LPM.pb.PT.min, test="Chisq")

# Compare to stepwise
s <- step(LPM.pb.PT.min, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)


# Logit model
Logit.pb.PT <- glm(MunicOP_OP ~ 
                  # Lag dependent variable (LDV)
                  lag.pb + MunicOP_OP.Acum +
                  # population
                  log(population) + Capital +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  ContinuityMayor + ContinuityMayor * lag.pb +
                  # Finantial variables
                  log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + 
                    lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer + log(BudgetPP):InvestPer + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
                data = Data.Analysis.Complete.PT,
                # Model
                family=binomial(link='logit'))


checkModel(Logit.pb.PT)

# Anova test
anova(Logit.pb.Min, test="Chisq")

s <- step(LPM.pb.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)



# Only Left mayors

# Linear predicted model
LPM.pb.Left <- lm(MunicOP_OP ~ 
                    # Lag dependent variable (LDV)
                    lag.pb + MunicOP_OP.Acum +
                    # population
                    log(population) + Capital +
                    # PT variables
                    ptwin + VictoryPTAfter2002 + 
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    ContinuityMayor + ContinuityMayor * lag.pb +
                    # Finantial variables
                    log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                    lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer +
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
                data = Data.Analysis.Complete.Left)


# Check model results
checkModel(LPM.pb.Left)

# Compare to stepwise
s <- step(LPM.pb.Left, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)


# Logit model
Logit.pb.Left <- glm(MunicOP_OP ~ 
                       # Lag dependent variable (LDV)
                       lag.pb + MunicOP_OP.Acum +
                       # population
                       log(population) + Capital +
                       # PT variables
                       ptwin + VictoryPTAfter2002 + 
                       # Political Variables
                       continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                       ContinuityMayor + ContinuityMayor * lag.pb +
                       # Finantial variables
                       log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + 
                       lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer +
                       # Time Variables
                       YearDummies2004 + YearDummies2008 + YearDummies2012,  
                   # Dataset
                   data = Data.Analysis.Complete.Left,
                   # Model
                   family=binomial(link='logit'))


checkModel(Logit.pb.Left)

# Compare to stepwise
s <- step(Logit.pb.Left, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)


################## Existence Post-Estimation ##################

# Anova test
anova(Logit.pb.PT, test="Chisq")


# Check false positive cases
Data.Analysis.Complete$predict <- predict(LPM.pb, type = 'response')

Data.Analysis.Complete <- Data.Analysis.Complete %>% 
  mutate(predict.dummy = ifelse(predict > 0.5, 1, 0)) %>% 
  mutate(False.Positive = ifelse(MunicOP_OP == 0 & predict.dummy == 1, 1, 0)) %>% 
  mutate(False.Negative = ifelse(MunicOP_OP == 1 & predict.dummy == 0, 1, 0))

View(Data.Analysis.Complete[Data.Analysis.Complete$False.Negative == 1,])
View(Data.Analysis.Complete[Data.Analysis.Complete$False.Positive == 1,])


################## Adoption of pb ##################




LPM.Adopt <- lm(Adopt.pb ~ 
                  # population
                  log(population) + Capital +
                  # PT variables
                  ptwin + VictoryPTAfter2002 + LeftParty +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  ContinuityMayor + 
                  # Finantial variables
                  log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + 
                  FiscalSpacePer + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012,  
             # Dataset
             data = Data.Analysis.Adoption)


checkModel(LPM.Adopt)

s <- step(LPM.Adopt, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)



# Logit model
Logit.Adopt <- glm(MunicOP_OP ~ 
                     # population
                     log(population) + Capital +
                     # PT variables
                     ptwin + VictoryPTAfter2002 + LeftParty +
                     # Political Variables
                     continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                     ContinuityMayor + 
                     # Finantial variables
                     log(BudgetPP) + taxrevenues + balsheetrev + InvestPer + log(Investpp) + DebtPer + 
                     FiscalSpacePer + 
                     # Time Variables
                     YearDummies2004 + YearDummies2008 + YearDummies2012,  
                     # Dataset
                     data = Data.Analysis.Adoption,
                     # Model
                     family=binomial(link='logit'))

checkModel(Logit.Adopt)

s <- step(Logit.Adopt, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)


# Only for PT mayors
LPM.Adopt.PT <- lm(Adopt.pb ~ 
                     # population
                     log(population) + Capital +
                     # Political Variables
                     continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                     ContinuityMayor + 
                     # Finantial variables
                     log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                     # Time Variables
                     YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
                data = Data.Analysis.Adoption.PT)


checkModel(LPM.Adopt.PT)

s <- step(LPM.Adopt.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)



# Logit model
Logit.Adopt.PT <- glm(MunicOP_OP ~ 
                        # population
                        log(population) + Capital +
                        # Political Variables
                        continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                        ContinuityMayor + 
                        # Finantial variables
                        log(BudgetPP) + taxrevenues + balsheetrev + + InvestPer + log(Investpp) + DebtPer + 
                        FiscalSpacePer +
                        # Time Variables
                        YearDummies2004 + YearDummies2008 + YearDummies2012,  
                   # Dataset
                   data = Data.Analysis.Adoption.PT,
                   # Model
                   family=binomial(link='logit'))

checkModel(Logit.Adopt.PT)

s <- step(Logit.Adopt.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(population) + log(BudgetPP) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)




################## Adoption Post-Estimation ##################

LPM.Adopt <- s

# Anova test
anova(LPM.Adopt, test="Chisq")

# Check Data
Data.Analysis.Adoption.PT$predict <- predict(LPM.Adopt, type = 'response')

Data.Analysis.Adoption.PT <- Data.Analysis.Adoption.PT %>% 
  mutate(predict.dummy = ifelse(predict > 0.5, 1, 0)) %>% 
  mutate(False.Positive = ifelse(Adopt.pb == 0 & predict.dummy == 1, 1, 0)) %>% 
  mutate(False.Negative = ifelse(Adopt.pb == 1 & predict.dummy == 0, 1, 0))

View(Data.Analysis.Adoption.PT[Data.Analysis.Adoption.PT$False.Negative == 1,])
View(Data.Analysis.Adoption.PT[Data.Analysis.Adoption.PT$False.Positive == 1,])



################## Abandon of pb ##################


LPM.Abandon <- lm(Abandon.pb ~ 
                         # Lag dependent variable (LDV)
                         MunicOP_OP.Acum +
                         # population
                         log(population) + Capital +
                         # PT variables
                         ptwin + VictoryPTAfter2002 + LeftParty +
                         # Political Variables
                         continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                         ContinuityMayor + 
                         # Finantial variables
                         taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + 
                         lag.pb:InvestPer + FiscalSpacePer + lag.pb:FiscalSpacePer +
                         # Time Variables
                         YearDummies2004 + YearDummies2008 + YearDummies2012,  
                       # Dataset
                       data = Data.Analysis.Complete)
  
  
  
checkModel(LPM.Abandon)

s <- step(LPM.Abandon, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)


# P(pb = 1 | pb(t-1) = 1) # A chance de continuidade do OP
LPM.Abandon.pb <- lm((MunicOP_OP) ~ 
                    # Lag dependent variable (LDV)
                    MunicOP_OP.Acum + 
                    # population
                    log(population) + Capital +
                    # PT variables
                    ptwin + VictoryPTAfter2002 +  LeftParty +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    ContinuityMayor + 
                    # Finantial variables
                    taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.abandon)

checkModel(LPM.Abandon.pb)

s <- step(LPM.Abandon.pb, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)



Logit.Abandon.pb <- glm(MunicOP_OP ~ 
                       # Lag dependent variable (LDV)
                       MunicOP_OP.Acum + 
                       # population
                       log(population) + Capital +
                       # PT variables
                       ptwin + VictoryPTAfter2002 +  LeftParty +
                       # Political Variables
                       continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                       ContinuityMayor + 
                       # Finantial variables
                       taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                       # Time Variables
                       YearDummies2004 + YearDummies2008 + YearDummies2012,  
                     # Dataset
                     data = Data.Analysis.abandon,
                     # Model
                     family=binomial(link='logit'))

checkModel(Logit.Abandon.pb)

s <- step(Logit.Abandon.pb, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) +  InvestPer +
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)


# Only PT mayors

table(Data.Analysis.abandon.PT$MunicOP_OP)


LPM.Abandon.pb.PT <- lm(MunicOP_OP ~ 
                       # Lag dependent variable (LDV)
                       MunicOP_OP.Acum + 
                       # population
                       log(population) + Capital +
                       # Political Variables
                       continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                       ContinuityMayor + 
                       # Finantial variables
                       taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                       # Time Variables
                       YearDummies2004 + YearDummies2008 + YearDummies2012,  
                     # Dataset
                     data = Data.Analysis.abandon.PT)

checkModel(LPM.Abandon.pb.PT)

s <- step(LPM.Abandon.pb.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)

s <- step(LPM.Abandon.pb.PT, trace = FALSE, direction = "both")

checkModel(s)


Logit.Abandon.pb.PT <- glm(MunicOP_OP ~ 
                          # Lag dependent variable (LDV)
                          MunicOP_OP.Acum + 
                          # population
                          log(population) + Capital +
                          # Political Variables
                          continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                          ContinuityMayor + 
                          # Finantial variables
                          taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                          # Time Variables
                          YearDummies2004 + YearDummies2008 + YearDummies2012,  
                        # Dataset
                        data = Data.Analysis.abandon.PT,
                        # Model
                        family=binomial(link='logit'))

checkModel(Logit.Abandon.pb.PT)

s <- step(Logit.Abandon.pb.PT, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)


# Only left mayors
LPM.Abandon.pb.Left <- lm(MunicOP_OP ~ 
                            # Lag dependent variable (LDV)
                            MunicOP_OP.Acum + 
                            # population
                            log(population) + Capital +
                            # PT variables
                            ptwin + VictoryPTAfter2002 +
                            # Political Variables
                            continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                            ContinuityMayor + 
                            # Finantial variables
                            taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                            # Time Variables
                            YearDummies2004 + YearDummies2008 + YearDummies2012,  
                        # Dataset
                        data = Data.Analysis.abandon.Left)

checkModel(LPM.Abandon.pb.Left)

s <- step(LPM.Abandon.pb.Left, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))

checkModel(s)



Logit.Abandon.pb.Left <- glm(MunicOP_OP ~ 
                               # Lag dependent variable (LDV)
                               MunicOP_OP.Acum + 
                               # population
                               log(population) + Capital +
                               # PT variables
                               ptwin + VictoryPTAfter2002 + 
                               # Political Variables
                               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                               ContinuityMayor + 
                               # Finantial variables
                               taxrevenues + balsheetrev + log(BudgetPP) + InvestPer + log(Investpp) + DebtPer + FiscalSpacePer +
                               # Time Variables
                               YearDummies2004 + YearDummies2008 + YearDummies2012,  
                           # Dataset
                           data = Data.Analysis.abandon.Left,
                           # Model
                           family=binomial(link='logit'))

checkModel(Logit.Abandon.pb.Left)

s <- step(Logit.Abandon.pb.Left, trace = FALSE, direction = "both", 
          scope = list(lower=as.formula(Adopt.pb ~ log(BudgetPP) + log(population) + 
                                          YearDummies2004 + YearDummies2008 + YearDummies2012)))
checkModel(s)

################## Abandon Post-Estimation ##################

LPM.Abandon <- s

# Anova test
anova(LPM.Abandon, test="Chisq")


Data.Analysis.abandon <- Data.Analysis.abandon %>% 
  mutate(predict.dummy = ifelse(predict > 0.5, 1, 0)) %>% 
  mutate(False.Positive = ifelse(Abandon.pb == 0 & predict.dummy == 1, 1, 0)) %>% 
  mutate(False.Negative = ifelse(Abandon.pb == 1 & predict.dummy == 0, 1, 0))

View(Data.Analysis.abandon[Data.Analysis.abandon$False.Negative == 1,])
View(Data.Analysis.abandon[Data.Analysis.abandon$False.Positive == 1,])


# End