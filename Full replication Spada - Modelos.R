
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


# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)
#library(zeligverse)
library(scales)


################## Load Data ##################


Data.Analysis <- fread(paste0(InputFolder, "Data.Analysis.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


################## Codebook ##################


# Codebook for papers variables:

## Adopt.pb = Adoption of pb,  Dependent Variable of models 1 and 2
## Abandon.pb = Adandon of pb, Dependent Variable of models 3 and 4
## VictoryPTAfter2002 = Victory of the PT before 2002 (discrete) * -1
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
   # Create lag variable
   mutate(lag.pb = lag(MunicOP_OP)) %>% 
  distinct(Munic_Id, year, .keep_all = TRUE)

names(Data.Analysis )

# Filter only municipalities with more than 50k pop in 1996
Data.Analysis <- Data.Analysis %>% 
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


map_int(Data.Analysis, function(x) sum(is.na(x), na.rm = TRUE))

map_int(Data.Analysis[Data.Analysis$year >= 1996,], 
        function(x) sum(is.na(x), na.rm = TRUE))

map_int(Data.Analysis.Complete, function(x) sum(is.na(x), na.rm = TRUE))

map_chr(Data.Analysis.Complete, class)

# Prevent type data problems

Data.Analysis.Complete <- Data.Analysis.Complete %>% 
  mutate(taxrevenues = as.numeric(sub(",", ".", taxrevenues))) %>% 
  mutate(balsheetrev = as.numeric(sub(",", ".", balsheetrev))) %>% 
  mutate(MayorsVulnerability = as.numeric(sub(",", ".", MayorsVulnerability))) %>% 
  mutate(legprefpower = as.numeric(sub(",", ".", legprefpower))) %>% 
  mutate(Investpp = as.numeric(sub(",", ".", Investpp))) %>% 
  mutate(InvestPer = as.numeric(sub(",", ".", InvestPer)))

# Cases by year  
table(Data.Analysis.Complete$year)


################## Models ##################



# I will ommit the variable mindist for now in order to include the 2012 election

# The first models is about the chance of exist pb in a given year (dependent variable is the existence of pb)

# Run basic model
LPM.pb <- lm(MunicOP_OP ~ 
               # PT variables
               ptwin + VictoryPTAfter2002 +
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
               # Finantial variables
               taxrevenues + balsheetrev + 
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012, 
             # Dataset
             data = Data.Analysis.Complete)

summary(LPM.pb)


# Now with population (log) as explanatory variable

LPM.pb <- lm(MunicOP_OP ~ 
               # population
               log(population) + 
               # PT variables
               ptwin + VictoryPTAfter2002 +
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
               # Finantial variables
               taxrevenues + balsheetrev + 
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012,  
             # Dataset
             data = Data.Analysis.Complete)

summary(LPM.pb)


# With lag dependent variable
LPM.pb <- lm(MunicOP_OP ~ 
               # Lag dependent variable (LDV)
               lag.pb + 
               # population
               log(population) + 
               # PT variables
               ptwin + VictoryPTAfter2002 +
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
               # Finantial variables
               taxrevenues + balsheetrev + 
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012,  
             # Dataset
             data = Data.Analysis.Complete)

summary(LPM.pb)


# With investment variables

LPM.pb <- lm(MunicOP_OP ~ 
               # Lag dependent variable (LDV)
               lag.pb + 
               # population
               log(population) + 
               # PT variables
               ptwin + VictoryPTAfter2002 +
               # Political Variables
               continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
               # Finantial variables
               taxrevenues + balsheetrev + InvestPer + log(Investpp) +
               # Time Variables
               YearDummies2004 + YearDummies2008 + YearDummies2012,  
             # Dataset
             data = Data.Analysis.Complete)

summary(LPM.pb)


LPM.pb <- glm(MunicOP_OP ~ 
                # Lag dependent variable (LDV)
                lag.pb + 
                # population
                log(population) + 
                # PT variables
                ptwin + VictoryPTAfter2002 +
                # Political Variables
                continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                # Finantial variables
                taxrevenues + balsheetrev + InvestPer + log(Investpp) +
                # Time Variables
                YearDummies2004 + YearDummies2008 + YearDummies2012,  
              # Dataset
              data = Data.Analysis.Complete,
              # Model
              family=binomial(link='logit'))

summary(LPM.pb)



# Now the chance of adoption of pb

## Logicaly it is necessary exclude the cases that the municipality have pb in t-1
## (it is not possible adopt pb if it is already adopted).

Data.Analysis.adopt <- Data.Analysis.Complete %>% 
  arrange(Munic_Id, year) %>%
  mutate(AlreadyAdopted = ifelse(Munic_Id == lag(Munic_Id) & MunicOP_OP == 1 & lag(MunicOP_OP) == 1, 1, 0)) %>% 
           filter(AlreadyAdopted == 0)

LPM.Adopt <- lm(Adopt.pb ~ 
                  # PT variables
                  ptwin + VictoryPTAfter2002 +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  # Finantial variables
                  taxrevenues + balsheetrev + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012, 
                # Dataset
                data = Data.Analysis.adopt)

summary(LPM.Adopt)


# Now with population (log) as explanatory variable

LPM.Adopt <- lm(Adopt.pb ~ 
                  # population
                  log(population) + 
                  # PT variables
                  ptwin + VictoryPTAfter2002 +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  # Finantial variables
                  taxrevenues + balsheetrev + 
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
                data = Data.Analysis.adopt)

summary(LPM.Adopt)



# With investment variables

LPM.Adopt <- lm(Adopt.pb ~ 
                  # Lag dependent variable (LDV)
                  lag.pb + 
                  # population
                  log(population) + 
                  # PT variables
                  ptwin + VictoryPTAfter2002 +
                  # Political Variables
                  continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                  # Finantial variables
                  taxrevenues + balsheetrev + InvestPer + log(Investpp) +
                  # Time Variables
                  YearDummies2004 + YearDummies2008 + YearDummies2012,  
                # Dataset
             data = Data.Analysis.adopt)

summary(LPM.Adopt)




# The Chance of abandon of pb (version 1)

LPM.Abandon <- lm(Abandon.pb ~ 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + 
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.Complete)

summary(LPM.Abandon)


# Now with population (log)  and LDV as explanatory variables:

LPM.Abandon <- lm(Abandon.pb ~ 
                    # Lag dependent variable (LDV)
                    lag.pb + 
                    # population
                    log(population) + 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + 
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.Complete)

summary(LPM.Abandon)

# With investment variables

LPM.Abandon <- lm(Abandon.pb ~ 
                    # Lag dependent variable (LDV)
                    lag.pb + 
                    # population
                    log(population) + 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + InvestPer + log(Investpp) +
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.Complete)

summary(LPM.Abandon)


# The Chance of abandon of pb (version 2)

## Logicaly it is not possible abandon pb if the municipality doens't previously adopted pb

# Delete municipalities that doens't previously adopted pb
Data.Analysis.abandon <- Data.Analysis.Complete %>% 
  arrange(Munic_Id, year) %>%
  filter(Abandon.pb == 1 | MunicOP_OP == 1)

  
table(Data.Analysis.abandon$year)


LPM.Abandon <- lm(Abandon.pb ~ 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + 
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.abandon)

summary(LPM.Abandon)


# Now with population (log) as explanatory variable

LPM.Abandon <- lm(Abandon.pb ~ 
                    # population
                    log(population) + 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + 
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                  data = Data.Analysis.abandon)

summary(LPM.Abandon) 


# With investment variables

LPM.Abandon <- lm(Abandon.pb ~ 
                    # population
                    log(population) + 
                    # PT variables
                    ptwin + VictoryPTAfter2002 +
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + MayorControlCouncil + legprefpower + 
                    # Finantial variables
                    taxrevenues + balsheetrev + InvestPer + log(Investpp) +
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                  # Dataset
                data = Data.Analysis.abandon)

summary(LPM.Abandon)



################## Post-Estimation ##################


# Anova test
anova(LPM.pb, test="Chisq")

# Accuracy 
predict <- predict(LPM.pb, type = 'response')
predict <- ifelse(predict > 0.5,1,0)

misClasificError <- mean(predict != Data.Analysis.Complete$MunicOP_OP)

print(paste('Accuracy',1-misClasificError))


#confusion matrix
predict <- predict(LPM.pb, type = 'response')
table(Data.Analysis.Complete$MunicOP_OP, predict > 0.5)

# Chance of true positive
177/(207 + 177) 


# Anova test
anova(LPM.Abandon, test="Chisq")

# Accuracy 
predict <- predict(LPM.Abandon, type = 'response')
predict <- ifelse(predict > 0.5,1,0)

misClasificError <- mean(predict != Data.Analysis.Complete$Abandon.pb)

print(paste('Accuracy',1-misClasificError))


#confusion matrix
predict <- predict(LPM.Abandon, type = 'response')
table(Data.Analysis.Complete$Abandon.pb, predict > 0.5)

46/(120+46)

# End