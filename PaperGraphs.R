
# Valores preditos do modelo em gráficos:

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


# install.packages("Zelig")

# instala o pacote de extração dos dados, se necessário
library(data.table)
#library(zeligverse)
library(scales)
library(tidyverse)
library(Zelig)


# Script with functions
source(paste0(ScriptFolder, "PaperFunctions.R"))



################## Load Data ##################


Data.Analysis <- fread(paste0(InputFolder, "Data.Analysis.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


################## Codebook ##################


# Codebook for papers variables:

## MunicOP_OP = If the municipality have or not participatory budget in one year
## lag.pb = lag of MunicOP_OP
## MunicOP_OP.Acum = accumulated years number of continuous adoption of pb
## LeftParty = victory of left parties (PT, PDT, PSB and PCdoB)
## ptwin = Victory of pt
## VictoryPTAfter202 = Victory of the PT before 2002 (discrete) * -1
## ChangeEffect2002 = Change in effect after 2002 (discrete)
## BudgetPP.log = Anual budget per capita.
## InvestPer = Percentual of investiments of anual budget
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



# Create log variables
Data.Analysis.Complete <- Data.Analysis.Complete %>% 
  mutate(population.log = log(population)) %>% 
  mutate(BudgetPP.log = log(BudgetPP))

map_chr(Data.Analysis.Complete, class)

# Cases by year  
table(Data.Analysis.Complete$year)

  
################## Models ##################

# Basic Model
LPM.pb.Min <- lm(MunicOP_OP ~ 
                   # Lag dependent variable (LDV)
                   lag.pb + MunicOP_OP.Acum + 
                   # population
                   population.log + population.log:LeftParty  + population.log:ptwin +
                   # PT variables
                   ptwin + VictoryPTAfter2002 + LeftParty + # ptwin:ContinuityMayor +
                   VictoryPTAfter2002:population.log + 
                   # Political Variables
                   continuitypartpref + MayorsVulnerability + legprefpower + 
                   ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
                   # Finantial variables
                   BudgetPP.log + InvestPer + lag.pb:InvestPer + BudgetPP.log:InvestPer + 
                   # Time Variables
                   YearDummies2004 + YearDummies2008 + YearDummies2012
                 ,  
                 # Dataset
                 data = Data.Analysis.Complete)


# Check model results
checkModel(LPM.pb.Min)

# Zelig Model
Zelig.pb.Min <- zelig(MunicOP_OP ~ 
                        # Lag dependent variable (LDV)
                        lag.pb + MunicOP_OP.Acum + 
                        # population
                        population.log + population.log:LeftParty  + population.log:ptwin +
                        # PT variables
                        ptwin + VictoryPTAfter2002 + LeftParty +  # ptwin:ContinuityMayor +
                        VictoryPTAfter2002:population.log + 
                        # Political Variables
                        continuitypartpref + MayorsVulnerability + legprefpower + 
                        ContinuityMayor + ContinuityMayor:lag.pb + lag.pb:continuitypartpref +
                        # Finantial variables
                        BudgetPP.log + InvestPer + lag.pb:InvestPer + BudgetPP.log:InvestPer + 
                        # Time Variables
                        YearDummies2004 + YearDummies2008 + YearDummies2012
                      ,
                      # Dataset
                      data = Data.Analysis.Complete,
                      model = "ls",
                      cite = FALSE)

summary(Zelig.pb.Min)


################## Interaction between Investiments and lag.pb ##################

# Distribution of InvestPer
hist(Data.Analysis.Complete$InvestPer)
summary(Data.Analysis.Complete$InvestPer)

# Quantities of interest
SemPB <- setx(Zelig.pb.Min, InvestPer = seq(0.02, 0.2, by=0.02), lag.pb = 0)
ComPB <- setx(Zelig.pb.Min, InvestPer = seq(0.02, 0.2, by=0.02), lag.pb = 1)

# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = SemPB, x1 = ComPB)
# summary(s.out)
ci.plot(s.out, var = "InvestPer", ci = 90, leg = 0, 
        xlab = "Percentual de investimento municipal",
        ylab = "Chance de adoção do OP")


# ggplot graph

# Extract simulated data
qi.Values <- list(SemPB, ComPB)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "InvestPer", ci = 90)
levels(plotdata$Group) <- c("Sem PB", "Com PB")


#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  theme_classic(base_size = 15) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  xlab("Percentual de investimento municipal") + 
  ylab("Probabilidade de OP") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  scale_fill_manual(values=c("light blue", "orange"), name="Legenda")


rm(ComPB, SemPB, s.out)
rm(qi.Values, plotdata)

################## Interaction between Population and PT before and After 2002 ##################

# Checking population.log distribution
hist(Data.Analysis.Complete$population.log)
summary(Data.Analysis.Complete$population.log)

# Quantities of interest
PT.Antes2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0)
PT.Depois2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 1)

# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = PT.Antes2002, x1 = PT.Depois2002)
ci.plot(s.out, var = "population.log", ci = 90, leg = 0, 
        xlab = "População (log)",
        ylab = "Chance de adoção do OP")



# Extract simulated data
qi.Values <- list(PT.Antes2002, PT.Depois2002)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "population.log", ci = 90)
levels(plotdata$Group) <- c("PT de Antes 2002", "PT Depois de 2002")


#plot in ggplot2
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 15) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  xlab("População (log)") + 
  ylab("Probabilidade de OP") + 
  scale_y_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.6) + 
  scale_fill_manual(values=c("red", "green", "blue"), name="Legenda")


# Free memory
rm(PT.Depois2002, PT.Antes2002)
rm(plotdata, s.out, qi.Values)



################## Demography and Ideology ##################

# Quantities of interest
PT <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1)
PT.Depois2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 1)
PT.Antes2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0)
Esquerda <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 0, LeftParty = 1, VictoryPTAfter2002 = 0)
CentroDireita <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 0, LeftParty = 0, VictoryPTAfter2002 = 0)

# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = PT, x1 = Esquerda)
ci.plot(s.out, var = "population.log", ci = 90, leg = 0)

# Extract simulated data
qi.Values <- list(PT, Esquerda, CentroDireita)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "population.log", ci = 90)
levels(plotdata$Group) <- c("PT", "Esquerda", "Centro Direita")


#plot in ggplot2
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 15) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  xlab("População (log)") + 
  ylab("Probabilidade de OP") + 
  scale_y_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.6) + 
  scale_fill_manual(values=c("red", "green", "blue"), name="Legenda")


## Comparando o PT Depois de 2002 e os outros partidos

# Extract simulated data
qi.Values <- list(PT.Depois2002, Esquerda, CentroDireita)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "population.log", ci = 90)
levels(plotdata$Group) <- c("PT Depois de 2002", "Esquerda", "Centro Direita")


#plot in ggplot2
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 15) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  xlab("População (log)") + 
  ylab("Probabilidade de OP") + 
  scale_y_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.6) + 
  scale_fill_manual(values=c("red", "green", "blue"), name="Legenda")


rm(PT, PT.Depois2002, PT.Antes2002, CentroDireita, Esquerda)
rm(plotdata, s.out, qi.Values)


##################### Modelo Spada ###########################

Zelig.pb <- zelig(MunicOP_OP ~ 
                    # Lag dependent variable (LDV)
                    lag.pb + # MunicOP_OP.Acum + 
                    # population
                    population.log + # population.log:ptwin + population.log:LeftParty  + 
                    # PT variables
                    ptwin + VictoryPTAfter2002 + 
                    # Political Variables
                    continuitypartpref + MayorsVulnerability + legprefpower +  MayorControlCouncil +
                    # Finantial variables
                    taxrevenues + balsheetrev + 
                    # Time Variables
                    YearDummies2004 + YearDummies2008 + YearDummies2012
                  ,  
                  # Dataset
                  data = Data.Analysis.Complete,
                  model = "ls",
                  cite = FALSE)

                      

summary(Zelig.pb)



x.low <- setx(Zelig.pb, ptwin = 1, VictoryPTAfter2002 = 0)
x.high <- setx(Zelig.pb, ptwin = 1, VictoryPTAfter2002 = 1)
s.out <- sim(Zelig.pb, x = x.low, x1 = x.high)


s.out <- sim(Zelig.pb.Min, x = x.low, x1 = x.high)

summary(s.out)

ev <- s.out[["sim.out"]][["x"]][["ev"]][[1]]
ev1 <- s.out[["sim.out"]][["x1"]][["ev"]][[1]]

df.summary <- rbind(quantile(ev, probs = c(0.025, 0.5, 0.975)), 
                    quantile(ev1, probs = c(0.025, 0.5, 0.975))) %>% 
  as.data.frame() %>% 
  set_names("ymin", "ymean", "ymax") %>% 
  mutate(x = as.factor(row_number() - 1))

levels(df.summary$x) <- c("Antes 2002", "Depois 2002")
  
ggplot(df.summary, aes(x = x, y = ymean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  theme_classic(base_size = 15) + 
  xlab("Momento Político") + 
  ylab("Probabilidade de OP") +
  scale_y_continuous(labels = scales::percent)

# Podemos ver que nessa análise não interativa, a chance do PT implementar o PB
# depois de 2002 cai muito!

rm(x.low, x.high, s.out)
rm(ev, ev1, df.summary)
rm(Zelig.pb, Zelig.pb.Min, LPM.pb.Min)
rm(Data.Analysis, Data.Analysis.Complete)

# End