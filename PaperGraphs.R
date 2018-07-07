
<<<<<<< HEAD
# Valores preditos do modelo em gráficos:
=======
## Working paper "Why has the Participatory Budgeting declined in Brazil?"

# Preficted values Graphs and Results. File 3 of 3.

# By Murilo Junqueira

# Created: 2018-05-09
# Last Modified: 2018-02-07

#CarlaBezerra fork: 2018-04-07

>>>>>>> CarlaChanges/master

################## Setup Working Space ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
<<<<<<< HEAD
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"

=======
InputFolder <- "C:/Users/Carla/FinancasParticipacao2018/data/" 
OutputFolder <- "C:/Users/Carla/FinancasParticipacao2018/data/"
ScriptFolder <- "C:/Users/Carla/FinancasParticipacao2018/"
>>>>>>> CarlaChanges/master

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
<<<<<<< HEAD

=======
library(ggthemes)
>>>>>>> CarlaChanges/master

# Script with functions
source(paste0(ScriptFolder, "PaperFunctions.R"))



################## Load Data ##################


Data.Analysis <- fread(paste0(InputFolder, "Data.Analysis.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


################## Codebook ##################


# Codebook for papers variables:

<<<<<<< HEAD
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
=======
## Adopt.pb = Adoption of pb,  Dependent Variable of models 1 and 2
## Abandon.pb = Adandon of pb, Dependent Variable of models 3 and 4
## VictoryPTAfter2002 = Victory of the PT after 2002 (discrete)
## ptwin = Victory of the PT (discrete)
## taxrevenues = Tax share of revenues
## balsheetrev = Financial viability index
## continuitypartpref = City government continuity (discrete)
## MayorsVulnerability = Mayor's vulnerability
## MayorControlCouncil = Mayor controls the council (discrete)
## legprefpower = Mayor's share of council seats
## InvestPer = ???
## Investpp = ???
## lag.pb = ?
## log (population) = ?
## YearDummies2000 = Period 3 (1996-2000)
## YearDummies2004 = Period 4 (2001-2004)
## YearDummies2008 = Period 5 (2005-2008)
## YearDummies2012 = Period 6 (2009-2012)

## CARLA: Updated codebook variables. Some definitions missing.
>>>>>>> CarlaChanges/master


################## Select and check data ##################

<<<<<<< HEAD
=======
#CARLA: I would move this piece of code (that is repeated in PaperModels.R) to the Dataset building script.
>>>>>>> CarlaChanges/master

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

<<<<<<< HEAD
# Basic Model
=======
# A. Basic LPM Model [Minimal LPM (3) in PaperModels]
>>>>>>> CarlaChanges/master
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
<<<<<<< HEAD
                   YearDummies2004 + YearDummies2008 + YearDummies2012
                 ,  
                 # Dataset
                 data = Data.Analysis.Complete)
=======
                   YearDummies2004 + YearDummies2008 + YearDummies2012,  
                   # Dataset
                   data = Data.Analysis.Complete)
>>>>>>> CarlaChanges/master


# Check model results
checkModel(LPM.pb.Min)

<<<<<<< HEAD
# Zelig Model
=======
# B. Zelig Model
>>>>>>> CarlaChanges/master
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
<<<<<<< HEAD
                        YearDummies2004 + YearDummies2008 + YearDummies2012
                      ,
                      # Dataset
                      data = Data.Analysis.Complete,
                      model = "ls",
                      cite = FALSE)
=======
                        YearDummies2004 + YearDummies2008 + YearDummies2012,
                        # Dataset
                        data = Data.Analysis.Complete,
                        model = "ls", cite = FALSE)
>>>>>>> CarlaChanges/master

summary(Zelig.pb.Min)


################## Interaction between Investiments and lag.pb ##################

# Distribution of InvestPer
hist(Data.Analysis.Complete$InvestPer)
summary(Data.Analysis.Complete$InvestPer)

<<<<<<< HEAD
# Quantities of interest
SemPB <- setx(Zelig.pb.Min, InvestPer = seq(0.02, 0.2, by=0.02), lag.pb = 0)
ComPB <- setx(Zelig.pb.Min, InvestPer = seq(0.02, 0.2, by=0.02), lag.pb = 1)
=======
# Values of interest
SemPB <- setx(Zelig.pb.Min, InvestPer = seq(0, 0.3, by=0.01), lag.pb = 0)
ComPB <- setx(Zelig.pb.Min, InvestPer = seq(0, 0.3, by=0.01), lag.pb = 1)
>>>>>>> CarlaChanges/master

# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = SemPB, x1 = ComPB)
# summary(s.out)
<<<<<<< HEAD
ci.plot(s.out, var = "InvestPer", ci = 90, leg = 0, 
        xlab = "Percentual de investimento municipal",
        ylab = "Chance de adoção do OP")


# ggplot graph
=======
#portuguese subtitles
ci.plot(s.out, var = "InvestPer", ci = 90, leg = 0,
        main = "Efeitos Marginais da Interação",
        xlab = "Investimento Municipal (%)",
        ylab = "Adoção/continuidade do OP")



#english subtitles
ci.plot(s.out, var = "InvestPer", ci = 90, leg = 0, 
        main = "Interaction Marginal Effects",
        xlab = "Local Investment (%)",
        ylab = "PB adoption/continuity")


# Same info in ggplot graph
>>>>>>> CarlaChanges/master

# Extract simulated data
qi.Values <- list(SemPB, ComPB)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "InvestPer", ci = 90)
<<<<<<< HEAD
levels(plotdata$Group) <- c("Sem PB", "Com PB")
=======
levels(plotdata$Group) <- c("Sem OP", "Com OP") # c("no PB", "adopts PB")
>>>>>>> CarlaChanges/master


#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
<<<<<<< HEAD
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
=======
  theme_classic(base_size = 12) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  labs(title = "Probabilidade da adoção/continuidade do OP",
       subtitle = "Existência prévia de OP em interação com taxa de investimento",
       #caption = "Source: Spada(2012)/ TSE/ IBGE", 
       x = "Taxa de Investimento Municipal", y = "Adoção/Continuidade do OP") +
  #labs(title = "Probability of PB Adoption",
  #subtitle = "Previous PB existence effects in interaction with Investment rate",
  #caption = "Source: Spada(2012)/ TSE/ IBGE", 
  #x = "Investment rate", y = "PB Adoption/Continuity") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  scale_fill_manual(values=c("light blue", "orange"), name="Gestão Anterior") #name = "Previous Adminsitration")
>>>>>>> CarlaChanges/master


rm(ComPB, SemPB, s.out)
rm(qi.Values, plotdata)

################## Interaction between Population and PT before and After 2002 ##################

# Checking population.log distribution
hist(Data.Analysis.Complete$population.log)
summary(Data.Analysis.Complete$population.log)

<<<<<<< HEAD
# Quantities of interest
=======
# Values of interest
>>>>>>> CarlaChanges/master
PT.Antes2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0)
PT.Depois2002 <- setx(Zelig.pb.Min, population.log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 1)

# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = PT.Antes2002, x1 = PT.Depois2002)
ci.plot(s.out, var = "population.log", ci = 90, leg = 0, 
<<<<<<< HEAD
        xlab = "População (log)",
        ylab = "Chance de adoção do OP")


=======
        xlab = "População (log)", #Population
        ylab = "Chance de adoção do OP") #PB adoption probability



#Same data in in ggplot2
>>>>>>> CarlaChanges/master

# Extract simulated data
qi.Values <- list(PT.Antes2002, PT.Depois2002)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "population.log", ci = 90)
<<<<<<< HEAD
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
=======
levels(plotdata$Group) <- c("Antes de 2002", "Depois de 2002") #c("Before 2002", "After 2002")


#ggplot
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 12) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  labs(title = "Probabilidade de adoção do OP em Prefeituras do PT",
       subtitle = "Eleição do PT ao Governo Federal em interação com população",
       #caption = "Fonte: Spada(2012)/ TSE/ IBGE", 
       x = "População (log)", y = "Adoção  de OP") +
  #labs(title = "Probability of PB Adoption in PT Prefectures",
       #subtitle = " Federal Government Election effects in interaction with Population",
       #caption = "Source: Spada(2012)/ TSE/ IBGE", 
       #x = "Population (log)", y = "PB Adoption") +
  scale_y_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  scale_fill_manual(values=c("#ba2121", "#ffc3a0"), name = "Período")
>>>>>>> CarlaChanges/master


# Free memory
rm(PT.Depois2002, PT.Antes2002)
rm(plotdata, s.out, qi.Values)



################## Demography and Ideology ##################

<<<<<<< HEAD
# Quantities of interest
=======
# CARLA: tenho dúvida sobre usar esses gráficos aqui. Eu me concentraria nos dois primeiros, que já considero suficientes.

# Values of interest
>>>>>>> CarlaChanges/master
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
<<<<<<< HEAD
                    YearDummies2004 + YearDummies2008 + YearDummies2012
                  ,  
                  # Dataset
                  data = Data.Analysis.Complete,
                  model = "ls",
                  cite = FALSE)
=======
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                    # Dataset
                    data = Data.Analysis.Complete,
                    model = "ls",
                    cite = FALSE)
>>>>>>> CarlaChanges/master

                      

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