
## Working paper "Why has the Participatory Budgeting declined in Brazil?"

# Preficted values Graphs and Results. File 3 of 3.

# By Murilo Junqueira e Carla Bezerra

# Created: 2018-05-09
# Last Modified: 2018-07-07-10

################## Setup Working Space ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Análise/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/FinancasParticipacao2018/"

# InputFolder <- "C:/Users/Carla/FinancasParticipacao2018/data/" 
# OutputFolder <- "C:/Users/Carla/FinancasParticipacao2018/data/"
# ScriptFolder <- "C:/Users/Carla/FinancasParticipacao2018/"


# Check folders
dir.exists(c(InputFolder, OutputFolder, ScriptFolder))


# Set Working directory
setwd(ScriptFolder)

# install.packages("Zelig")

# instala o pacote de extração dos dados, se necessário
library(data.table)
#library(zeligverse)
library(scales)
library(tidyverse)
library(Zelig)
library(ggthemes)

# Script with functions
source(paste0(ScriptFolder, "PaperFunctions.R"))



################## Load Data ##################


Data.Analysis <- fread(paste0(InputFolder, "Data.Analysis.csv"), 
                       sep = ";", dec = ",",
                       stringsAsFactors = FALSE)


################## Codebook ##################


# Codebook for papers variables:

## MunicOP_OP = If the municipality have or not participatory budget in one year
## Adopt.pb = Adoption of pb,  Dependent Variable of models 1 and 2
## Abandon.pb = Adandon of pb, Dependent Variable of models 3 and 4
## lag.pb = lag of MunicOP_OP
## log (population) = Log of population
## MunicOP_OP.Acum = accumulated years number of continuous adoption of pb
## LeftParty = victory of left parties (PT, PDT, PSB and PCdoB)
## ptwin = Victory of the PT (discrete)
## VictoryPTAfter202 = Victory of the PT before 2002 (discrete) * -1
## ChangeEffect2002 = Change in effect after 2002 (discrete)
## BudgetPP.log = Anual budget per capita.
## InvestPer = Percentual of investiments of anual budget
## taxrevenues == Tax share of revenues
## balsheetrev == Financial viability index
## InvestPer = Percentual of investiments (investiments/total Budget)
## Investpp = Investiment per capita
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

# names(Data.Analysis)

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

# A. Basic LPM Model [Minimal LPM (3) in PaperModels]

LPM.pb.Min <- lm(MunicOP_OP ~ 
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
                   BudgetPP.log:lag.pb + 
                   # Time Variables
                   YearDummies2004 + YearDummies2008 + YearDummies2012
                 ,  
                 # Dataset
                 data = Data.Analysis.Complete)


# Check model results
checkModel(LPM.pb.Min)

# B. Zelig Model
Zelig.pb.Min <- zelig(MunicOP_OP ~ 
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
                        BudgetPP.log + InvestPer + lag.pb:InvestPer + BudgetPP.log:lag.pb + 
                        BudgetPP.log:InvestPer + 
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
SemPB <- setx(Zelig.pb.Min, InvestPer = seq(0, 0.25, by=0.01), lag.pb = 0)
ComPB <- setx(Zelig.pb.Min, InvestPer = seq(0, 0.25, by=0.01), lag.pb = 1)


# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = SemPB, x1 = ComPB)
# summary(s.out)
#english subtitles
ci.plot(s.out, var = "InvestPer", ci = 90, leg = 0, 
        # main = "Interaction Marginal Effects",
        # xlab = "Local Investment (%)",
        # ylab = "PB adoption/continuity")
        xlab = "Investimento Municipal (%)",
        ylab = "Chance de adotar o OP")

# Same info in ggplot graph

# Extract simulated data
qi.Values <- list(SemPB, ComPB)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "InvestPer", ci = 90)
levels(plotdata$Group) <- c("No PB", "Adopts PB") 
#levels(plotdata$Group) <- c("Sem OP  ", "Com OP") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.5/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  #labs(title = "Probabilidade da adoção/continuidade do OP",
  #     subtitle = "Existência prévia de OP em interação com taxa de investimento",
  #     caption = "Fonte: Spada(2012)/ TSE/ IBGE",
  #     x = "Taxa de Investimento Municipal", y = "Adoção/Continuidade do OP") +
  #labs(title = "Probability of PB Adoption",
  labs(x = "Investment rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  #scale_fill_manual(values=c("light blue", "orange"), name="Administração prévia:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

ggsave("results/English_Investment_vs_Lag.png")

rm(ComPB, SemPB, s.out)
rm(qi.Values, plotdata)


################## Interaction between BudgetPP.log and lag.pb ##################

# Distribution of InvestPer
hist(Data.Analysis.Complete$BudgetPP.log)
summary(Data.Analysis.Complete$BudgetPP.log)

# Quantities of interest
SemPB <- setx(Zelig.pb.Min, BudgetPP.log = seq(6, 9, by=0.2), lag.pb = 0)
ComPB <- setx(Zelig.pb.Min, BudgetPP.log = seq(6, 9, by=0.2), lag.pb = 1)


# Zelig Graph
s.out <- sim(Zelig.pb.Min, x = SemPB, x1 = ComPB)
# summary(s.out)
#english subtitles
ci.plot(s.out, var = "BudgetPP.log", ci = 90, leg = 0, 
        # main = "Interaction Marginal Effects",
        # xlab = "Local Budget per capita (log) (%)",
        # ylab = "PB adoption/continuity")
        xlab = "Orçamento Municipal per capita (log) (%)",
        ylab = "Chance de adotar o OP")

# Same info in ggplot graph

# Extract simulated data
qi.Values <- list(SemPB, ComPB)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "BudgetPP.log", ci = 90)
levels(plotdata$Group) <- c("No PB", "Adopts PB") 
#levels(plotdata$Group) <- c("Sem PB  ", "Com PB") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = BudgetPP.log, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.5/max(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(6:9), labels=c(exp.f(6:9))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  # labs(title = "Probabilidade da adoção/continuidade do OP",
  #      subtitle = "Existência prévia de OP em interação com orçamento per capita",
  #      caption = "Fonte: Spada(2012)/ TSE/ IBGE", 
  #     x = "Orçamento Municipal per capita (log)", y = "Adoção/Continuidade do OP") +
  labs(x = "Investment rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") + 
  # scale_fill_manual(values=c("light blue", "orange"), name="Administração prévia:") +
  theme_classic(base_size = 14) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("results/English_Budget_vs_Lag.png")

rm(s.out, SemPB, ComPB)
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
        xlab = "Population (log)", 
        ylab = "PB adoption probability")

#Same data in in ggplot2
qi.Values <- list(PT.Antes2002, PT.Depois2002)
plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "population.log", ci = 90)
levels(plotdata$Group) <- c("Before 2002", "After 2002")
# levels(plotdata$Group) <- c("Antes de 2002", "Depois de 2002")


#plot in ggplot2
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 14) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  # labs(title = "Probabilidade de adoção do OP em Prefeituras do PT",
  #      subtitle = "Eleição do PT ao Governo Federal em interação com população",
  #      #caption = "Fonte: Spada(2012)/ TSE/ IBGE",
  #      x = "População (log)", y = "Adoção  de OP") +
  labs(x = "Population (log)", y = "Probability of PB") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(10:16), labels=c(exp.f(10:16))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  scale_fill_manual(values=c("#ba2121", "#ffc3a0"), name = "Período") +
  theme(legend.position="bottom", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("results/English_Population_vs_PT.png")

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
#levels(plotdata$Group) <- c("PT   ", "Esquerda   ", "Centro Direita")
levels(plotdata$Group) <- c("PT   ", "Left   ", "Center-Right")


#plot in ggplot2
ggplot(data=plotdata, aes(x = population.log, y =mean, fill = Group)) + 
  theme_classic(base_size = 15) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  # xlab("População (log)") + 
  # ylab("Probabilidade de OP") +
  xlab("Population (log)") + 
  ylab("Probability of PB") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(10:16), labels=c(exp.f(10:16))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4) + 
  # labs(title = "Probabilidade da adoção/continuidade do OP",
  #      subtitle = "Interação entre população e ideologia",
  #      caption = "Fonte: Spada(2012)/ TSE/ IBGE",
  #      x = "População (log)", y = "Adoção/Continuidade do OP") +
  scale_fill_manual(values=c("red", "green", "blue"), name="") + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("results/English_Demography_Ideology.png")

rm(PT, PT.Depois2002, PT.Antes2002, CentroDireita, Esquerda)
rm(plotdata, s.out, qi.Values)


################## Continuity ##################

summary(Zelig.pb.Min)


# Quantities of interest
PartyOnly.Continuity.NoPB <- setx(Zelig.pb.Min, continuitypartpref = 1, ContinuityMayor = 0, lag.pb = 0)
PartyOnly.Continuity.PB <- setx(Zelig.pb.Min, continuitypartpref = 1, ContinuityMayor = 0, lag.pb = 1)

PartyPref.NoPB <- setx(Zelig.pb.Min, continuitypartpref = 1, ContinuityMayor = 1, lag.pb = 0)
PartyPref.PB <- setx(Zelig.pb.Min, continuitypartpref = 1, ContinuityMayor = 1, lag.pb = 1)

# Script with functions
source(paste0(ScriptFolder, "PaperFunctions.R"))


# Extract simulated data
qi.Values <- list(PartyOnly.Continuity.NoPB, PartyPref.NoPB, 
                  PartyOnly.Continuity.PB, PartyPref.PB)

plotdata <- Graph.Data(qi.Values, Zelig.pb.Min, "range", ci = 90, discrete = TRUE)
# levels(plotdata$Group) <- c("Continuidade Partido (Sem OP)", "Reeleição Prefeito (Sem OP)",
#                            "Continuidade Partido (Com OP)", "Reeleição Prefeito (Com OP)")
 levels(plotdata$Group) <- c("Party Continuity (No PB)", "Mayor Reelection (No PB)",
                            "Party Continuity (With PB)", "Mayor Reelection (With PB)")

#plot in ggplot2
ggplot(data=plotdata, aes(x = Group, y =mean)) + 
  theme_classic(base_size = 15) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +   
  # labs(title = "Probabilidade da adoção/continuidade do OP",
  #      subtitle = "Efeitos da Continuidade Administrativa",
  #      caption = "Fonte: Spada(2012)/ TSE/ IBGE",
  #      x = "Continuidade Político-Adminsitrativa", y = "Adoção/Continuidade do OP") +
  labs(x = "Administrative_Continuity", y = "Probability of PB") +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/English_Administrative_Continuity.png")



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
                    YearDummies2004 + YearDummies2008 + YearDummies2012,  
                    # Dataset
                    data = Data.Analysis.Complete,
                    model = "ls",
                    cite = FALSE)


                      

summary(Zelig.pb)



x.low <- setx(Zelig.pb, ptwin = 1, VictoryPTAfter2002 = 0)
x.high <- setx(Zelig.pb, ptwin = 1, VictoryPTAfter2002 = 1)
s.out <- sim(Zelig.pb, x = x.low, x1 = x.high)


s.out <- sim(Zelig.pb.Min, x = x.low, x1 = x.high)

# summary(s.out)

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




rm(x.low, x.high, s.out)
rm(ev, ev1, df.summary)
rm(Zelig.pb, Zelig.pb.Min, LPM.pb.Min)
rm(Data.Analysis, Data.Analysis.Complete)

# End