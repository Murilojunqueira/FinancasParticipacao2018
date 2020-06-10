
## Working paper "Why has the Participatory Budgeting declined in Brazil?"

# LPM and Logit models for the analysis. File 1 of 3

# By Murilo Junqueira e Carla Bezerra

# Created: 2018-05-09


################## Setup Working Space and loading data ##################

# devtools::install_github('IQSS/Zelig')

# Carrega os pacotes necessários
library(Zelig)
library(scales)
# library(sjPlot)
library(stargazer)
library(ggpubr)
library(gridExtra)
library(extrafont)

# Load Cambria font
windowsFonts(sans="Cambria")
loadfonts(device="win", quiet = TRUE)
loadfonts(device="postscript", quiet = TRUE)

# Script with functions

source("src/generalFunctions/ChechBDV.R") # "Check Binary Dependent Variable Model
source("src/generalFunctions/GraphData.R") # to simulate graphs
source("src/generalFunctions/getSimData.R") # to simulate graphs
source("src/generalFunctions/exp_f.R") # Convert a logarithmic scale to original scale

# Load Data 
DataAnalysis <- fread("data/analysisdata/DataAnalysis.csv", 
                      sep = ";", dec = ",",
                      stringsAsFactors = FALSE)

# Check data
names(DataAnalysis)

################## Codebook ##################

# Codebook for papers variables:

## Munic_Id = municipality code
## year = year (some variables are 4 years period)
## MunicOP_OP = If the municipality have or not participatory budget in one year
## UF_Id = State initials
## Munic_Id6 = municipality code (6 digits) 
## lag_pb = lag of MunicOP_OP
## log (population) = Log of population
## MunicOP_OP_Acum = accumulated years number of continuous adoption of pb
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
## BudgetPP_log = Anual budget per capita.
## InvestPer = Percentual of investiments of anual budget
## taxrevenues == Tax share of revenues
## balsheetrev == Financial viability index
## InvestPer = Percentual of investiments (investiments/total Budget)
## Investpp = Investiment per capita
## YearDummies1996 = Period 3 (1996-2000)
## YearDummies2000 = Period4 (2001-2004)
## YearDummies2004 = Period5 (2005-2008)
## YearDummies2008 = Period 6 (2009-2012)



################## Select and check data ##################

## avoid city duplication
DataAnalysis_Filter <- DataAnalysis %>% 
  distinct(Munic_Id, year, .keep_all = TRUE) %>% 
  rename(lag_pb = lag.pb) %>% 
  rename(MunicOP_OP_Acum = MunicOP_OP.Acum)

# names(DataAnalysis_Filter)

# Filter only municipalities with more than 50k pop in 1996
DataAnalysis_Filter <- DataAnalysis_Filter %>% 
  # Filtering in 1996 because we don't have data for 1992
  mutate(Sample.flag = ifelse(year == 1996 & population > 50000, 1, 0)) %>% 
  group_by(Munic_Id) %>% 
  mutate(Sample.Selection = max(Sample.flag, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::filter(Sample.Selection == 1) %>% 
  select(-Sample.flag, -Sample.Selection)


# Check complete cases
DataAnalysis_Complete <- DataAnalysis_Filter %>% 
  dplyr::filter(year > 1992) %>% 
  select(-CoalitionPref) %>% 
  na.omit()

# Check complete cases
map_int(DataAnalysis_Complete, function(x) sum(is.na(x), na.rm = TRUE))

# Create log variables
DataAnalysis_Complete <- DataAnalysis_Complete %>% 
  mutate(population_log = log(population)) %>% 
  mutate(BudgetPP_log = log(BudgetPP))

# Check variable class
map_chr(DataAnalysis_Complete, class)

# Cases by year  
table(DataAnalysis_Complete$year)


################## Models ##################

# Basic Model
# A. Basic LPM Model with no interactions

LPM_pb_Basic <- lm(MunicOP_OP ~ 
                   # Lag dependent variable (LDV)
                   lag_pb + MunicOP_OP_Acum + 
                   # population
                   population_log +
                   # PT variables
                   ptwin + VictoryPTAfter2002 + LeftParty +
                   # Political Variables
                   continuitypartpref + MayorsVulnerability + legprefpower + 
                   ContinuityMayor +
                   # Finantial variables
                   BudgetPP_log + InvestPer +
                   # Time Variables
                   YearDummies2004 + YearDummies2008 + YearDummies2012 + YearDummies2016
                 ,  
                 # Dataset
                 data = DataAnalysis_Complete)


# Check model results
checkModel(LPM_pb_Basic)

# B1. Basic LPM Model with interactions

LPM_pb_Min <- lm(MunicOP_OP ~ 
                   # Lag dependent variable (LDV)
                   lag_pb + MunicOP_OP_Acum + 
                   # population
                   population_log + population_log:LeftParty  + population_log:ptwin +
                   # PT variables
                   ptwin + VictoryPTAfter2002 + LeftParty + # ptwin:ContinuityMayor +
                   VictoryPTAfter2002:population_log + 
                   # Political Variables
                   continuitypartpref + MayorsVulnerability + legprefpower + 
                   ContinuityMayor + ContinuityMayor:lag_pb + lag_pb:continuitypartpref +
                   # Finantial variables
                   BudgetPP_log + InvestPer + lag_pb:InvestPer + BudgetPP_log:InvestPer + 
                   lag_pb:BudgetPP_log +
                   # Time Variables
                   YearDummies2004 + YearDummies2008 + YearDummies2012 + YearDummies2016
                 ,  
                 # Dataset
                 data = DataAnalysis_Complete)


# Check model results
checkModel(LPM_pb_Min)

# B2. Zelig Model (same as B model, but using zelig package)
Zelig_pb_Min <- zelig(MunicOP_OP ~ 
                        # Lag dependent variable (LDV)
                        lag_pb + MunicOP_OP_Acum + 
                        # population
                        population_log + population_log:LeftParty  + population_log:ptwin +
                        # PT variables
                        ptwin + VictoryPTAfter2002 + LeftParty + # ptwin:ContinuityMayor +
                        VictoryPTAfter2002:population_log + 
                        # Political Variables
                        continuitypartpref + MayorsVulnerability + legprefpower + 
                        ContinuityMayor + ContinuityMayor:lag_pb + lag_pb:continuitypartpref +
                        # Finantial variables
                        BudgetPP_log + InvestPer + lag_pb:InvestPer + BudgetPP_log:lag_pb + 
                        BudgetPP_log:InvestPer +
                        # Time Variables
                        YearDummies2004 + YearDummies2008 + YearDummies2012
                      , 
                      # Dataset
                      data = DataAnalysis_Complete,
                      model = "ls",
                      cite = FALSE)

summary(Zelig_pb_Min)

################## Table 1: Variables Description ##################

# Variables Names
VarLabels <- c('Participatory Budget (PB)', 'PB in t-1 (LDV)', 'PB Accumulative', 'City Population', 
               'PT Mayor', 'PT Mayor After 2002', 'Left Party (including PT)', 'Party continuity', 
               'Mayor Continuity (re-election)', 'Mayors Vulnerability', 'Mayors Legislative Power', 
               'City Budget per capita', 'Investment Share', 'City Population (log)', 
               'City Budget per capita (log)')


# Create summary table
stargazer(as.data.frame(DataAnalysis_Complete),
          # Output table 
          # out = "doc/Tables/Table1.html",
          # Remove non used variables
          omit = c("Munic_Id", "year", "UF_Id", "MayorElecNumber", "ChangeEffect2002", 'Investpp',
                   "taxrevenues", "TransferDep", "balsheetrev", "MayorControlCouncil", "LeftShare",
                   "YearDummies"),
          # Remove non used statistics
          omit.summary.stat = c("p25", "p75"),
          digits = 2,
          # Insert proper labels in variables
          covariate.labels = VarLabels)

rm(VarLabels)

################## Table 2: Model Results ##################

# Covariates labels
VarLabels <- c("Lag Dependent Variable (PBi,t-1)", "PB Accumulative", "Population (log)", 
               "PT Mayor", "PT Mayor After 2002", "Left Party (including PT)", "Party continuity", 
               "Mayors Vulnerability", "Mayors Legislative Power", "Mayor Continuity (re-election)", 
               "City Budget per capita (log)", "Investment Share", "Year Dummies 2004", 
               "Year Dummies 2008", "Year Dummies 2012", "Year Dummies 2016", "Population (log) * Left Party",
               "Population (log) * PT Mayor", "Population (log) * PT Mayor After 2002", 
               "LDV * Mayor Continuity (re-election)", 
               "LDV * Party continuity", "LDV * Investment Share", "City Budget per capita (log) * Investment Share", 
               "LDV * City Budget per capita (log)")


# Create regression summary table
ModelResults <- stargazer(LPM_pb_Basic, LPM_pb_Min, 
                          # Output table 
                          # out = "doc/Tables/Table2.html",
                          # Table Parameters:
                          type = "html", 
                          header = FALSE,
                          out.header = FALSE,
                          dep.var.labels.include = FALSE,
                          dep.var.caption = "Dependent Variable: Participatory Budget (PB)",
                          column.labels=c("Basic Model","Interactive Model"),
                          single.row=TRUE,
                          # Insert proper labels in variables
                          covariate.labels = VarLabels)

rm(VarLabels)

################## Figure 1: PB over time ##################

# Legend order
Order <- list(Total = 1, PT = 2, Other_Left = 3, 
              Center = 4, Right = 5) %>% 
  as.data.frame() %>% gather(Group_OP, order)


# Frame 1: PB total case evolution
# Frame 2: PB total case evolution by party
# Frame 3: Share of party mayors in cities with PB

# Data frame with number os PB over the years
Data_Frame1 <- DataAnalysis_Filter %>% 
  group_by(year) %>% 
  summarise(Total = sum(MunicOP_OP, na.rm = TRUE))

# Check data
Data_Frame1

# Data Frame with number and share of cities with PB over the years by party
Data_Frame23 <- DataAnalysis_Filter %>% 
  select(year, Munic_Nome, UF_Id, MayorName, MayorParty, LeftParty, MunicOP_OP, ptwin) %>% 
  mutate(Other_Left = ifelse(ptwin == 0 & LeftParty == 1, 1, 0)) %>%
  mutate(Center = ifelse(MayorParty %in% c("PMDB", "PSDB", "PV"), 1, 0)) %>% 
  mutate(Right = ifelse(LeftParty == 0 & Center == 0, 1, 0)) %>% 
  rename(PT = ptwin) %>% 
  gather(Group_OP, Ideology_Value, PT:Right) %>% 
  dplyr::filter(Ideology_Value == 1) %>% 
  group_by(year, Group_OP) %>%
  summarise(YearCount = n(),
            Freq_OP = sum(MunicOP_OP, na.rm = TRUE)) %>% 
  mutate(Per_OP = Freq_OP/YearCount) %>% 
  left_join(Order, by = "Group_OP") %>% 
  mutate(Group_OP = fct_reorder(as_factor(Group_OP), order)) %>% 
  arrange(year, order)

# Check data
# View(Data_Frame23)


# Frame 1: PB total case evolution
Frame1 <- ggplot(data = Data_Frame1,  aes(x = year, y = Total)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  geom_line(aes(y = Total), colour = "grey", size = 1) + 
  ggtitle("Number of cities with PB - 1992-2016") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks=c(seq(1992, 2016, by = 4)), labels=seq(1992, 2016, by = 4)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


Frame1

# Frame 2: PB total case evolution by party
Frame2 <- ggplot(data = Data_Frame23, aes(x = year, y = Freq_OP, fill = Group_OP)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  ggtitle("Number of cities with PB by party - 2000-2016") +
  geom_line(aes(y = Freq_OP, colour = Group_OP), size = 1) + 
  scale_color_manual(values=c(PT  = "red",
                              Other_Left = 'green',
                              Center = "purple",
                              Right = "blue"), name = "Mayor Party: ") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks=c(seq(1992, 2016, by = 4)), labels=seq(1992, 2016, by = 4)) +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

Frame2

# Frame 3: Share of party mayors in cities with PB
Frame3 <- ggplot(data = Data_Frame23, aes(x = year, y = Per_OP, fill = Group_OP)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  geom_line(aes(y = Per_OP, colour = Group_OP), size = 1) + 
  scale_color_manual(values=c(Total = "orange",
                              PT  = "red",
                              Other_Left = 'green',
                              Center = "purple",
                              Right = "blue"), name = "Mayor Party: ") +
  ggtitle("Share of party mayors in cities with PB") +
  labs(x = "Year", y = "") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(seq(1992, 2016, by = 4)), labels=seq(1992, 2016, by = 4)) +
  theme(legend.position="bottom", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

Frame3


# Combine all frames and make some annotations
Figure1 <- grid.arrange(Frame1, Frame2, Frame3, nrow = 3) 
# %>% 
#   annotate_figure(bottom = 
#                     text_grob("\nNote: Center parties are PMDB, PSDB and PV. Other Left-wing parties are PSB, PDT, PC do B 
#                     and PSOL. All other cases (22 parties) are labeled as right-wing parties. Only cities with 
#                     more than 50,000 people in 1996 were considered. Given that the 1992 election data is 
#                     missing, we cannot show the 1996 party information.", size = 10))

# Show Graph
Figure1

# Print plot
ggsave(filename = "doc/figures/Figure1.png", plot = Figure1,
       width = 10, height = 16, units = "cm", device = "png")

# Free memory
rm(Order, Data_Frame1, Data_Frame23, Frame1, Frame2, Frame3, Figure1)



################## Figure 2: Interaction between Population and PT before and After 2002 ##################

# Checking population_log distribution
hist(DataAnalysis_Complete$population_log)
summary(DataAnalysis_Complete$population_log)


# Quantities of interest
PT.Antes2002 <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0)
PT.Depois2002 <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 1)

# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = PT.Antes2002, x1 = PT.Depois2002)
ci.plot(s_out, var = "population_log", ci = 95, leg = 0, 
        xlab = "Population (log)", 
        ylab = "PB adoption probability")

#Same data in in ggplot2
qi_Values <- list(PT.Antes2002, PT.Depois2002)
plotdata <- GraphData(qi_Values, Zelig_pb_Min, "population_log", ci = 95)
levels(plotdata$Group) <- c("Before 2002", "After 2002")

# Figure 3

#plot in ggplot2
ggplot(data=plotdata, aes(x = population_log, y =mean, fill = Group)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  labs(x = "Population (log)", y = "Probability of PB") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(10:16), labels=c(exp_f(10:16))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  scale_fill_manual(values=c("#ba2121", "#ffc3a0"), name = "Period") +
  theme(legend.position="bottom", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ggsave(filename = "doc/figures/Figure2.png",
       width = 11.5, height = 8, units = "cm", device = "png")



# Free memory
rm(PT.Depois2002, PT.Antes2002)
rm(plotdata, s_out, qi_Values)



##################  Figure 3: Demography and Ideology ##################


# Quantities of interest
PT <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0.5)
PT.Depois2002 <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 1)
PT.Antes2002 <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 1, LeftParty = 1, VictoryPTAfter2002 = 0)
Esquerda <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 0, LeftParty = 1, VictoryPTAfter2002 = 0)
CentroDireita <- setx(Zelig_pb_Min, population_log = seq(10, 16, by=0.5), ptwin = 0, LeftParty = 0, VictoryPTAfter2002 = 0)

# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = PT, x1 = Esquerda)
ci.plot(s_out, var = "population_log", ci = 95, leg = 0)

# Extract simulated data
qi_Values <- list(PT, Esquerda, CentroDireita)
plotdata <- GraphData(qi_Values, Zelig_pb_Min, "population_log", ci = 95)
levels(plotdata$Group) <- c("PT   ", "Left   ", "Center-Right")

#plot in ggplot2
ggplot(data=plotdata, aes(x = population_log, y =mean, fill = Group)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  xlab("Population (log)") + 
  ylab("Probability of PB") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(10:16), labels=c(exp_f(10:16))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.4) + 
  scale_fill_manual(values=c("red", "green", "blue"), name="") + 
  theme(legend.position="bottom", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
 ggsave(filename = "doc/figures/Figure3.png",
        width = 11.5, height = 8, units = "cm", device = "png")

rm(PT, PT.Depois2002, PT.Antes2002, CentroDireita, Esquerda)
rm(plotdata, s_out, qi_Values)


##################  Figure 4: Continuity ##################

# Quantities of interest
PartyOnlyContinuity_NoPB <- setx(Zelig_pb_Min, continuitypartpref = 1, ContinuityMayor = 0, lag_pb = 0)
PartyOnlyContinuity_PB <- setx(Zelig_pb_Min, continuitypartpref = 1, ContinuityMayor = 0, lag_pb = 1)

PartyPref_NoPB <- setx(Zelig_pb_Min, continuitypartpref = 1, ContinuityMayor = 1, lag_pb = 0)
PartyPref_PB <- setx(Zelig_pb_Min, continuitypartpref = 1, ContinuityMayor = 1, lag_pb = 1)

# Extract simulated data
qi_Values <- list(PartyOnlyContinuity_NoPB, PartyPref_NoPB, 
                  PartyOnlyContinuity_PB, PartyPref_PB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "range", ci = 95, discrete = TRUE)
levels(plotdata$Group) <- c("Party Continuity (No PB)", "Mayor Reelection (No PB)",
                            "Party Continuity (With PB)", "Mayor Reelection (With PB)")

#plot in ggplot2
ggplot(data=plotdata, aes(x = Group, y =mean)) + 
  theme_classic(base_size = 11, base_family = "Cambria") + 
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +   
  labs(x = "Administrative Continuity", y = "Probability of PB") +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
 ggsave(filename = "doc/figures/Figure4.png",
        width = 11.5, height = 8, units = "cm", device = "png")

rm(PartyOnlyContinuity_NoPB, PartyOnlyContinuity_PB, PartyPref_NoPB, PartyPref_PB)
rm(plotdata, s_out, qi_Values)

##################  Figure 5: Interaction between Budget per capita (log) and lag_pb ##################

# Distribution of InvestPer
hist(DataAnalysis_Complete$BudgetPP_log)
summary(DataAnalysis_Complete$BudgetPP_log)

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, BudgetPP_log = seq(6, 9, by=0.25), lag_pb = 0)
ComPB <- setx(Zelig_pb_Min, BudgetPP_log = seq(6, 9, by=0.25), lag_pb = 1)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)

#english subtitles
ci.plot(s_out, var = "BudgetPP_log", ci = 95, leg = 0, 
        main = "Interaction Marginal Effects",
        xlab = "Budget per capita",
        ylab = "PB adoption/continuity")

# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "BudgetPP_log", ci = 95)
levels(plotdata$Group) <- c("No PB", "Adopts PB") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = BudgetPP_log, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.3/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(6:9), labels=c(exp_f(6:9))) +
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Budget per capita", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 11, base_family = "Cambria") + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

# Print plot
 ggsave(filename = "doc/figures/Figure5.png",
        width = 11.5, height = 8, units = "cm", device = "png")

rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)




##################  Figure 6: Interaction between Investiments and lag_pb ##################

# Distribution of InvestPer
hist(DataAnalysis_Complete$InvestPer)
summary(DataAnalysis_Complete$InvestPer)

# Quantities of interest
SemPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.025), lag_pb = 0)
ComPB <- setx(Zelig_pb_Min, InvestPer = seq(0, 0.25, by=0.025), lag_pb = 1)


# Zelig Graph
s_out <- sim(Zelig_pb_Min, x = SemPB, x1 = ComPB)

# summary(s_out)

#english subtitles
ci.plot(s_out, var = "InvestPer", ci = 95, leg = 0, 
        xlab = "Investimento Municipal (%)",
        ylab = "Chance de adotar o OP")

# Same info in ggplot graph

# Extract simulated data
qi_Values <- list(SemPB, ComPB)

plotdata <- GraphData(qi_Values, Zelig_pb_Min, "InvestPer", ci = 95)
levels(plotdata$Group) <- c("No PB", "Adopts PB") 

#plot in ggplot2
ggplot(data=plotdata, aes(x = InvestPer, y =mean, fill = Group)) + 
  geom_line(aes(y =mean)) + 
  geom_line(aes(y =high), linetype="dashed", color=NA) + 
  geom_line(aes(y =low), linetype="dashed", color=NA) + 
  geom_bar(aes(y = (0.3/min(mean))*Freq/sum(Freq)), stat="identity", 
           fill = "grey", colour = "white", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  geom_ribbon(aes(ymin=low, ymax=high), alpha=0.5) + 
  labs(x = "Investment rate", y = "PB Adoption/Continuity") +
  scale_fill_manual(values=c("light blue", "orange"), name="Previous Adminsitration:") +
  theme_classic(base_size = 11, base_family = "Cambria") + 
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 

# Print plot
 ggsave(filename = "doc/figures/Figure6.png",
        width = 11.5, height = 8, units = "cm", device = "png")


rm(ComPB, SemPB, s_out)
rm(qi_Values, plotdata)


# End