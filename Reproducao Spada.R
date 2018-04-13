
# Script para extrair dados do BD para a análise das finanças municipais
# e do probabilidade de sobrevivência do Orçamento Participativo.

# Criado por Murilo Junqueira

# Data criação: 2018-03-15
# Ultima modificação: 2018-03-15

################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
ScriptFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"
SpadaFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/PBCENSUS Spada/"



# instala o pacote de extração dos dados, se necessário
library(tidyverse)
library(data.table)
library(readstata13)
library(readxl)
library(scales)

# Dados de análise Spada
Spada.Data <- read.dta13(paste0(SpadaFolder, "PBCENSUS1989_2012data.dta"))

# Dados do censo do OP
Censo.OP <- read_excel(paste0(SpadaFolder, "05112017_PB CENSUS 2016.xlsx"), 
                                  sheet = "Final census") %>% filter(!is.na(codeipea))

SocioDemoEconomia <- fread(paste0(InputFolder, "SocioDemoEconomia.csv"), 
                           sep = ";", dec = ",",
                           stringsAsFactors = FALSE)

################## Tabelas ##################

names(Censo.OP)

# Tabela 01:


Adopted.Op <- Censo.OP %>% 
  mutate(Adopt1992 = pb1992) %>% 
  mutate(Adopt1996 = ifelse(pb1992 == 0 & pb1996 == 1, 1, 0)) %>% 
  mutate(Adopt2000 = ifelse(pb1996 == 0 & pb2000 == 1, 1, 0)) %>%
  mutate(Adopt2004 = ifelse(pb2000 == 0 & pb2004 == 1, 1, 0)) %>% 
  mutate(Adopt2008 = ifelse(pb2004 == 0 & pb2008 == 1, 1, 0)) %>% 
  mutate(Adopt2012 = ifelse(pb2008 == 0 & pb2012 == 1, 1, 0)) %>% 
  mutate(Adopt2016 = ifelse(pb2012 == 0 & pb2016 == 1, 1, 0)) %>% 
  mutate(Abandon1996 = ifelse(pb1992 == 1 & pb1996 == 0, 1, 0)) %>%
  mutate(Abandon2000 = ifelse(pb1996 == 1 & pb2000 == 0, 1, 0)) %>% 
  mutate(Abandon2004 = ifelse(pb2000 == 1 & pb2004 == 0, 1, 0)) %>% 
  mutate(Abandon2008 = ifelse(pb2004 == 1 & pb2008 == 0, 1, 0)) %>%
  mutate(Abandon2012 = ifelse(pb2008 == 1 & pb2012 == 0, 1, 0)) %>%
  mutate(Abandon2016 = ifelse(pb2012 == 1 & pb2016 == 0, 1, 0))


UnbrokeSeq <- function(x) {
  
  x <- x[!is.na(x)]
  unbroke <- 0
  for (i in seq_along(x)) {
    if(i == 1) {
      if(x[i] != 0){
        unbroke <- unbroke + 1
      }
    } else {
      if(x[i] != 0 & x[i-1] != 0){
        unbroke <- unbroke + 1
      } else {
        unbroke <- 0
      }
      
      if(x[i] != 0 & x[i-1] == 0){
        unbroke <- 1
      }
    }
  }
  return(unbroke)
}


FirstColumn <- which(names(Adopted.Op) == "pb1992")
LastColumnVector <- 3:8
UnbrokenMatrix <- data.frame(matrix(nrow = nrow(Adopted.Op), ncol = 0))

for (i in 1:length(LastColumnVector)) {
  # i <- 1  
  SelectMatrix <- select(Adopted.Op, FirstColumn:LastColumnVector[i])
  UnbrokenSeq <- as.numeric()
  
  for (j in seq_len(nrow(SelectMatrix))) {
    # j <- 1
    UnbrokenSeq[j] <- UnbrokeSeq(as.numeric(SelectMatrix[j,]))
  }
  vectorname <- paste0("Unbroke", names(Adopted.Op)[LastColumnVector[i]])
  UnbrokenMatrix[[vectorname]] <- UnbrokenSeq
}

rm(UnbrokeSeq, SelectMatrix, i, j)
rm(FirstColumn, LastColumnVector, vectorname)
rm(UnbrokenSeq)


Adopted.Op <- cbind(Adopted.Op, UnbrokenMatrix)
rm(UnbrokenMatrix)

names(Spada.Data)

Pop92 <- SocioDemoEconomia %>% 
  filter(SocioMunic_Ano == 1992) %>% 
  select(Munic_Id, SocioMunic_Populacao) %>% 
  rename(codeipea = Munic_Id) %>% 
  rename(pop = SocioMunic_Populacao) %>% 
  mutate(pop = as.numeric(pop)) %>% 
  mutate(codeipea = as.numeric(codeipea))
  

map_chr(Adopted.Op, typeof)
map_chr(Pop92, typeof)

names(Adopted.Op)

Adopted.Op.Table <- as_tibble(Adopted.Op) %>% 
  # Exclui Brasília
  filter(codeipea != "5300108") %>% 
  # Considera os valores nulos em pb2016 como sem OP (creio que essa a melhor interpretação)
  mutate(pb2016 = ifelse(is.na(pb2016), 0, pb2016)) %>% 
  select(codeipea, name, state, pb1992:pb2016, Adopt1992:Unbrokepb2016) %>% 
  gather(Variable, Var_Value, pb1992:Unbrokepb2016) %>% 
  mutate(year = substr(Variable, nchar(Variable) - 3, nchar(Variable))) %>% 
  mutate(variable2 = substr(Variable, 1, nchar(Variable) - 4)) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(Pop92, by = "codeipea") %>% 
  filter(pop > 50000) %>% select(-pop) %>% 
  mutate(variable3 = ifelse(variable2 == "Unbrokepb", paste0("UnbrokenPb_", Var_Value), variable2))


%>%
  group_by(variable2, year) %>% 
  summarise(ValorContagem = sum(Var_Value, na.rm = TRUE)) %>% 
  spread(year, ValorContagem)
  
table(Adopted.Op.Table$variable2)
table(Adopted.Op.Table$variable3)


head(Adopted.Op.Table)
names(Adopted.Op.Table)
View(Adopted.Op.Table)

rm(Pop92)
rm(Adopted.Op, Adopted.Op.Table)

################## Regressões ##################

table(Spada.Data$year)

names(Spada.Data)

# De/Para das variáveis do banco e do paper:

  ## ptwin == Victory of the PT before 2002 (discrete)
  ## mindist == Minimum Distance
  ## taxrevenues == Tax share of revenues
  ## balsheetrev == Financial viability index (?)
  ## legprefpower == Mayor's share of council seats


# Em dúvida:

  ## (continuitypref | continuitypartpref) == City government continuity (discrete)
  ##  Mayor controls the council (discrete)
    ### Usar: bestveradorparty & partpref



# Contruir / investigar melhor:

  ## Criar a variável dependente
    ### variável de adoção do pb
    ### variável de abandono do pb
  ## Change in effect after 2002 (discrete)
  ## Change in effect after 2002 (?)
  ## Mayor's vulnerability
    ### Utilizar as variáveis: (votiprefprim & secbestprefeitovote_ft
  ## Period 3 (1996-2000)
  ## Period4 (2001-2004)
  ## Period5 (2005-2008)
  ## Period 6 (2009-2012)



Spada.Data.OLS <- Spada.Data %>% 
  rename(Municipio = "Município") %>% 
  filter(pop > 50000) %>% 
  select(codeipea, Municipio, year, pb)

  
#mutate(PbAdoption = )
  
head(Spada.Data.OLS)
table(Spada.Data.OLS$year)
table(Spada.Data.OLS$pb)

LPM <- lm(data = Spada.Data.OLS)
