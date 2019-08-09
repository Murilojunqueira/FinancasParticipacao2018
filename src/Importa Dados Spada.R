# Script para Importar dados fornecidos por Paolo Spada:
# Censo dos Munic?pios com Or?amento Participativo.

# Criado por Murilo Junqueira

# Data cria??o: 2018-03-09
# Ultima modifica??o: 2018-03-09

# Documenta??o da base de dados: 
# https://participedia.net/en/content/brazilian-participatory-budgeting-census

################## Prepara ?rea de trabalho ##################

# Limpa mem?ria
rm(list=ls(all=TRUE))
gc()


# Os diret?rios de inser??o dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localiza??o dos scripts (ScriptFolder). Atualize se necess?rio!
InputFolder <- "C:/Users/mjunqueira/Dropbox/Acad?mico e Educa??o/Publica??es/2017 - Participa??o Carla/Dados/Dados Brutos/PBCENSUS Spada/"
OutputFolder <- "C:/Users/mjunqueira/Dropbox/Acad?mico e Educa??o/Publica??es/2017 - Participa??o Carla/Dados/BD csv/"
ScriptFolder <- "C:/Users/mjunqueira/Dropbox/Acad?mico e Educa??o/Publica??es/2017 - Participa??o Carla/Scripts R/"

# Instala o pacote readstata13, caso necess?rio
# install.packages("readstata13")

# Carrega os pacotes que ser?o usados nesse script.
library(tidyverse)
library(data.table)
#library(readstata13)
library(readxl)

################## Importa Dados Sobre Or?amento Participativo ##################

# Importa dados Spada no Stata.
# Preferimos importar do relat?rio em Excel, que est? mais claro.
# OP.Data <- read.dta13(paste0(InputFolder, "PBCENSUS1989_2012data.dta"))

OP.Data <-     read_excel(paste0(InputFolder, "05112017_PB CENSUS 2016.xlsx"), 
                          sheet = "Final census")

# Checa o banco de dados.
names(OP.Data)
View(OP.Data)


# Extrai os dados sobre OP e formata a base de dados
OP.Data.Select <- OP.Data %>% 
  # Seleciona apenas as colunas de indentifica??o no munic?pio e sobre o OP.
  # Os dados de ano est?o misturados aos dados sobre OP.
  select(codeipea, starts_with("pb")) %>% 
  # Transforma em base de dados curta (com n?mero fixo de colunas).
  gather(MunicOp_Ano, MunicOP_OP, starts_with("pb")) %>% 
  # Renomeia a vari?vel de identifica??o dos munic?pios.
  rename(Munic_Id = codeipea) %>% 
  # Filtra dados que vieram da coluna de total.
  filter(!is.na(Munic_Id)) %>% 
  # Cria a vari?vel de ano, a partir da legenda das vari?veis do banco longo.
  mutate(MunicOp_Ano = as.integer(sub("pb", "", MunicOp_Ano))) %>% 
  # Ordena o banco por munic?pio e ano.
  arrange(Munic_Id, MunicOp_Ano)

# Checa o banco.
names(OP.Data.Select)
head(OP.Data.Select)
View(OP.Data.Select)


# Grava o arquivo  
write.table(OP.Data.Select, file = paste0(OutputFolder, "MunicOp.csv"), 
            sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

# Limpa a mem?ria
rm(OP.Data.Select, OP.Data)
gc()
