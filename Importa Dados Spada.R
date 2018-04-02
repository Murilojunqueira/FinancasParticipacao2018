# Script para Importar dados fornecidos por Paolo Spada:
# Censo dos Municípios com Orçamento Participativo.

# Criado por Murilo Junqueira

# Data criação: 2018-03-09
# Ultima modificação: 2018-03-09

# Documentação da base de dados: 
# https://participedia.net/en/content/brazilian-participatory-budgeting-census

################## Prepara área de trabalho ##################

# Limpa memória
rm(list=ls(all=TRUE))
gc()


# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/PBCENSUS Spada/"
OutputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
ScriptFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"

# Instala o pacote readstata13, caso necessário
# install.packages("readstata13")

# Carrega os pacotes que serão usados nesse script.
library(tidyverse)
library(data.table)
library(readstata13)
library(readxl)

################## Importa Dados Sobre Orçamento Participativo ##################

# Importa dados Spada no Stata.
# Preferimos importar do relatório em Excel, que está mais claro.
# OP.Data <- read.dta13(paste0(InputFolder, "PBCENSUS1989_2012data.dta"))

OP.Data <-     read_excel(paste0(InputFolder, "05112017_PB CENSUS 2016.xlsx"), 
                          sheet = "Final census")

# Checa o banco de dados.
names(OP.Data)
View(OP.Data)


# Extrai os dados sobre OP e formata a base de dados
OP.Data.Select <- OP.Data %>% 
  # Seleciona apenas as colunas de indentificação no município e sobre o OP.
  # Os dados de ano estão misturados aos dados sobre OP.
  select(codeipea, starts_with("pb")) %>% 
  # Transforma em base de dados curta (com número fixo de colunas).
  gather(MunicOp_Ano, MunicOP_OP, starts_with("pb")) %>% 
  # Renomeia a variável de identificação dos municípios.
  rename(Munic_Id = codeipea) %>% 
  # Filtra dados que vieram da coluna de total.
  filter(!is.na(Munic_Id)) %>% 
  # Cria a variável de ano, a partir da legenda das variáveis do banco longo.
  mutate(MunicOp_Ano = as.integer(sub("pb", "", MunicOp_Ano))) %>% 
  # Ordena o banco por município e ano.
  arrange(Munic_Id, MunicOp_Ano)

# Checa o banco.
names(OP.Data.Select)
head(OP.Data.Select)
View(OP.Data.Select)


# Grava o arquivo  
write.table(OP.Data.Select, file = paste0(OutputFolder, "MunicOp.csv"), 
            sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

# Limpa a memória
rm(OP.Data.Select, OP.Data)
gc()
