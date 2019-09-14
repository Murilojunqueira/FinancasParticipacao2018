# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Create From/for tables to extract accounts from raw financial Files

# Source: Secretaria do Tesouro Nacional (National Treasure Department), Brazil.

# By Murilo Junqueira

# Created at 2019-08-13


################## Altera De/Para Finbra  ##################

# Search for budget variables in BDCamposFinbra

if(!file.exists("data/dataset/BDCamposFinbra.csv")) {
  stop("First creade 'BDCamposFinbra.csv' file.")
}

# From/for tables to extract accounts from raw financial Files
BDCamposFinbra <- fread("data/dataset/BDCamposFinbra.csv", 
                      sep = ";", dec = ",", 
                      stringsAsFactors = FALSE, 
                      encoding = "UTF-8")

# Check fields names
#x <- table(BDCamposFinbra$Campos)
#View(x)

# Summary of total revenue and spending data
"Rec Orçamentária" # 2012-1998
"REC_ORCAM" # 1997
"RECORÇAMENTARIAS" # 1996
"RECORÇAMENTÁRIAS" # ??
"RECORÇAMENTARIA" # 1995-1989
"RECORÇAMENTÁRIA" # ??


"Despesas Orçamentárias" # 2012-1998
"D_ORCAMENT" # 1997
"DESPORÇAMENTÁRIA" # 1996 - 1994
"DESPESAORÇAMENTÁRIA" # 1993 - 1989

Rec <- c("Rec Orçamentária",
         "REC_ORCAM",
         "RECORÇAMENTARIAS",
         "RECORÇAMENTÁRIAS",
         "RECORÇAMENTARIA",
         "RECORÇAMENTÁRIA")

desp <- c("Despesas Orçamentárias",
          "D_ORCAMENT",
          "DESPORÇAMENTÁRIA",
          "DESPESAORÇAMENTÁRIA")



NewRows <- BDCamposFinbra %>% 
  mutate(FinbraCampo_Campo = NA)
  filter(FinbraCampo_Campo == "Despesas Or?ament?rias" |
           FinbraCampo_Campo == "Rec Or?ament?ria" |
           FinbraCampo_Campo == "REC_ORCAM" |
           FinbraCampo_Campo == "D_ORCAMENT" |
           FinbraCampo_Campo == "RECOR?AMENT?RIAS" |
           FinbraCampo_Campo == "RECOR?AMENT?RIA" |
           FinbraCampo_Campo == "DESPOR?AMENT?RIA" |
           FinbraCampo_Campo == "DESPESAOR?AMENT?RIA") %>% 
  mutate(DespRec = ifelse(FinbraCampo_Campo %in% desp, "d", "r")) %>% 
  arrange(DespRec, desc(FinbraCampo_Ano), FinbraCampo_Campo) %>% 
  mutate(ContasPublica_Id = ifelse(DespRec == "r", 71200000000, NA)) %>% 
  mutate(ContasPublica_Id = ifelse(DespRec == "d", 73400000000, ContasPublica_Id)) %>% 
  rename(DeParaFinbra_Ano = FinbraCampo_Ano) %>% 
  select(ContasPublica_Id, FinbraCampo_Id, DeParaFinbra_Ano)


DeParaFinbra <- DeParaFinbra %>% 
  # Evita duplica??p de linhas
  filter(!(ContasPublica_Id %in% NewRows$ContasPublica_Id)) %>% 
  rbind(NewRows)


names(ContasPublicas)

# Novas Contas P?blicas

NewRowContas <- list(ContasPublica_Id = c(71200000000, 73400000000),
                     ContasPublica_RD = c("r", "d"),
                     ContasPublica_Nome = c("RecOrcamentaria", "DespOrcamentaria"),
                     ContasPublica_Descricao = c("Receita Orcamentaria", "Despesa Orcamentaria"),
                     ContasPublica_Categoria = c(NA, NA),
                     ContasPublica_Grupo = c(NA, NA),
                     ContasPublicas_Modalidade = c(NA, NA),
                     ContasPublica_AnoRef = c(NA, NA)
) %>% as.data.frame()


ContasPublicas <- ContasPublicas %>% 
  filter(!(ContasPublica_Id %in% NewRowContas$ContasPublica_Id)) %>% 
  rbind(NewRowContas)


# Grava o arquivo.
write.table(DeParaFinbra, file = paste0(OutputFolder, "DeParaFinbra.csv"), 
            sep = ";", dec = ",", row.names=FALSE, append = FALSE)


# End