# Script para Importar dados eleitorais diretamente do Repositório TSE (Tribunal Superior Eleitoral-Brasil)
# Eleição de 1996

# Criado por Murilo Junqueira

# Data criação: 2018-05-25
# Ultima modificação: 2018-05-25

# Site TSE (not working): http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
# Site TSE (2): http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-1996/resultados-das-eleicoes 
# Site TSE (3) (more convenient): http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-1996/divulgacao-candidatos-1996 

################## Setup Working Space ##################

# Limpa memória
rm(list=ls(all=TRUE))
gc()

# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/TSE/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"


# Como o API Cepesp grava informações no computador para agilizar repedições de extrações,
# é importante selecionar o InputFolder como o diretório de trabalho. Certifique que esse 
# diretório é o adequado para receber informações novas (um diretório dedicado no Dropbox, por
# exemplo).
setwd(InputFolder)

# Verifica se o diretório de trabalho está correto.
getwd()

# Carrega os pacotes que serão usados nesse script.
library(tidyverse)
library(data.table)
library(readxl)


################## Import External Data ##################

# Basic Municipal Data
Municipios <- fread(paste0(OutputFolder, "Municipios.csv"), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE)

# Basic State Data
UFs <- fread(paste0(OutputFolder, "UFs.csv"), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE)

# Final Data to mimic
CandidatoAno <- fread(paste0(OutputFolder, "CandidatoAno.csv"), 
                      sep = ";", dec = ",",
                      stringsAsFactors = FALSE)

# Electoral zone code
UEToCodIBGE <- fread(paste0(OutputFolder, "UEToCodIBGE.csv"), 
                     sep = ";", dec = ",",
                     stringsAsFactors = FALSE)


################## Import 1996 Election Data ##################

# Carrega os dados a partir do Excel correspondente.
CandidateVotes <- read_excel(paste0(InputFolder, "Candidatos_1996.xlsx"), 
                             sheet = 1, col_types = rep("text", 22))


# Check the data
dim(CandidateVotes)
names(CandidateVotes)
head(CandidateVotes)
# View(CandidateVotes)

# Transform the first dataset in the second one.
names(CandidateVotes)
names(CandidatoAno)

# Format and rename the dataframe
CandidatoAno.1996 <- CandidateVotes %>% 
  # Transform electoral city code in IBGE's city code
  mutate(COD_MUN = as.numeric(COD_MUN)) %>% 
  left_join(UEToCodIBGE, by = c("COD_MUN" = "SIGLA_UE")) %>% 
  # Linhas debug:
  # filter(Munic_Id == 3550308) %>% 
  # filter(CARGO == "Prefeito") %>% 
  # Add election's year
  mutate(CandAno_Ano = 1996) %>% 
  # Transform and Rename variables
  rename(CandAno_Cargo = CARGO) %>%
  mutate(CandAno_Cargo = as.character(CandAno_Cargo)) %>% 
  mutate(CandAno_Cargo = toupper(CandAno_Cargo)) %>% 
  mutate(CandAno_Cargo = substr(CandAno_Cargo, 1, 1)) %>% 
  mutate(CandAno_Turno = ifelse(is.na(SITUACAO2T), 1, 2)) %>% 
  rename(CandAno_Numero = NUMERO) %>%
  rename(CandAno_Nome = NOME) %>%
  rename(Partido_Sigla = SGL_PARTIDO) %>%
  # We don't have coligation data in this dataframe
  mutate(Coligacao_Id = NA) %>%
  rename(CandAno_QtVotos = QTD_VOTOS) %>%
  mutate(CandAno_SituacaoElec = ifelse(is.na(SITUACAO2T), SITUACAO1T, SITUACAO2T)) %>% 
  filter(!is.na(CandAno_SituacaoElec)) %>% 
  # Transmor candidate's situation description in candidate's situation code (2012 format)
  mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Eleito", "1", CandAno_SituacaoElec)) %>%
  mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Eleito por Média", "5", CandAno_SituacaoElec)) %>%
  mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Não eleito", "4", CandAno_SituacaoElec)) %>% 
  mutate(CandAno_SituacaoElec = ifelse(CandAno_SituacaoElec == "Suplente", "2", CandAno_SituacaoElec)) %>% 
  # Select previous dataframe variable. Select the right order.
  select(names(CandidatoAno))


# Adiciona dados do segundo turno
  ## O banco anterior mostra a quantidade total de votos de cada candidato, somando primeiro e segundo turno (?)

# Lê outro banco de dados, onde aparece apenas os votos do primeiro turno
# Carrega os dados a partir do Excel correspondente.
PrefVotes <- fread(paste0(InputFolder, "Resultado_da_Eleição_(Por_Municipio)_1996 (Prefeito).csv"),
                   skip = 6, header = FALSE) %>% select(-V11)

names(PrefVotes) <- c("CandAno_Cargo",
                      "UF_Sigla",
                      "Munic_Nome",
                      "Partido_Sigla",
                      "CandAno_Numero",
                      "CandAno_Nome",
                      "CandAno_QtVotos",
                      "CandAno_SituacaoElecDesc",
                      "PerValidos",
                      "Coligacao_Id")

# Seleciona candidatos que foram ao segundo turno
CandidatoAno.1996.2t <- CandidatoAno.1996 %>% 
  filter(CandAno_Turno == 2) %>% 
  # Agrega nomes dos municípios
  left_join(Municipios, by = "Munic_Id") %>% 
  # Agrega informações dos estados (sigla)
  left_join(UFs, by = "UF_Id") %>% 
  select(names(CandidatoAno.1996), Munic_Nome, UF_Sigla) 

# Seleciona cidades que foram ao segundo turno no banco acima.
Cidades.Pref <- PrefVotes %>% 
  filter(CandAno_SituacaoElecDesc == "2º turno") %>% 
  # Remove duplicatas das cidades
  distinct(UF_Sigla, Munic_Nome) %>% 
  # Procedimentos para dar uniformidade aos nomes de cidades (para join)
  # Garante que todos serão MAIÚSCULAS.
  mutate(Munic_Nome = toupper(Munic_Nome)) %>% 
  # Remove os acentos.
  mutate(Munic_Nome = iconv(Munic_Nome, to = "ASCII//TRANSLIT"))

# Seleciona nomes das cidades com segundo turno  banco principal
Cidades.Original <- CandidatoAno.1996.2t %>% 
  # Remove duplicatas das cidades
  distinct(Munic_Id, Munic_Nome) %>% 
  # Resolve o caso de Jaboatão, único a dar problema.
  mutate(Munic_Nome = ifelse(Munic_Nome == "JABOATÃO DOS GUARARAPES", "JABOATAO", Munic_Nome)) %>% 
  # Garante que todos serão MAIÚSCULAS.
  # Procedimentos para dar uniformidade aos nomes de cidades (para join)
  mutate(Munic_Nome = toupper(Munic_Nome)) %>% 
  # Remove os acentos.
  mutate(Munic_Nome = iconv(Munic_Nome, to = "ASCII//TRANSLIT")) %>%
  # Faz o Join entre os nomes das cidades nos dois bancos.
  left_join(Cidades.Pref, by = "Munic_Nome")

# View(Cidades.Original)

# Adiciona o código municipal (Munic_Id) no banco com o primeiro turno
PrefVotes.2t <- PrefVotes %>% 
  filter(CandAno_SituacaoElecDesc == "2º turno") %>% 
  # Transforma a quantidade de votos em integer
  mutate(CandAno_QtVotos = gsub(",", "", CandAno_QtVotos)) %>% 
  mutate(CandAno_QtVotos = as.integer(CandAno_QtVotos)) %>% 
  # Corta algumas variaveis não usadas
  select(1:8) %>% 
  # Agrega o nome municipal.
  left_join(Cidades.Original, by = c("Munic_Nome", "UF_Sigla"), suffix = c("", ".y"))

# Libera memória
rm(Cidades.Pref, Cidades.Original, PrefVotes)

names(CandidatoAno.1996.2t)
names(PrefVotes.2t)

# Cria a variável de votação em primeiro turno
CandidatoAno.1996.2t <-  CandidatoAno.1996.2t %>% 
  mutate(Votes1T = NA)

# Adiciona no banco CandidatoAno.1996.2t os nomes dos candidatos usando
# matching parcial.
for(i in seq_len(nrow(CandidatoAno.1996.2t))){
  
  # Debug lines
  # i <- 2

  # Seleciona os candidatos do município
  EleicaoAno <- PrefVotes.2t %>% 
    select(Munic_Id, Munic_Nome, CandAno_Nome, CandAno_QtVotos) %>% 
    filter(Munic_Id == CandidatoAno.1996.2t$Munic_Id[i])
  
  # Número da linha que corresponde ao votos do candidato
  num <- agrep(CandidatoAno.1996.2t$CandAno_Nome[i], EleicaoAno$CandAno_Nome)
  
  # Adiciona a votação em primeiro turno
  CandidatoAno.1996.2t$Votes1T[i] <- EleicaoAno$CandAno_QtVotos[num]
  
  # Libera memória
  rm(num, EleicaoAno)
}

# Libera memória
rm(PrefVotes.2t, i)

# Checa dados
# View(CandidatoAno.1996.2t)

# Formata o banco
CandidatoAno.1996.2t.Format <- CandidatoAno.1996.2t %>% 
  # Garante que as variáveis sejam inteiros
  mutate(CandAno_QtVotos = as.integer(CandAno_QtVotos)) %>% 
  mutate(Votes1T = as.integer(Votes1T)) %>%
  # Calcula a variável de votos em segundo turno
  mutate(Votes2T = CandAno_QtVotos - Votes1T) %>% 
  # Transforma cada candidato em duas linhas, correspondentes a 
  # primeiro e segundo turno.
  select(-CandAno_QtVotos, -CandAno_SituacaoElec, -CandAno_Turno) %>% 
  gather(CandAno_Turno, CandAno_QtVotos, Votes1T:Votes2T) %>% 
  # Coloca corretamente o turno da votação
  mutate(CandAno_Turno = ifelse(CandAno_Turno == "Votes1T", 1, CandAno_Turno)) %>% 
  mutate(CandAno_Turno = ifelse(CandAno_Turno == "Votes2T", 2, CandAno_Turno)) %>% 
  mutate(CandAno_Turno = as.integer(CandAno_Turno)) %>% 
  # Coloca a situação eleitoral
  # Situação eleitoral do primeiro turno (6 = passou para o 2º TURNO)
  mutate(CandAno_SituacaoElec = ifelse(CandAno_Turno == 1, 6, NA)) %>% 
  # Situação eleitoral no segundo turno
  group_by(Munic_Id, CandAno_Ano) %>% 
  # 1 = eleito, 4 = não eleito.
  mutate(CandAno_SituacaoElec = ifelse(CandAno_Turno == 2 & CandAno_QtVotos == max(CandAno_QtVotos), 
                                       1, CandAno_SituacaoElec)) %>% 
  mutate(CandAno_SituacaoElec = ifelse(CandAno_Turno == 2 & CandAno_QtVotos != max(CandAno_QtVotos), 
                                       4, CandAno_SituacaoElec)) %>% 
  ungroup() %>% 
  # Corrige a ordem das variáveis
  select(names(CandidatoAno)) %>% 
  arrange(Munic_Id)

# name (CandidatoAno.1996.2t.Format)
# View(CandidatoAno.1996.2t.Format)

rm(CandidatoAno.1996.2t)


CandidatoAno.1996.final <- CandidatoAno.1996 %>% 
  filter(CandAno_Turno != 2) %>% 
  rbind(CandidatoAno.1996.2t.Format) %>% 
  # Debug line:
  # filter(Munic_Id == 3550308) %>% 
  mutate(CandAno_QtVotos = as.integer(CandAno_QtVotos)) %>% 
  arrange(Munic_Id, CandAno_Ano, CandAno_Cargo, CandAno_Turno, 
          desc(CandAno_QtVotos)) 


# Checa os dados
# View(CandidatoAno.1996.final)

# Repare que os candidatos do PRONA (número 56), não tem a quantidade de votos.
# View(CandidatoAno.1996.final[is.na(CandidatoAno.1996.final$CandAno_QtVotos),])

# Check the data

# names(CandidatoAno.1996.final)
# View(CandidatoAno.1996.final)
# 
# CandidatoAno.1996.final$Munic_Id %>% is.na() %>% which() %>% length()
# CandidatoAno.1996.final$CandAno_SituacaoElec %>% is.na() %>% which() %>% length()
# 
# table(CandidatoAno.1996.final$CandAno_SituacaoElec)
# 
# table(CandidatoAno.1996.final$CandAno_Cargo)




# Evita linhas duplicadas
CandidatoAno <- CandidatoAno %>% 
  filter(CandAno_Ano != 1996)

# Bind old and new data
BindData <- rbind(CandidatoAno, CandidatoAno.1996.final)

# Check data
table(BindData$CandAno_Ano)

# Save data file
# Files's path
pathFile <- paste0(OutputFolder, "CandidatoAno.csv")

# Weite the file
write.table(BindData, file = pathFile, sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

rm(BindData, CandidatoAno.1996.2t.Format, CandidatoAno.1996.final, CandidatoAno)
rm(CandidateVotes, Municipios, UEToCodIBGE, UFs)
rm(InputFolder, OutputFolder, pathFile,  ScriptFolder)
rm(CandidatoAno.1996)
gc()

# End