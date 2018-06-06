# Script para Importar dados eleitorais diretamente do Repositório TSE (Tribunal Superior Eleitoral-Brasil)
# Eleição de 1996

# Criado por Murilo Junqueira

# Data criação: 2018-05-25
# Ultima modificação: 2018-05-25

# Site TSE (not working): http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
# Site TSE (2): http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-1996/resultados-das-eleicoes 

################## Prepara área de trabalho ##################

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


################## Import External Data ##################

# Final Data to mimic
CandidatoAno <- fread(paste0(OutputFolder, "CandidatoAno.csv"), 
                      sep = ";", dec = ",",
                      stringsAsFactors = FALSE)

# Finantial Data
UEToCodIBGE <- fread(paste0(OutputFolder, "UEToCodIBGE.csv"), 
                     sep = ";", dec = ",",
                     stringsAsFactors = FALSE)


################## CodeBook ##################

# CodeBook: 

#  (*)  - As variáveis em negrito seguidas de (*) são variáveis que podem ser utilizadas para relacionar os
#         arquivos uns com os outros;
# DATA_GERACAO Data de geração do arquivo (data da extração)
# HORA_GERACAO Hora de geração do arquivo (hora da extração) - Horário de Brasília
# ANO_ELEICAO Ano da eleição
# NUM_TURNO (*) Número do turno
# DESCRICAO_ELEICAO (*) Descrição da eleição
# SIGLA_UF Sigla da Unidade da Federação em que ocorreu a eleição
# SIGLA_UE (*) Sigla da Unidade Eleitoral (Em caso de eleição majoritária é a sigla da UF
#             que o candidato concorre (texto) e em caso de eleição municipal é o
#             código TSE do município (número)). Assume os valores especiais BR, ZZ e
#             VT para designar, respectivamente, o Brasil, Exterior e Voto em Trânsito
# CODIGO_MUNICIPIO (*) Código TSE do município onde ocorreu a eleição
# NOME_MUNICIPIO Nome do município onde ocorreu a eleição
# NUMERO_ZONA (*) Número da Zona Eleitoral
# CODIGO_CARGO (*) Código do cargo a que o candidato concorre
# NUMERO_CAND (*) Número do candidato na urna
# SQ_CANDIDATO (*) Número sequencial do candidato gerado internamente pelos sistemas
#                  eleitorais. Não é o número de campanha do candidato.
# NOME_CANDIDATO Nome completo do candidato
# NOME_URNA_CANDIDATO Nome de urna do candidato
# DESCRICAO_CARGO Descrição do cargo a que o candidato concorre
# COD_SIT_CAND_SUPERIOR Código da situação de totalização do candidato superior naquele turno.
#                       Esta variável deve ser considerada apenas quando o cargo do candidato
#                       for vice ou suplente.
# DESC_SIT_CAND_SUPERIOR Descrição da situação de totalização do candidato superior naquele turno.
#                        Esta variável deve ser considerada apenas quando o cargo do candidato
#                        for vice ou suplente.
# CODIGO_SIT_CANDIDATO Código da situação do registro de candidatura do candidato
# DESC_SIT_CANDIDATO Descrição da situação de registro de candidatura do candidato
# CODIGO_SIT_CAND_TOT Código da situação de totalização do candidato naquele turno
# DESC_SIT_CAND_TOT Descrição da situação de totalização do candidato naquele turno
# NUMERO_PARTIDO Número do partido
# SIGLA_PARTIDO Sigla do partido
# NOME_PARTIDO Nome do partido
# SEQUENCIAL_LEGENDA Código sequencial da legenda gerado pela Justiça Eleitoral
# NOME_COLIGACAO Nome da legenda
# COMPOSICAO_LEGENDA Composição da legenda
# TOTAL_VOTOS Quantidade de votos nominais totalizados para aquele candidato
#             naquele município e zona


NamesVar <- c('DATA_GERACAO',
              'HORA_GERACAO',
              'ANO_ELEICAO',
              'NUM_TURNO',
              'DESCRICAO_ELEICAO',
              'SIGLA_UF',
              'SIGLA_UE',
              'CODIGO_MUNICIPIO',
              'NOME_MUNICIPIO',
              'NUMERO_ZONA',
              'CODIGO_CARGO',
              'NUMERO_CAND',
              'SQ_CANDIDATO',
              'NOME_CANDIDATO', 
              'NOME_URNA_CANDIDATO',
              'DESCRICAO_CARGO',
              'COD_SIT_CAND_SUPERIOR',
              'DESC_SIT_CAND_SUPERIOR',
              'CODIGO_SIT_CANDIDATO',
              'DESC_SIT_CANDIDATO',
              'CODIGO_SIT_CAND_TOT',
              'DESC_SIT_CAND_TOT',
              'NUMERO_PARTIDO',
              'SIGLA_PARTIDO',
              'NOME_PARTIDO',
              "SEQUENCIAL_LEGENDA",
              "NOME_COLIGACAO",
              "COMPOSICAO_LEGENDA",
              "TOTAL_VOTOS")


################## Importa dados eleitorais de 1996 ##################


# urlData <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_1996.zip"

ListFiles <- list.files(paste0(InputFolder, "votacao_candidato_munzona_1996/"), pattern = "*.csv")

# Dados Socioeconômicos
StateVotes <- fread(paste0(InputFolder, "votacao_candidato_munzona_1996/", ListFiles[21]), 
                    sep = ";", dec = ",",
                    stringsAsFactors = FALSE, header = FALSE)

dim(StateVotes)

names(StateVotes) <- NamesVar

names(CandidatoAno)
names(StateVotes)
names(UEToCodIBGE)


Legenda.SitEleitora <- StateVotes %>% 
  group_by(DESCRICAO_CARGO) %>% 
  select(DESCRICAO_CARGO, CODIGO_SIT_CAND_TOT, DESC_SIT_CAND_TOT) %>% 
  distinct(DESCRICAO_CARGO, CODIGO_SIT_CAND_TOT, DESC_SIT_CAND_TOT) %>% 
  arrange(DESCRICAO_CARGO, CODIGO_SIT_CAND_TOT)

Legenda.SitEleitora


StateVotes.Repeat <- table(StateVotes$NOME_MUNICIPIO, StateVotes$ANO_ELEICAO) %>% as.data.frame()

View(StateVotes.Repeat)

StateVotes.Format <- StateVotes %>% 
  mutate(SIGLA_UE = as.numeric(SIGLA_UE)) %>% 
  left_join(UEToCodIBGE, by = "SIGLA_UE") %>% 
  rename(CandAno_Ano = ANO_ELEICAO) %>%
  rename(CandAno_Cargo = DESCRICAO_CARGO) %>% 
  rename(CandAno_Turno = NUM_TURNO) %>% 
  rename(CandAno_Numero = NUMERO_CAND) %>% 
  rename(CandAno_Nome = NOME_CANDIDATO) %>% 
  rename(Partido_Id = SIGLA_PARTIDO) %>% 
  mutate(Coligacao_Id = NA) %>% 
  rename(CandAno_QtVotos = TOTAL_VOTOS) %>% 
  rename(CandAno_SituacaoElec = CODIGO_SIT_CAND_TOT) %>% 
  mutate(Munic_Id = ifelse(NOME_MUNICIPIO == "SAO PAULO", 3550308, Munic_Id))

%>% 
  select(names(CandidatoAno))


  


StateVotes.Format$Munic_Id %>% is.na() %>% which() %>% length()

head(CandidatoAno$CandAno_Ano)

# End