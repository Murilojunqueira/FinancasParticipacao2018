# Script para Importar dados do Finbra no R.

# Esse é um scrip de controle, que manipula as funções que estão
# no arquivo "Importa Finbra Funcoes.R".

# Criado por Murilo Junqueira.

# Data criação: 2018-02-22.
# Ultima modificação: 2018-05-28


################## Prepara área de trabalho ##################

#clean memory.
rm(list=ls(all=TRUE))
gc()

# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/FinbraExcel/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
ScriptFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"

# Checa se os diretórios existem.
dir.exists(c(InputFolder, OutputFolder, ScriptFolder))

# Importa Funções necessárias para a importação de dados.
source(paste0(ScriptFolder, "Importa Finbra Funcoes.R"))


################## Tabelas de Trabalho  ##################

# Nesta seção, importamos partes da base de dados final que serão usadas 
# nesse script.

# Importa tabela BDCamposFinbra (já trabalhada)
## Atenção, esse arquivo é criado pela rotina da seção "Cria BDCamposFinbra", abaixo.
BDCamposFinbra <- fread(paste0(OutputFolder, "BDCamposFinbra.csv"), 
                        sep = ";", dec = ",", stringsAsFactors = FALSE)


# Importa tabela DeParaFinbra
# A criação desses dados é manual, estudando os campos da tabela BDCamposFinbra.csv
DeParaFinbra <- fread(paste0(OutputFolder, "DeParaFinbra.csv"), 
                      sep = ";", dec = ",", stringsAsFactors = FALSE)


# Importa tabela ContasPublicas
ContasPublicas <- fread(paste0(OutputFolder, "ContasPublicas.csv"), 
                        sep = ";", dec = ",", stringsAsFactors = FALSE)


# Relação dos municípios brasileiros, com os respectivos códigos do IBGE.
Municipios <- fread(paste0(OutputFolder, "Municipios.csv"), 
                    sep = ";", dec = ",", stringsAsFactors = FALSE)


# Relação dos estados brasileiros, com os respectivos códigos do IBGE.
UFs <- fread(paste0(OutputFolder, "UFs.csv"), 
                    sep = ";", dec = ",", stringsAsFactors = FALSE)


# De/Para entre código UG (usado no Finbra até 1997) e o Código Municipal IBGE
DeParaUGCodIBGE <- fread(paste0(OutputFolder, "DeParaUGCodIBGE.csv"), 
                         sep = ";", dec = ",", stringsAsFactors = FALSE)

################## Extrai Dados Finbra  ##################

# Essa seção visa extrair os dados brutos do Finbra, que já estão inseridos
# em tabelas Excel no diretório InputFolder, para o modelo escolhido 
# para ser a base de dados, ou seja, a tabela "MunicFinancas.csv".

# Cria um banco de dados vazio para agregar os dados de finanças municipais.
MunicFinancas <- tibble()


AnosDados <- c(2012:1995)


# Loop para cada linha da tabela ContasPublicas:
for(i in seq_len(nrow(ContasPublicas))) {
  
  # Linha de debug:
  # i <- 1
  
  # Exibe a conta que está sendo processada.
  print(paste0("Formatando variável ", ContasPublicas$ContasPublica_Descricao[i]))
  
  # Filtra apenas a conta pública que está sendo processada da tabela DeParaFinbra.
  DeParaFinbra.Select <- DeParaFinbra %>% 
    # Filtra a conta pública correspondente.
    filter(ContasPublica_Id == ContasPublicas$ContasPublica_Id[i]) %>% 
    # Ordena as linhas por ano.
    arrange(desc(DeParaFinbra_Ano)) %>% 
    # Garante que o ano são dados inteiros (integer).
    mutate(DeParaFinbra_Ano = as.integer(DeParaFinbra_Ano)) %>% 
    # Filtra os anos de análise que serão puxados
    filter(DeParaFinbra_Ano %in% AnosDados)
  
  # Cria um banco de dados vazio para agregar os dados da conta que estã sendo processada.
  MunicFinancas.NewVar <- as_tibble()
  
  # Loop para cada linha selecionada na tabela DeParaFinbra (correspondente à conta processada).
  for(j in seq_len(nrow(DeParaFinbra.Select)) ) {
    
    # Linha de debug.
    # j <- 16
    
    # Exibe o ano que está sendo processado na tabela DeParaFinbra.
    print(paste0("Encontrando dados do ano ", DeParaFinbra.Select$DeParaFinbra_Ano[j]))
    
    # Transforma a linha do ano na tabela DeParaFinbra em uma lista.
    CampoFinbra.Ref <- DeParaFinbra.Select[j,] %>% 
      unlist %>% as.list()
    
    # Cria uma lista com a localização (Arquivo, aba e coluna) dos dados a serem
    # extraidos do diretório OutputFolder.
    BDCamposFinbra.Select <- BDCamposFinbra %>% 
      filter(FinbraCampo_Id == CampoFinbra.Ref$FinbraCampo_Id) %>% 
      unlist %>% as.list()
    
    # Cria um vetor com os nomes dos campos que identificam os municípios.
    Munic.Select <- BDCamposFinbra %>% 
      filter(FinbraCampo_Ano == CampoFinbra.Ref$DeParaFinbra_Ano) %>% 
      filter(FinbraCampo_AbaXls == BDCamposFinbra.Select$FinbraCampo_AbaXls) %>% 
      filter(FinbraCampo_RefMunic == 1) %>% 
      select(FinbraCampo_Campo) %>% 
      unlist %>% as.character()
    
    # Mostra o caminho do arquivo com os dados.
    FilePath <- paste0(InputFolder, BDCamposFinbra.Select$FinbraCampo_ArquivoXls)
    # linha de debug:
    # file.exists(FilePath)
    
    # Carrega os dados a partir do Excel correspondente.
    BD.Fetch <- read_excel(FilePath, 
                           sheet = BDCamposFinbra.Select$FinbraCampo_AbaXls)
    
    # Cria um vetor com as colunas que precisam ser selecionadas (colunas de
    # identificação do município mais colunas de dados).
    Select.Columns <- c(Munic.Select, BDCamposFinbra.Select$FinbraCampo_Campo)
    
    # Formata os dados para que eles ficam no formato desejado.
    MunicFinancas.New <- BD.Fetch %>% 
      # Seleciona as colunas correspondentes
      select(Select.Columns)  %>% 
      # Formata os dados (atualmente a função FormataFinbra apenas melhora o formato das
      # colunas de identificação dos municípios). Mais informações em "Importa Finbra Funcoes.R".
      FormataFinbra(Ano = BDCamposFinbra.Select$FinbraCampo_Ano,
                    Aba = BDCamposFinbra.Select$FinbraCampo_AbaXls,
                    UGtoCodIBGE = DeParaUGCodIBGE, 
                    BDCamposFinbra = BDCamposFinbra,
                    InputFolder = InputFolder) %>% 
      # Muda o formato do código IBGE de 6 dígitos (antigo) para 7 dígitos (novo).
      MuncCod6To7("Munic_Id6", "Munic_Id", OutputFolder) %>% 
      # Garante que o ano seja inteiro (integer)
      mutate(MunicFinancas_Ano = as.integer(BDCamposFinbra.Select$FinbraCampo_Ano)) %>%
      # Seleciona os dados finais
      select(Munic_Id, MunicFinancas_Ano, everything())
    
    # Padroniza o nome da variável, de acordo com a tabela ContasPublicas.
    names(MunicFinancas.New)[
      which(names(MunicFinancas.New) == BDCamposFinbra.Select$FinbraCampo_Campo)] <-
      ContasPublicas$ContasPublica_Nome[i]
    
    # Linha de debug:
    # head(MunicFinancas.New)
    
    # Acrescenta os dados processados do ano à tabela de agregação da variável processada.
    MunicFinancas.NewVar <- rbind(MunicFinancas.NewVar, MunicFinancas.New)
    
    # Libera memória
    rm(Select.Columns, MunicFinancas.New)
    rm(BD.Fetch, CampoFinbra.Ref, BDCamposFinbra.Select, Munic.Select)
  }
  
  # Embilha os dados
  MunicFinancas.NewVar.Format <- MunicFinancas.NewVar %>% 
    gather(ContasPublica_Nome, MunicFinancas_ContaValor, 3)
  
  MunicFinancas <- rbind(MunicFinancas, MunicFinancas.NewVar.Format)
  
  # Libera memória
  rm(j, DeParaFinbra.Select, MunicFinancas.NewVar)
  rm(MunicFinancas.NewVar.Format)
  gc()
}
rm(i, AnosDados)

# Verifica os dados extraídos;
 # names(MunicFinancas)
 # dim(MunicFinancas)
 # table(MunicFinancas$MunicFinancas_Ano)
 # head(MunicFinancas, n = 10)
 # tail(MunicFinancas, n = 10)
# View(MunicFinancas)

# Verifica repetições no banco.
#  x <- table(MunicFinancas$Munic_Id, MunicFinancas$MunicFinancas_Ano) %>% as.data.frame()
#  x[x$Freq > 8,]
#  View(x)
#  table(x$Freq)
# 
#  test <-  MunicFinancas[MunicFinancas$Munic_Id %in% x$Var1[x$Freq > 8],] %>% arrange(Munic_Id, MunicFinancas_Ano)
# View(test)
# rm(test, x)
 
## Obtem o código das contas financeiras.
ContasPublicas.Select <- ContasPublicas %>% 
  mutate(ContasPublica_Id = as.character(ContasPublica_Id)) %>% 
  select(ContasPublica_Nome, ContasPublica_Id)

# Obtem o total de colunas.
totalColunas <- ncol(MunicFinancas)

# Substitui o nome das contas pelos códigos
MunicFinancas.Short <- MunicFinancas %>% 
  # Acrecenta o código das contas públicas.
  left_join(ContasPublicas.Select, by = "ContasPublica_Nome") %>% 
  # Seleciona as colunas da base de dados final.
  select(Munic_Id, MunicFinancas_Ano, ContasPublica_Id, MunicFinancas_ContaValor)

names(MunicFinancas.Short)
head(MunicFinancas.Short)

# Libera memória
rm(ContasPublicas.Select)

# Salvar em arquivo no Banco de Dados

# Caminho do arquivo final
pathFile <- paste0(OutputFolder, "MunicFinancas.csv")

# Grava o arquivo  
write.table(MunicFinancas.Short, file = pathFile, sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

# Libera memória
rm(MunicFinancas.Short, MunicFinancas, pathFile)


################## Encontra Códigos Municipais pré-1998  ##################

# Script para importar dados do FINBRA antes de 1998, quando os municípios não eram
# identificados pelo código IBGE, mas pelo código UG.

# Consolidar (empilhar e depois retirar repetições) dos anos de 1997 e 1996 
# (em 94-95 nem UG tem, é só nomes dos Municípios).

## Caminho dos arquivos
FilePath197 <- paste0(InputFolder, "Finbra1997.xlsx")
FilePath196 <- paste0(InputFolder, "Finbra1996.xlsx")

## Carrega arquivos
UGs1997 <- read_excel(FilePath197, sheet = "DespesasReceitas")
UGs1996 <- read_excel(FilePath196, sheet = "Plan6")

# Libera memória
rm(FilePath196, FilePath197)

UGs1997 <- UGs1997 %>% 
  # Seleciona variáveis relevantes
  select(UG, NOME, UF) %>% 
  # Determina que as variáveis UG serão de tipo texto.
  mutate(UG = as.character(UG)) %>% 
  mutate(Origem = 1997)

UGs1996 <- UGs1996 %>% 
  # Determina que as variáveis UG serão de tipo texto.
  mutate(UG = as.character(UG)) %>% 
  mutate(Origem = 1996)

# Uniformiza os nomes de variáveis entre os dois arquivos
names(UGs1996) <- names(UGs1997)

# Cria um arquivo único com as UGs de 1997 e 1997
ConsolidaUG <- rbind(UGs1996, UGs1997) %>% 
  arrange(desc(Origem)) %>% 
  # Remove repetições
  distinct(UG, .keep_all = TRUE) %>%
  # Renomeia algumas variáveis, para deixa-las no padrão do banco.
  rename(Munic_Nome = NOME) %>% 
  rename(UF_Sigla = UF) %>% 
  # Retira os casos dos municípios sem nome.
  ## Como o nome é a ponte entre o código IBGE e o UG, não podemos trabalhar sem ele.
  filter(!is.na(Munic_Nome)) %>% 
  # Trata as variáveis de nome, para deixa-las mais uniformes.
  ## Remove espaços e caracteres especiais.
  mutate(Munic_Nome = trimws(Munic_Nome)) %>% 
  ## Remove apostroves (ex: Pau D'Agua -> Pau DAgua)
  mutate(Munic_Nome = gsub("'", "", Munic_Nome)) %>% 
  mutate(Munic_Nome = gsub("`", "", Munic_Nome)) %>% 
  ## Alterações de nomes que, após pesquisa, descobrimos as verdadeiras reverências no IBGE.
  ## A maioria dos casos se referem a municípios que realmente mudaram de nome no período.
  mutate(Munic_Nome = sub("ALTO JEQUITIBA (PRESIDENTE SOARES)", "ALTO JEQUITIBA", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("CACHOEIRA DO PAJEU (EX-ANDRE FERNANDES)", "CACHOEIRA DO PAJEU", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("MATHIAS LOBATO (VILA MATIAS)", "MATHIAS LOBATO", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("NOVO HORIZONTE DOESTE (EX-CACAIEIROS)", "NOVO HORIZONTE DOESTE", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("SAO GONCALO DO RIO PRETO (EX-FELISB.CALDEIRA)", "SAO GONCALO DO RIO PRETO", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("SERRA CAIADA (EX-PRESIDENTE JUSCELINO)", "SERRA CAIADA", Munic_Nome, fixed = TRUE)) %>%
  mutate(Munic_Nome = ifelse(UF_Sigla == "RN" & Munic_Nome == "PRESIDENTE JUSCELINO", "SERRA CAIADA", Munic_Nome)) %>%
  mutate(Munic_Nome = sub("SERRA DO NAVIO (EX-AGUA BRANCA DO AMAPARI)", "SERRA DO NAVIO", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("MOSQUITO (PALMEIRAS DO TO)", "PALMEIRAS DO TOCANTINS", Munic_Nome, fixed = TRUE)) %>% 
  mutate(Munic_Nome = sub("VILA NOVA DO MAMORE", "NOVA MAMORE", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("VILA ALTA", "ALTO PARAISO", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("VARRE E SAI", "VARRE-SAI", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("SITIO DOS MOREIRAS", "MOREILANDIA", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("SAO MIGUEL DE TOUROS", "SAO MIGUEL DO GOSTOSO", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("SAO DOMIMGOS DE POMBAL", "SAO DOMIMGOS", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("SAO BENTO DE POMBAL", "SAO BENTO", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("MOJI-GUACU", "MOGI-GUACU", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("ITABIRINHA DE MANTENA", "ITABIRINHA", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("IPAUCU", "IPAUSSU", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("BRODOSQUI", "BRODOWSKI", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("BROCHIER DO MARATA", "BROCHIER", Munic_Nome)) %>% 
  mutate(Munic_Nome = sub("COUTO DE MAGALHAES", "COUTO MAGALHAES", Munic_Nome)) %>% 
  # Evita nomes repetidos (casos onde o UG mudou, mas o Nome manteve, dentro do mesmo Estado)
  distinct(Munic_Nome, UF_Sigla, .keep_all = TRUE) %>%
  select(UG, Munic_Nome, UF_Sigla)

# Checa se não há repetições de nomes dentro de um mesmo Estado
# x <- table(ConsolidaUG$Munic_Nome, ConsolidaUG$UF_Sigla) %>% as.data.frame()
# x[x$Freq > 1,]
# View(x)
# ConsolidaUG[ConsolidaUG$Munic_Nome %in% x$Var1[x$Freq > 1],] %>% arrange(Munic_Nome)

# Libera memória
rm(UGs1996, UGs1997)

# Formata os nomes da lista do IBGE, para deixa-los mais comparáveis.
ConsolidaMunic <- Municipios %>% 
  # Determina que os códigos são textos.
  mutate(Munic_Id = as.character(Munic_Id)) %>% 
  # Garante que todos serão MAIÚSCULAS.
  mutate(Munic_Nome = toupper(Munic_Nome)) %>% 
  # Remove os acentos.
  mutate(Munic_Nome = iconv(Munic_Nome, to = "ASCII//TRANSLIT")) %>% 
  ## Remove apostroves (ex: Pau D'Agua -> Pau DAgua)
  mutate(Munic_Nome = gsub("'", "", Munic_Nome)) %>% 
  mutate(Munic_Nome = gsub("`", "", Munic_Nome)) %>%
  # Adiciona o banco dos Estados (necessário para rotinas abaixo)
  left_join(UFs, by = "UF_Id") %>% 
  # Remove "D"s isolados (ex: PAU D AGUA - > PAU DAGUA)
  mutate(Munic_Nome = gsub(" D ", " D", Munic_Nome)) %>% 
  # Adiciona um coluna com os números das linhas
  mutate(n = row_number())
  
# Join os bancos da lista de UGs com a lista de municípios IBGE, pelo nome dos municípios.
JoinUG <- ConsolidaUG %>% 
  left_join(ConsolidaMunic, by = c("Munic_Nome", "UF_Sigla"))

# Verifica o banco resultante
#View(JoinUG)
#names(JoinUG)

# Cria um banco onde há apenas os casos onde não há correspondência perfeita 
# entre os nomes de cidades.
PartialMatching <- JoinUG %>% 
  filter(is.na(Munic_Id)) %>% 
  select(UG, Munic_Nome, UF_Sigla)

# Banco apenas com os municípios onde não tive match exato.

# Uiliza a função MatchCity (acima), para encontrar as cidades sem correspondência perfeita
# A busca é feita dentro de cada estado.
PartialMatching$Partial.Ref <- pmap_int(.l = list(CityName = PartialMatching$Munic_Nome, 
                                                  Region = PartialMatching$UF_Sigla),
                                        .f = MatchCity,
                                        CompareData = ConsolidaMunic, 
                                        CityNameVar = "Munic_Nome", 
                                        RegionVarName = "UF_Sigla")

# Verifica os casos onde mesmo o matching parcial não encontrou correspondência
PartialMatching %>% 
  filter(is.na(Partial.Ref)) %>% 
  select(Munic_Nome, UF_Sigla)

# São Apenas dois casos.

# Adiciona o código IBGE dos municípios com match parcial.
PartialMatchingJoin  <-  PartialMatching %>% 
  left_join(select(ConsolidaMunic, n, Munic_Id, UF_Id), by = c("Partial.Ref" = "n")) %>% 
  # filter(!is.na(Partial.Ref)) %>% 
  # Adicina uma variável indicadora de que é correspondência parcial
  mutate(Origem = "Partial") %>% 
  select(Munic_Id, UG, Munic_Nome, UF_Sigla, Origem)
  
# Cria um banco único com o match exato e o match parcial empilhados
DeParaUGIBGE <- JoinUG %>% 
  select(Munic_Id, UG, Munic_Nome, UF_Sigla) %>% 
  filter(!is.na(Munic_Id)) %>% 
  # Adicina uma variável indicadora de que é correspondência exata
  mutate(Origem = "Exact") %>% 
  rbind(PartialMatchingJoin) %>% 
  arrange(Munic_Id, Origem) %>% 
  distinct(Munic_Id, .keep_all = TRUE)
  

# Checa repetições do banco:
# Check <- table(DeParaUGIBGE$Munic_Id) %>% as.data.frame()
# View(Check)
# Check[Check$Freq > 1,]

# Repeats <- Check$Var1[Check$Freq > 1] %>% as.character()
# x <- DeParaUGIBGE[which(DeParaUGIBGE$Munic_Id %in% Repeats),] %>% arrange(Munic_Id)
# View(x)
# x

DeParaUGIBGE <- DeParaUGIBGE %>% 
  select(-Origem)

# Checa o banco criado
#names(DeParaUGIBGE)
#dim(DeParaUGIBGE)
#View(DeParaUGIBGE)

# Caminho do arquivo contendo todos os dados de BDCamposFinbra.
OutputFile <- paste0(OutputFolder, "DeParaUGCodIBGE.csv")

# Grava o arquivo.
write.table(DeParaUGIBGE, file = OutputFile, sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)


# Libera memória
rm(ConsolidaMunic, ConsolidaUG)
rm(PartialMatching, PartialMatchingJoin)
rm(MatchCity, JoinUG)
rm(DeParaUGIBGE, OutputFile)



### Problemas para ver no futuro

# Modelar casos onde existem dois UGs para um mesmo município (comparando 1996 e 1997)
# Como lidar com municípios que mudaram de nome entre 1996 e 1997, tendo o mesmo UG e Cod IBGE?


################## Cria BDCamposFinbra  ##################

# Script para analisar todos os arquivos do diretório InputFolder e listar todos
# os arquivos Excel, abas e colunas, agrevando tudo na tabela BDCamposFinbra.csv.

# Lista os arquivos do diretório InputFolder que começa com "Finbra".
FileList <- list.files(InputFolder, pattern = "^Finbra*")

# Cria um banco de dados agregador das informações.
BDCamposFinbra <- data.frame()

# Lopp para cada arquivo encontrado em FileList.
for(i in seq_along(FileList)) {
  
  # Linha de debug.
  # i <- 20
  
  # Exibe o arquivo que está sendo processado.
  print(paste("Lendo o arquivo", FileList[i]))
  
  # Cria uma lista vazia para agregar as informações de uma linha da tabela BDCamposFinbra.
  NewRow <- list()
  
  # Insere na lista o nome do arquivo.
  NewRow$FileName <- FileList[i]
  
  # Caminho completo do arquivo.
  FullPath <- paste0(InputFolder, FileList[i])
  
  # Cria um vetor de texto com o nome das Abas do arquivo.
  sheets <- excel_sheets(FullPath)
  
  # Loop para cada Aba do arquivo que está sendo processado.
  for(j in seq_along(sheets)) {
    
    # Linha de debug.
    # j <- 2
    
    # Exibe a Aba que está sendo processada.
    print(paste("Aba", sheets[[j]]))
    
    # Insere na lista agregadora o nome da aba que está sendo processada.
    NewRow$Sheet <- sheets[[j]]
    
    # Insere na lista agregadora o nome das colunas na Aba que está sendo processada.
    NewRow$Campos <- names(read_excel(FullPath, sheet = sheets[[j]]))
    
    # Transforma os dados coletados (Nome do arquivo, da Aba e das colunas)
    # em um banco de dados.
    NewRows <- as.data.frame(NewRow$Campos) %>% 
      mutate(FileName = NewRow$FileName) %>% 
      mutate(Aba = NewRow$Sheet) %>% 
      select(FileName, Aba, everything())
    
    # Acerta o nome da coluna "Campos" na tabela acima.
    names(NewRows)[ncol(NewRows)] <- "Campos"
    
    # Agrega os dados coletados na tabela agregadora.
    BDCamposFinbra <- rbind(BDCamposFinbra, NewRows)
  }
}

# Libera memória.
rm(i, j, FullPath)
rm(sheets, NewRow, NewRows)

# Caminho do arquivo contendo todos os dados de BDCamposFinbra.
OutputFile <- paste0(InputFolder, "BDCamposFinbra.csv")

# Grava o arquivo.
write.csv2(BDCamposFinbra, file = OutputFile, 
           sep = ";", dec = ",")

# Libera memória.
rm(FileList, BDCamposFinbra)
