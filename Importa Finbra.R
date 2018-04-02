# Script para Importar dados do Finbra no R.

# Esse é um scrip de controle, que manipula as funções que estão
# no arquivo "Importa Finbra Funcoes.R".

# Criado por Murilo Junqueira.

# Data criação: 2018-02-22.
# Ultima modificação: 2018-03-01.


################## Prepara área de trabalho ##################

#clean memory.
rm(list=ls(all=TRUE))
gc()

# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!
InputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/Dados Brutos/FinbraExcel/"
OutputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Dados/BD csv/"
ScriptFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Publicações/2017 - Participação Carla/Scripts R/"

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

################## Extrai Dados Finbra  ##################

# Essa seção visa extrair os dados brutos do Finbra, que já estão inseridos
# em tabelas Excel no diretório InputFolder, para o modelo escolhido 
# para ser a base de dados, ou seja, a tabela "MunicFinancas.csv".

# Cria um banco de dados vazio para agregar os dados de finanças municipais.
MunicFinancas <- tibble()

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
    # Atualmente, somente estamos processando os dados de 1998 em diante,
    # pois antes disso o código de identificação dos municípios é diferente.
    filter(DeParaFinbra_Ano >=1998)
  
  # Cria um banco de dados vazio para agregar os dados da conta que estã sendo processada.
  MunicFinancas.NewVar <- as_tibble()
  
  # Loop para cada linha selecionada na tabela DeParaFinbra (correspondente à conta processada).
  for(j in seq_len(nrow(DeParaFinbra.Select)) ) {
    
    # Linha de debug.
    # j <- 3
    
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
      select(Select.Columns) %>% 
      # Formata os dados (atualmente a função FormataFinbra apenas melhora o formato das
      # colunas de identificação dos municípios). Mais informações em "Importa Finbra Funcoes.R".
      FormataFinbra(Ano = BDCamposFinbra.Select$FinbraCampo_Ano,
                    Aba = BDCamposFinbra.Select$FinbraCampo_AbaXls) %>% 
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
  
  
  # Acrescenta dados no banco de dados agregador final. Nesse momento, as
  # variáveis são empilhadas em colunas, sendo cada linha um município-ano.
  if(nrow(MunicFinancas) == 0) {
    MunicFinancas <- MunicFinancas.NewVar
  } else {
    MunicFinancas <- MunicFinancas %>% 
      full_join(MunicFinancas.NewVar, by = c("Munic_Id", "MunicFinancas_Ano"))
  }
  
  # Libera memória
  rm(j, DeParaFinbra.Select, MunicFinancas.NewVar)
  gc()
}
rm(i)

# Verifica os dados extraídos;
names(MunicFinancas)
View(MunicFinancas)

## Obtem o código das contas financeiras.
ContasPublicas.Select <- ContasPublicas %>% 
  mutate(ContasPublica_Id = as.character(ContasPublica_Id)) %>% 
  select(ContasPublica_Nome, ContasPublica_Id)

# Obtem o total de colunas.
totalColunas <- ncol(MunicFinancas)

# Transforma o painel longo (número indefinido de colunas) em um painel
# curto (número fixo de colunas).
MunicFinancas.Short <- MunicFinancas %>% 
  # Encurta o painel.
  gather(ContasPublica_Nome, MunicFinancas_ContaValor, 3:totalColunas) %>% 
  # Acrecenta o código das contas públicas.
  left_join(ContasPublicas.Select, by = "ContasPublica_Nome") %>% 
  # Cria uma variável de identificação das linhas.
  mutate(MunicFinancas_Id = paste0(Munic_Id, "_", MunicFinancas_Ano, "_", ContasPublica_Id)) %>% 
  # Seleciona as colunas da base de dados final.
  select(MunicFinancas_Id, Munic_Id, MunicFinancas_Ano, ContasPublica_Id, 
         MunicFinancas_ContaValor)

names(MunicFinancas.Short)
head(MunicFinancas.Short)

# Libera memória
rm(totalColunas, ContasPublicas.Select)

# Salvar em arquivo no Banco de Dados

# Caminho do arquivo final
pathFile <- paste0(OutputFolder, "MunicFinancas.csv")

# Grava o arquivo  
write.table(MunicFinancas.Short, file = pathFile, sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

# Libera memória
rm(MunicFinancas.Short, MunicFinancas, pathFile)



################## Encontra Códigos Municipais pré-1998  ##################

# NÃO FINALIZADO!

# Script para importar dados do FINBRA antes de 1998, quando os municípios não eram
# identificados pelo código IBGE, mas pelo código UG.

# Consolidar (empilhar e depois retirar repetições) dos anos de 1997 e 1996 (que é igual 94-95).

FilePath197 <- paste0(InputFolder, "Finbra1997.xlsx")
FilePath196 <- paste0(InputFolder, "Finbra1996.xlsx")

UGs1997 <- read_excel(FilePath197, sheet = "DespesasReceitas")
UGs1996 <- read_excel(FilePath196, sheet = "Plan6")


UGs1997 <- UGs1997 %>% 
  select(UG, NOME, UF) %>% 
  mutate(UG = as.character(UG))

names(UGs1996) <- names(UGs1997)

UGs1996 <- UGs1996 %>% 
  mutate(UG = as.character(UG))


head(UGs1996)
head(UGs1997)
View(UGs1997)


ConsolidaUG <- rbind(UGs1996, UGs1997) %>% 
  distinct(UG, .keep_all = TRUE)

head(ConsolidaUG)
View(ConsolidaUG)

nrow(UGs1996)
nrow(UGs1997)
nrow(ConsolidaUG)


# Formatar os nomes de ambos os grupo
## Deixar todas maiúsculas
## Retirar os acentos
## Retirar as estrofes (Ex: PAU D'AGUA -> PAU D AGUA)
## Retira D isolados (Ex: PAU D AGUA -> PAU DAGUA)

# Usar um sistema de match para encontrar referências exatas nos dois grupos
## left_join.

# Testar alguns métodos de match parcial:

## agrep - http://astrostatistics.psu.edu/su07/R/html/base/html/agrep.html
## Stringdist - https://cran.r-project.org/web/packages/stringdist/




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
