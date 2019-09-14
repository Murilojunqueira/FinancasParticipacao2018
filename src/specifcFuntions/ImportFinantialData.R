# Project: Working paper "Why has the Participatory Budgeting declined in Brazil?"

# This script: Import data about public revenue and spending

# Source: Secretaria do Tesouro Nacional (National Treasure Department), Brazil.

# By Murilo Junqueira

# Created at 2018-02-22


################## Setup WorkSpace ##################

# Functions dependecy
library(RODBC)
library(stringi)
library(xlsx)


################## External Functions ##################

source("src/generalFunctions/CheckPath.R")
source("src/generalFunctions/GetMSAccessData.R")
source("src/generalFunctions/unzipTemp.R")
source("src/generalFunctions/ListToExcel01.R")


################## Generic Function to do what is called ##################



# Lógica da função
# Uma função genérica que chamará uma subfunção certa

# Se estabelece o que quer e a função faz o resto.


# Campos obrigatórios:

# url: link para o arquivo bruto
# FileName: Nome para o arquivo compactado
# DirOutputRaw: diretório para onde vai o baixado
# DirOutputExcel: diretório para onde vai o arquivo Excel.
# DirOutputDF: diretório para onde vão os bancos formatados

# Opções:

# Download_URL: Faz  ou não o Download ou não do Access
# Import_Excel: Importa ou não para o formato Excel ou não
# Extract_Vars: Extrai um conjunto selecionados de campos


# Debug:

# url <- FinantialRawFiles$FinRawFiles_FileURL[1]
# FileName <- FinantialRawFiles$FinRawFiles_FileName[1]
# DirOutputRaw <- "data/raw/Finbra/OriginalFiles"
# DirOutputExcel <- "data/raw/Finbra/ExcelFiles"
# DirOutputDF <- "data/dataset"
# Download_URL <- TRUE
# Import_Excel <- TRUE
# Extract_Vars <- TRUE


ImportFinantialData <- function(url, FileName, DirOutputRaw, DirOutputExcel, DirOutputDF, 
                                Extract_Vars = NA,Download_URL = TRUE, Import_Excel = TRUE) {
  
  DirOutputRaw <- CheckPath(DirOutputRaw)
  DirOutputExcel <- CheckPath(DirOutputExcel)
  DirOutputDF <- CheckPath(DirOutputDF)
  
  
  
  # To do: criar um sistema que detecta o ano do arquivo e o que estamos baixando
  
  
  if(isTRUE(Download_URL)) {
    
    message("Downloading raw file")
    
    # Download
    download.file(url = url, 
                  destfile = paste0(DirOutputRaw, FileName),
                  mode='wb')
  }
  
  
  # Call function based on file year and source
  
  # Debug
  # is89_93 <- TRUE
  
  if(isTRUE(is89_93)) {
    
    if(isTRUE(Import_Excel)) {
      
      # Unzip file
      Rawfile <- unzipTemp(DirOutputRaw, FileName)
      
      # Extract all information from MS Access file
      RawData <- GetMSAccessData(Rawfile, 
                                 TableNameVar = "TABLE_NAME", 
                                 tableType = "TABLE")
      
      # Delete unziped file
      unlink(Rawfile)
      
      # Tables with finantial data (and the table with "UG", old IBGE municipal ID).
      DataTables <- RawData[str_detect(names(RawData), "Quadro")] %>% 
        c(RawData["ugmunicípios"])
      
      # names(DataTables)
      
      # Split data for each year and save in MS Excel File
      for (i in 89:93) {
        # i <- 89
        
        # Filter year data
        DataTablesYear <- DataTables[str_detect(names(DataTables), as.character(i))]
        
        # names(DataTablesYear)
        
        # Add UG code in each table
        for (j in seq_along(DataTablesYear)) {
          
          # Table with differrent name in "Quadro1-89"
          names(DataTablesYear[[j]]) <- sub("REGIÕES E MUNICÍPIOS", "MUNICÍPIOS", names(DataTablesYear[[j]]))
          
          DataTablesYear[[j]] <- DataTablesYear[[j]] %>% 
            left_join(DataTables[["ugmunicípios"]], by = c("MUNICÍPIOS" = "município", "UF" = "uf"))
        }
        
        # names(DataTables[["ugmunicípios"]])
        # View(DataTablesYear[[1]])
        
        # Save xlsx file with this table
        ListToExcel01(ListData = DataTablesYear, 
                      FileNameXlsx = paste0(DirOutputExcel, "Finbra19", i, ".xlsx"))
      }
      
    }
    
    if(!is.na(Extract_Vars)) {
      
      # Soft script:
      
        ## Para cada variável:
          ### Cria uma entrada para a tabela BDCamposFinbra.csv 
          ### Seleciona o correspondente "de/para" das variáveis selecionadas na tabela BDCamposFinbra.csv
          ### Para cada arquivo-ano:
            #### Extrar os dados da coluna correspondente do arquivo-ano
            #### Agregar a variável em um único banco de dados
      
      
      ### Cria uma entrada para a tabela BDCamposFinbra.csv
      
      # List of Excel files
      # Starts with ""Finbra" and ends with "xlsx"
      ExcelFileList <- list.files(DirOutputExcel, pattern = c("^Finbra", "xlsx$")) 
      
      for (i in seq_along(ExcelFileList)) {
        
        # i <- 1
        
        SheetsNames <- loadWorkbook(paste0(DirOutputExcel, ExcelFileList[i])) %>% 
          getSheets()  %>% names()
        
        for (j in seq_along(SheetsNames)) {
          
          # j <- 1
          
          TableNew <- read.xlsx(file = paste0(DirOutputExcel, ExcelFileList[i]),
                                sheetIndex = j, encoding = "UTF-8")
          
          names(TableNew)
          
          
          CamposNew <- list(FinbraCampo_Id = NA,
          FinbraCampo_ArquivoXls = ExcelFileList[i],	
          FinbraCampo_ArquivoAccess	 = FinantialRawFiles$FinRawFiles_FileName[FinantialRawFiles$FinRawFiles_FileURL == url],
          FinbraCampo_AbaXls	 = SheetsNames[j],
          FinbraCampo_Tabela	 = SheetsNames[j],
          FinbraCampo_Campo	 = names(TableNew),
          FinbraCampo_Ano	 = substr(SheetsNames[j], nchar(SheetsNames[j])-1, nchar(SheetsNames[j])),
          FinbraCampo_RefMunic = 0)
          
          
          for (g in seq_along(CamposNew)) {
            # g <- 1
            if(length(CamposNew[[g]]) < length(names(TableNew))) {
              CamposNew[[g]] <- rep(CamposNew[[g]][[1]], length(names(TableNew)))
            }
          }
          
          CamposNew <- as.data.frame(CamposNew)
          
            
          
        }
        
        
        # In each data-year file, extract selected variables
        
        
        
        
      }
      
      
      
      
      
      
      
    }
  }
}


################## MS Excel Functions ##################







################## Sobras ##################

# download.file



# Description of public finantial accounts
ContasPublicas <- fread("data/dataset/ContasPublicas.csv", 
                        sep = ";", dec = ",", stringsAsFactors = FALSE)


# List of Brazilian municipalities
Municipios <- fread("data/dataset/Municipios.csv", 
                    sep = ";", dec = ",", stringsAsFactors = FALSE)


# List of Brazilian states
UFs <- fread("data/dataset/UFs.csv", 
             sep = ";", dec = ",", stringsAsFactors = FALSE)



# From/for tables to extract accounts from raw financial Files
DeParaFinbra <- fread("data/dataset/DeParaFinbra.csv", 
                      sep = ";", dec = ",", stringsAsFactors = FALSE)



# From/For table of "Govermnent Code", id of municipalities until 1997, and IBGE Municipal Code
DeParaUGCodIBGE <- fread("data/dataset/DeParaUGCodIBGE.csv", 
                         sep = ";", dec = ",", stringsAsFactors = FALSE)


################## Extract Finantial Data  ##################

# Create table that show all variables in each annual local govermnent finantial dataset.
if (!file.exists("data/dataset/BDCamposFinbra.csv")) {
  source("src/specifcFuntions/CreateBDCamposFinbra.R")
}





file.exists("src/specifcFuntions/CreateBDCamposFinbra.R")


################## Extrai Dados Finbra  ##################

# Insert the raw data from Finbra in the modeled dataset 


# Empty dataset to gather informations about municipalities
MunicFinancas <- tibble()


AnosDados <- c(2012:1995)


# Loop para cada linha da tabela ContasPublicas:
for(i in seq_len(nrow(ContasPublicas))) {
  
  # Linha de debug:
  # i <- 1
  
  # Exibe a conta que est? sendo processada.
  print(paste0("Formatando vari?vel ", ContasPublicas$ContasPublica_Descricao[i]))
  
  # Filtra apenas a conta p?blica que est? sendo processada da tabela DeParaFinbra.
  DeParaFinbra.Select <- DeParaFinbra %>% 
    # Filtra a conta p?blica correspondente.
    filter(ContasPublica_Id == ContasPublicas$ContasPublica_Id[i]) %>% 
    # Ordena as linhas por ano.
    arrange(desc(DeParaFinbra_Ano)) %>% 
    # Garante que o ano s?o dados inteiros (integer).
    mutate(DeParaFinbra_Ano = as.integer(DeParaFinbra_Ano)) %>% 
    # Filtra os anos de an?lise que ser?o puxados
    filter(DeParaFinbra_Ano %in% AnosDados)
  
  # Cria um banco de dados vazio para agregar os dados da conta que est? sendo processada.
  MunicFinancas.NewVar <- as_tibble()
  
  # Loop para cada linha selecionada na tabela DeParaFinbra (correspondente ? conta processada).
  for(j in seq_len(nrow(DeParaFinbra.Select)) ) {
    
    # Linha de debug.
    # j <- 1
    
    # Exibe o ano que est? sendo processado na tabela DeParaFinbra.
    print(paste0("Encontrando dados do ano ", DeParaFinbra.Select$DeParaFinbra_Ano[j]))
    
    # Transforma a linha do ano na tabela DeParaFinbra em uma lista.
    CampoFinbra.Ref <- DeParaFinbra.Select[j,] %>% 
      unlist %>% as.list()
    
    # Cria uma lista com a localiza??o (Arquivo, aba e coluna) dos dados a serem
    # extraidos do diret?rio OutputFolder.
    BDCamposFinbra.Select <- BDCamposFinbra %>% 
      filter(FinbraCampo_Id == CampoFinbra.Ref$FinbraCampo_Id) %>% 
      unlist %>% as.list()
    
    # Cria um vetor com os nomes dos campos que identificam os munic?pios.
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
    # identifica??o do munic?pio mais colunas de dados).
    Select.Columns <- c(Munic.Select, BDCamposFinbra.Select$FinbraCampo_Campo)
    
    # Formata os dados para que eles ficam no formato desejado.
    MunicFinancas.New <- BD.Fetch %>% 
      # Seleciona as colunas correspondentes
      select(Select.Columns)  %>% 
      # Formata os dados (atualmente a fun??o FormataFinbra apenas melhora o formato das
      # colunas de identifica??o dos munic?pios). Mais informa??es em "Importa Finbra Funcoes.R".
      FormataFinbra(Ano = BDCamposFinbra.Select$FinbraCampo_Ano,
                    Aba = BDCamposFinbra.Select$FinbraCampo_AbaXls,
                    UGtoCodIBGE = DeParaUGCodIBGE, 
                    BDCamposFinbra = BDCamposFinbra,
                    InputFolder = InputFolder) %>% 
      # Muda o formato do c?digo IBGE de 6 d?gitos (antigo) para 7 d?gitos (novo).
      MuncCod6To7("Munic_Id6", "Munic_Id", OutputFolder) %>% 
      # Garante que o ano seja inteiro (integer)
      mutate(MunicFinancas_Ano = as.integer(BDCamposFinbra.Select$FinbraCampo_Ano)) %>%
      # Seleciona os dados finais
      select(Munic_Id, MunicFinancas_Ano, everything())
    
    # Padroniza o nome da vari?vel, de acordo com a tabela ContasPublicas.
    names(MunicFinancas.New)[
      which(names(MunicFinancas.New) == BDCamposFinbra.Select$FinbraCampo_Campo)] <-
      ContasPublicas$ContasPublica_Nome[i]
    
    # Linha de debug:
    # head(MunicFinancas.New)
    
    # Acrescenta os dados processados do ano ? tabela de agrega??o da vari?vel processada.
    MunicFinancas.NewVar <- rbind(MunicFinancas.NewVar, MunicFinancas.New)
    
    # Libera mem?ria
    rm(Select.Columns, MunicFinancas.New)
    rm(BD.Fetch, CampoFinbra.Ref, BDCamposFinbra.Select, Munic.Select)
  }
  
  # Embilha os dados
  MunicFinancas.NewVar.Format <- MunicFinancas.NewVar %>% 
    gather(ContasPublica_Nome, MunicFinancas_ContaValor, 3)
  
  MunicFinancas <- rbind(MunicFinancas, MunicFinancas.NewVar.Format)
  
  # Libera mem?ria
  rm(j, DeParaFinbra.Select, MunicFinancas.NewVar)
  rm(MunicFinancas.NewVar.Format)
  gc()
}
rm(i, AnosDados)

## Obtem o c?digo das contas financeiras.
ContasPublicas.Select <- ContasPublicas %>% 
  mutate(ContasPublica_Id = as.character(ContasPublica_Id)) %>% 
  select(ContasPublica_Nome, ContasPublica_Id)

# Obtem o total de colunas.
totalColunas <- ncol(MunicFinancas)

# Substitui o nome das contas pelos c?digos
MunicFinancas.Short <- MunicFinancas %>% 
  # Acrecenta o c?digo das contas p?blicas.
  left_join(ContasPublicas.Select, by = "ContasPublica_Nome") %>% 
  # Seleciona as colunas da base de dados final.
  select(Munic_Id, MunicFinancas_Ano, ContasPublica_Id, MunicFinancas_ContaValor)

names(MunicFinancas.Short)
head(MunicFinancas.Short)
table(MunicFinancas.Short$ContasPublica_Id)

# Libera mem?ria
rm(ContasPublicas.Select)

# Salvar em arquivo no Banco de Dados

# Caminho do arquivo final
pathFile <- paste0(OutputFolder, "MunicFinancas.csv")

# Grava o arquivo  
write.table(MunicFinancas.Short, file = pathFile, sep = ";", dec = ",", 
            row.names=FALSE, append = FALSE)

# Libera mem?ria
rm(MunicFinancas.Short, MunicFinancas, pathFile)




# Clean memory
rm(BDCamposFinbra)

# End