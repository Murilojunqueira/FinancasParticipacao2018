# Script para Importar dados do Finbra no R.

# Script Para guardar as funções de extração dos dados do Finbra.

# Criado por Murilo Junqueira.

# Data criação: 2018-02-27.


################## Carrega pacotes necessários ##################

# Lista de pacotes necessários para as funções desse arquivo.
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "dplyr", 
                      "tidyr", 
                      "stringr",
                      "lubridate",
                      "readxl")

# Verifica os que não estão instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Instala os pacotes não instalados
if(length(new.packages)) install.packages(new.packages)

# Lê todos os pacotes
for(i in 1:length(list.of.packages)) {
  #print(paste("Lendo o pacote", list.of.packages[i]))
  library(list.of.packages[i], character.only = TRUE)  
}

# Libera memória
rm(list.of.packages, new.packages, i)



################## Funções ##################

# Funções para formatações básicas dos dados do Finbra para cada ano.
FormataFinbra <- function(x, Ano, Aba, UGtoCodIBGE = NULL, BDCamposFinbra = NULL, 
                          InputFolder = NULL){
  
  # Linhas de debug.
  # x <- MunicFinancas.New
  # Ano <- BDCamposFinbra.Select$FinbraCampo_Ano
  
  # Garante que o Ano é uma variável inteira (integer).
  Ano <- as.integer(Ano)
  
  # Atribui a função correta para o dado  
  ## 1998 a 2012 para a função "Fun2012_1998"
  if(Ano <= 2012 & Ano >= 1998) {
    FUN <-  "Fun2012_1998" 
    } else if(Ano == 1997) {
      ## 1997 para a função "Fun1997"
      FUN <-  "Fun1997"
    } else if(Ano <= 1996 & Ano >= 1992) {
      ## 1996 a 1994 para a função "Fun1996_1994"
      FUN <-  "Fun1996_1994"
    }
  
  # Caso o ano indicado não corresponder a nenhuma função determinada, retorna erro.
  if(is.null(FUN)) {
    stop("Deve-se indicar um valor de ano com uma função correspondente")
  }
  
  # função para formatar os dados de 1998 a 2012.
  Fun2012_1998 <- function(x){
    
    Output <- x %>% 
      # Cria uma variável uniformizada de indentificação dos municípios
      mutate(Munic_Id6 = paste0(CD_UF, str_pad(CD_MUN, 4, "left", "0"))) %>% 
      # Remove campos anteriores de indentificação dos municípios.
      select(-CD_UF, -CD_MUN) %>% 
      # Deixa a variável de indentificação com a primeira coluna da tabela.
      select(Munic_Id6, everything())
  }
  
  # função para formatar os dados de 1998 a 2012.
  Fun1997 <- function(x){
    
    UGtoCodIBGE.ref6 <- DeParaUGCodIBGE %>% 
      mutate(Munic_Id6 = substr(Munic_Id, 1, 6)) %>% 
      mutate(UG = as.character(UG))
    
    Output <- x %>% 
      mutate(UG = as.character(UG)) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG")  %>% 
      # Remove campos anteriores de indentificação dos municípios.
      select(-NOME, -UF, -Munic_Id, -UG, -Munic_Nome, -UF_Sigla) %>% 
      # Deixa a variável de indentificação com a primeira coluna da tabela.
      select(Munic_Id6, everything())
  }
  
  
  # função para formatar os dados de 1998 a 2012.
  Fun1996_1994 <- function(x){
    
    # Carrega a aba onde estão os dados de UG
    Ref.Munic <- BDCamposFinbra %>% 
      filter(FinbraCampo_Ano == Ano) %>% 
      filter(FinbraCampo_Campo == "UG") %>% 
      select(FinbraCampo_ArquivoXls, FinbraCampo_AbaXls) %>% 
      unlist %>% as.list()
    
    # Carrega a tabela de UG
    Fetch.UGTable <- read_excel(paste0(InputFolder, Ref.Munic$FinbraCampo_ArquivoXls), 
                                sheet = Ref.Munic$FinbraCampo_AbaXls) %>% 
      mutate(UG = as.character(UG))
    
    UGtoCodIBGE.ref6 <- DeParaUGCodIBGE %>% 
      mutate(Munic_Id6 = substr(Munic_Id, 1, 6)) %>% 
      mutate(UG = as.character(UG)) %>% 
      mutate(Partial.Ref = row_number())
    
    JoinMunic <- x %>% 
      filter(UF != "BR") %>% 
      left_join(Fetch.UGTable, by = c("MUNICÍPIOS" = "município", "UF" = "uf")) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG") %>% 
      rename(MUNICIPIOS = "MUNICÍPIOS")
    
    ExactMatch <- JoinMunic %>% 
      filter(!is.na(Munic_Id6)) %>% 
      select(-UF, -Munic_Id, -UG, -UF_Sigla, -Partial.Ref) %>% 
      # Remove campos anteriores de indentificação dos municípios.
      # Deixa a variável de indentificação com a primeira coluna da tabela.
      select(Munic_Id6, everything()) %>% 
      filter(!is.na(Munic_Id6)) %>%
      distinct(Munic_Id6, .keep_all = TRUE) %>% 
      mutate(ExactPartial = "Exact")
    
    PartialMatch <- JoinMunic %>% 
      filter(is.na(Munic_Id6)) %>% 
      select(1:3)
    
    # Uiliza a função MatchCity (acima), para encontrar as cidades sem correspondência perfeita
    # A busca é feita dentro de cada estado.
    PartialMatch$Partial.Ref <- pmap_int(.l = list(CityName = PartialMatch$MUNICIPIOS,
                                                   Region = PartialMatch$UF),
                                         .f = MatchCity,
                                         CompareData = UGtoCodIBGE.ref6, 
                                         CityNameVar = "Munic_Nome", 
                                         RegionVarName = "UF_Sigla")
    
    PartialMatch.Join <- PartialMatch %>%
      left_join(UGtoCodIBGE.ref6, by = "Partial.Ref") %>% 
      filter(!is.na(Partial.Ref)) %>% 
      mutate(ExactPartial = "Partial") %>% 
      select(names(ExactMatch))
      
    
    Output <- rbind(ExactMatch, PartialMatch.Join) %>% 
      arrange(Munic_Id6, ExactPartial) %>% 
      distinct(Munic_Id6, .keep_all = TRUE) %>% 
      select(-Munic_Nome, -MUNICIPIOS, -ExactPartial)
    
    return(Output)
  }
  
  
  # Função genérica para consertar o banco (usando uma função acima).
  GenericFunction <- function(x, FUN) {
    x <- get(FUN)(x)
  }
  
  # Executa a função genérica para consertar o banco.
  Output <- GenericFunction(x, FUN)
  
  # Retorna o banco formatado.
  return(Output)
  
}


# Função para transformar o código municipal IBGE de seis dígitos 
# em um código de sete dígitos.
MuncCod6To7 <- function(x, Munc6.Name, Munc7.Name, InputFolder) {
  
  # Linhas de debug:
  # x <- MunicFinancas.New
  # Munc6.Name <- "Munic_Id6"
  # Munc7.Name <- "Munic_Id"
  
  # Importa tabela com o código dos municípios.
  Municipios <- fread(paste0(InputFolder, "Municipios.csv"), 
                      sep = ";", dec = ",", stringsAsFactors = FALSE)
  
  # seleciona apenas as colunas relevantes.
  Municipios <- Municipios %>% 
    select(Munic_Id, Munic_Id6)
  
  # Cria um banco com o código municipal corrigido.
  Output <- x %>% 
    # Cria uma variável "temp" com os mesmos valores do código IBGE 6 dig.
    mutate_(.dots = setNames(list(Munc6.Name), "temp")) %>% 
    # Garante que essa variável seja inteiros.
    mutate(temp = as.integer(temp)) %>% 
    # Insere banco com o código de seis dígitos.
    left_join(Municipios, by = c(temp = "Munic_Id6")) %>% 
    # Remove a variável temp.
    select(-temp) %>% 
    # Coloca a variável código 7 dig como a primeira do banco.
    select(Munic_Id, everything()) %>% 
    # Remove antiga variável com IBGE 6 dig.
    select(-matches(Munc6.Name))
  
  # Deixa a variável de código municipal com o nome determinado.
  names(Output)[1] <- Munc7.Name
  
  # Retorna banco com os códigos corrigidos.
  return(Output)
}


# Função para executar um match parcial nas cidades 
## (ex: descobre que "Alvorada Do Norte" e "Alvorada Da Norte" são os mesmos)
## Tem a opção de realizar essa busca apenas dentro dos Estados.
MatchCity <- function(CityName, CompareData, CityNameVar, Region = NULL, RegionVarName = NULL) {
  
  # Variáveis para debug:
  # CityName = "ALVORADA DO OESTE"
  # CompareData = ConsolidaMunic 
  # CityNameVar = "Munic_Nome"
  # Region = "RO"
  # RegionVarName = "UF_Sigla"
  
  # Adiciona uma variável igual ao número das linhas
  CompareData <- CompareData %>% mutate(n = row_number())
  
  # Caso houve a indicação de região, faz um subset no banco. 
  if(!is.null(Region) & !is.null(RegionVarName)) {
    CompareData <- CompareData %>% 
      filter_(.dots = paste0(RegionVarName, " == ", "'", Region,"'"))
  }
  
  # Retorna o número da linha em que existe o match parcial
  CompareData$n[agrep(CityName, CompareData$Munic_Nome)][1]
}
