# Script para Importar dados do Finbra no R.

# Script Para guardar as fun??es de extra??o dos dados do Finbra.

# Criado por Murilo Junqueira.

# Data cria??o: 2018-02-27.
# Ultima modifica??o: 2018-03-01.


################## Carrega pacotes necess?rios ##################

# Lista de pacotes necess?rios para as fun??es desse arquivo.
list.of.packages <- c("tidyverse",
                      "data.table", 
                      "dplyr", 
                      "tidyr", 
                      "stringr",
                      "lubridate",
                      "readxl")

# Verifica os que n?o est?o instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Instala os pacotes n?o instalados
if(length(new.packages)) install.packages(new.packages)

# L? todos os pacotes
for(i in 1:length(list.of.packages)) {
  #print(paste("Lendo o pacote", list.of.packages[i]))
  library(list.of.packages[i], character.only = TRUE)  
}

# Libera mem?ria
rm(list.of.packages, new.packages, i)



################## Fun??es ##################

# Fun??es para formata??es b?sicas dos dados do Finbra para cada ano.
FormataFinbra <- function(x, Ano, Aba, UGtoCodIBGE = NULL, BDCamposFinbra = NULL, 
                          InputFolder = NULL){
  
  # Linhas de debug.
  # x <- MunicFinancas.New
  # Ano <- BDCamposFinbra.Select$FinbraCampo_Ano
  
  # Garante que o Ano ? uma vari?vel inteira (integer).
  Ano <- as.integer(Ano)
  
  # Atribui a fun??o correta para o dado  
  ## 1998 a 2012 para a fun??o "Fun2012_1998"
  if(Ano <= 2012 & Ano >= 1998) {
    FUN <-  "Fun2012_1998" 
    } else if(Ano == 1997) {
      ## 1997 para a fun??o "Fun1997"
      FUN <-  "Fun1997"
    } else if(Ano <= 1996 & Ano >= 1992) {
      ## 1996 a 1994 para a fun??o "Fun1996_1994"
      FUN <-  "Fun1996_1994"
    }
  
  # Caso o ano indicado n?o corresponder a nenhuma fun??o determinada, retorna erro.
  if(is.null(FUN)) {
    stop("Deve-se indicar um valor de ano com uma fun??o correspondente")
  }
  
  # fun??o para formatar os dados de 1998 a 2012.
  Fun2012_1998 <- function(x){
    
    Output <- x %>% 
      # Cria uma vari?vel uniformizada de indentifica??o dos munic?pios
      mutate(Munic_Id6 = paste0(CD_UF, str_pad(CD_MUN, 4, "left", "0"))) %>% 
      # Remove campos anteriores de indentifica??o dos munic?pios.
      select(-CD_UF, -CD_MUN) %>% 
      # Deixa a vari?vel de indentifica??o com a primeira coluna da tabela.
      select(Munic_Id6, everything())
  }
  
  # fun??o para formatar os dados de 1998 a 2012.
  Fun1997 <- function(x){
    
    UGtoCodIBGE.ref6 <- DeParaUGCodIBGE %>% 
      mutate(Munic_Id6 = substr(Munic_Id, 1, 6)) %>% 
      mutate(UG = as.character(UG))
    
    Output <- x %>% 
      mutate(UG = as.character(UG)) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG")  %>% 
      # Remove campos anteriores de indentifica??o dos munic?pios.
      select(-NOME, -UF, -Munic_Id, -UG, -Munic_Nome, -UF_Sigla) %>% 
      # Deixa a vari?vel de indentifica??o com a primeira coluna da tabela.
      select(Munic_Id6, everything())
  }
  
  
  # fun??o para formatar os dados de 1998 a 2012.
  Fun1996_1994 <- function(x){
    
    # Carrega a aba onde est?o os dados de UG
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
      left_join(Fetch.UGTable, by = c("MUNIC?PIOS" = "munic?pio", "UF" = "uf")) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG") %>% 
      rename(MUNICIPIOS = "MUNIC?PIOS")
    
    ExactMatch <- JoinMunic %>% 
      filter(!is.na(Munic_Id6)) %>% 
      select(-UF, -Munic_Id, -UG, -UF_Sigla, -Partial.Ref) %>% 
      # Remove campos anteriores de indentifica??o dos munic?pios.
      # Deixa a vari?vel de indentifica??o com a primeira coluna da tabela.
      select(Munic_Id6, everything()) %>% 
      filter(!is.na(Munic_Id6)) %>%
      distinct(Munic_Id6, .keep_all = TRUE) %>% 
      mutate(ExactPartial = "Exact")
    
    PartialMatch <- JoinMunic %>% 
      filter(is.na(Munic_Id6)) %>% 
      select(1:3)
    
    # Uiliza a fun??o MatchCity (acima), para encontrar as cidades sem correspond?ncia perfeita
    # A busca ? feita dentro de cada estado.
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
  
  
  # Fun??o gen?rica para consertar o banco (usando uma fun??o acima).
  GenericFunction <- function(x, FUN) {
    x <- get(FUN)(x)
  }
  
  # Executa a fun??o gen?rica para consertar o banco.
  Output <- GenericFunction(x, FUN)
  
  # Retorna o banco formatado.
  return(Output)
  
}


# Fun??o para transformar o c?digo municipal IBGE de seis d?gitos 
# em um c?digo de sete d?gitos.
MuncCod6To7 <- function(x, Munc6.Name, Munc7.Name, InputFolder) {
  
  # Linhas de debug:
  # x <- MunicFinancas.New
  # Munc6.Name <- "Munic_Id6"
  # Munc7.Name <- "Munic_Id"
  
  # Importa tabela com o c?digo dos munic?pios.
  Municipios <- fread(paste0(InputFolder, "Municipios.csv"), 
                      sep = ";", dec = ",", stringsAsFactors = FALSE)
  
  # seleciona apenas as colunas relevantes.
  Municipios <- Municipios %>% 
    select(Munic_Id, Munic_Id6)
  
  # Cria um banco com o c?digo municipal corrigido.
  Output <- x %>% 
    # Cria uma vari?vel "temp" com os mesmos valores do c?digo IBGE 6 dig.
    mutate_(.dots = setNames(list(Munc6.Name), "temp")) %>% 
    # Garante que essa vari?vel seja inteiros.
    mutate(temp = as.integer(temp)) %>% 
    # Insere banco com o c?digo de seis d?gitos.
    left_join(Municipios, by = c(temp = "Munic_Id6")) %>% 
    # Remove a vari?vel temp.
    select(-temp) %>% 
    # Coloca a vari?vel c?digo 7 dig como a primeira do banco.
    select(Munic_Id, everything()) %>% 
    # Remove antiga vari?vel com IBGE 6 dig.
    select(-matches(Munc6.Name))
  
  # Deixa a vari?vel de c?digo municipal com o nome determinado.
  names(Output)[1] <- Munc7.Name
  
  # Retorna banco com os c?digos corrigidos.
  return(Output)
}


# Fun??o para executar um match parcial nas cidades 
## (ex: descobre que "Alvorada Do Norte" e "Alvorada Da Norte" s?o os mesmos)
## Tem a op??o de realizar essa busca apenas dentro dos Estados.
MatchCity <- function(CityName, CompareData, CityNameVar, Region = NULL, RegionVarName = NULL) {
  
  # Vari?veis para debug:
  # CityName = "ALVORADA DO OESTE"
  # CompareData = ConsolidaMunic 
  # CityNameVar = "Munic_Nome"
  # Region = "RO"
  # RegionVarName = "UF_Sigla"
  
  # Adiciona uma vari?vel igual ao n?mero das linhas
  CompareData <- CompareData %>% mutate(n = row_number())
  
  # Caso houve a indica??o de regi?o, faz um subset no banco. 
  if(!is.null(Region) & !is.null(RegionVarName)) {
    CompareData <- CompareData %>% 
      filter_(.dots = paste0(RegionVarName, " == ", "'", Region,"'"))
  }
  
  # Retorna o n?mero da linha em que existe o match parcial
  CompareData$n[agrep(CityName, CompareData$Munic_Nome)][1]
}
