# Format data extract from Finbra


# Dependencies:
library(dplyr)

################## External Functions ##################

source("src/generalFunctions/PartialMatchFilter.R")

################## Funções ##################


# Debug:
# x <- MunicFinancas.New
# Ano = "1996"
# Aba = "Despesa"
# UGtoCodIBGE = DeParaUGCodIBGE
# BDCamposFinbra = BDCamposFinbra
# InputFolder = "data/raw/Finbra/ExcelFiles/"


# Funções para formatações básicas dos dados do Finbra para cada ano.
FormatFinbra <- function(x, Ano, Aba, UGtoCodIBGE = NULL, BDCamposFinbra = NULL, 
                          InputFolder = NULL){

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
  } else {
    # Caso o ano indicado não corresponder a nenhuma função determinada, retorna erro.
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
      mutate(UG = as.character(UG)) %>% 
      select(Munic_Id6, UG)
    
    Output <- x %>% 
      mutate(UG = as.character(UG)) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG") %>% 
      # Remove campos anteriores de indentificação dos municípios.
      select(-UG) %>% 
      # Deixa a variável de indentificação com a primeira coluna da tabela.
      select(Munic_Id6, everything())
  }
  
  
  # função para formatar os dados de 1998 a 2012.
  Fun1996_1994 <- function(x){
    
    # Carrega a aba onde estão os dados de UG
    Ref.Munic <- BDCamposFinbra %>% 
      dplyr::filter(FinbraCampo_ArquivoXls == "Finbra94-96.xlsx") %>% 
      dplyr::filter(FinbraCampo_Tabela == "ugmunicipios") %>% 
      dplyr::filter(FinbraCampo_Campo == "UG") %>% 
      select(FinbraCampo_ArquivoXls, FinbraCampo_Tabela) %>% 
      unlist %>% as.list()
    
    # Carrega a tabela de UG
    Fetch.UGTable <- read_excel(paste0(InputFolder, Ref.Munic$FinbraCampo_ArquivoXls), 
                                sheet = Ref.Munic$FinbraCampo_Tabela) %>% 
      mutate(UG = as.character(UG))
    # Tenta encontrar o máximo de municípios através da correspondência entre UG e 
    # O código do IBGE.
    UGtoCodIBGE.ref6 <- DeParaUGCodIBGE %>% 
      mutate(Munic_Id6 = substr(Munic_Id, 1, 6)) %>% 
      mutate(UG = as.character(UG)) %>% 
      mutate(Partial.Ref = row_number())
    
    JoinMunic <- x %>% 
      dplyr::filter(UF != "BR") %>% 
      left_join(Fetch.UGTable, by = c("MUNICÍPIOS" = "município", "UF" = "uf")) %>% 
      left_join(UGtoCodIBGE.ref6, by = "UG") %>% 
      rename(MUNICIPIOS = "MUNICÍPIOS")
    
    ExactMatch <- JoinMunic %>% 
      dplyr::filter(!is.na(Munic_Id6)) %>% 
      select(-UF, -Munic_Id, -UG, -UF_Sigla, -Partial.Ref) %>% 
      # Remove campos anteriores de indentificação dos municípios.
      # Deixa a variável de indentificação com a primeira coluna da tabela.
      select(Munic_Id6, everything()) %>% 
      dplyr::filter(!is.na(Munic_Id6)) %>%
      distinct(Munic_Id6, .keep_all = TRUE) %>% 
      mutate(ExactPartial = "Exact")
    
    PartialMatch <- JoinMunic %>% 
      dplyr::filter(is.na(Munic_Id6)) %>% 
      select(1:3)
    
    # Uiliza a função MatchCity, para encontrar as cidades sem correspondência perfeita
    # A busca é feita dentro de cada estado.
    
    PartialMatch$Partial.Ref <- NA
    
    for (h in seq_len(nrow(PartialMatch))) {
      # h <- 5
      FilterList.h <- UGtoCodIBGE.ref6$UF == PartialMatch$UF[h]
      
      PartialMatch$Partial.Ref[h] <- PartialMatchFilter(x = PartialMatch$MUNICIPIOS[h],
                                                        CompareData = UGtoCodIBGE.ref6$Munic_Nome,
                                                        FilterList = FilterList.h)
    }
    rm(FilterList.h)
    
    
    PartialMatch.Join <- PartialMatch %>%
      left_join(UGtoCodIBGE.ref6, by = "Partial.Ref") %>% 
      dplyr::filter(!is.na(Partial.Ref)) %>% 
      # As duas linhas abaixo são para checar os nomes
      mutate(ExactPartial = "Partial") %>% 
      mutate(TestNome = MUNICIPIOS == Munic_Nome) %>% 
      select(TestNome, MUNICIPIOS, Munic_Nome, UF, UF_Sigla, everything()) %>% 
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


# End