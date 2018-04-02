# Script para Importar dados do Finbra no R.

# Script Para guardar as funções de extração dos dados do Finbra.

# Criado por Murilo Junqueira.

# Data criação: 2018-02-27.
# Ultima modificação: 2018-03-01.


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
FormataFinbra <- function(x, Ano, Aba){
  
  # Linhas de debug.
  # x <- MunicFinancas.New
  # Ano <- BDCamposFinbra.Select$FinbraCampo_Ano
  
  # Garante que o Ano é uma variável inteira (integer).
  Ano <- as.integer(Ano)
  
  # Atribui os anos entre 1998 a 2012 para a função "Fun2012_1998"
  FUN <- if(Ano <= 2012 & Ano >= 1998) "Fun2012_1998"
  
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
