# Função para transformar o código municipal IBGE de seis dígitos 
# em um código de sete dígitos.

# Dependencies
library(dplyr)

# Linhas de debug:
# x <- MunicFinancas.New
# Munc6.Name <- "Munic_Id6"
# Munc7.Name <- "Munic_Id"

MuncCod6To7 <- function(x, Munc6.Name, Municipios) {

  
  # seleciona apenas as colunas relevantes.
  Municipios.Select <- Municipios %>% 
    select(Munic_Id, Munic_Id6)
  
  names(Municipios.Select)
  
  # Cria um banco com o código municipal corrigido.
  Output <- x %>% 
    # Cria uma variável "temp" com os mesmos valores do código IBGE 6 dig.
    rename(temp = Munc6.Name) %>% 
    # Garante que essa variável seja inteiros.
    mutate(temp = as.integer(temp)) %>% 
    # Insere banco com o código de seis dígitos.
    left_join(Municipios.Select, by = c(temp = "Munic_Id6")) %>% 
    # Coloca a variável código 7 dig como a primeira do banco.
    select(Munic_Id, everything()) %>% 
    # Remove antiga variável com IBGE 6 dig.
    select(-temp)
  
  # Retorna banco com os códigos corrigidos.
  return(Output)
}

# End