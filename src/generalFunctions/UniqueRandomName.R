
# Gera nomes aleatórios não repetidos dentro de um conjunto

# length = número de caracteres do nome
# Check = vetor de nomes em que o novo nome é único
# pattern = padrão para a criação de um novo número

# Debug:
# length <- 1
# Check <- stri_rand_strings(20, 1)
# Check
# UniqueRandomName(1, Check = Check)

# dependencies:
library(stringi)


UniqueRandomName <- function(length, Check, pattern = "[A-Za-z0-9]") {

  TestOutput <- stri_rand_strings(n = 1, length = length, pattern = pattern)
  
  while (any(Check %in% TestOutput)) {
    
    TestOutput <- stri_rand_strings(1, length, pattern = pattern)
    
  }
  return(TestOutput)
}
