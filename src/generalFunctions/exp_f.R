# Convert a logarithmic scale to original scale.
# Ideal to graph labels.

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created: 2018-05-09

# Debug: 
# x <- 10:16

exp_f <- function(x, lan = "en") {
  
  
  if( lan == "en") {
    unidade <- ifelse(exp(x)>1000000, "million", "thousand")
  }
  
  if( lan == "pt") {
    unidade <- ifelse(exp(x)>1000000, "milhões", "mil")
  }
  
  divisao <- ifelse(exp(x)>1000000, 1000000, 1000)
  digitos <- ifelse(exp(x)>1000000, 1, 0)
  
  Output <- exp(x) %>% 
    "/"(divisao) %>% 
    round(digitos) %>% 
    paste(unidade) %>% 
    trimws
  
  if( lan == "pt") {
  Output <- Output %>% 
    gsub(".", "PPPP", ., fixed = TRUE) %>% 
    gsub(",", "VVVV", ., fixed = TRUE) %>%
    gsub("PPPP", ",", ., fixed = TRUE) %>% 
    gsub("VVVV", ".", ., fixed = TRUE)
  }
    
  return(Output)
}
