# Convert a logarithmic scale to original scale.
# Ideal to graph labels.

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created: 2018-05-09

exp_f <- function(x, lan = "en") {
  
  
  if( lan == "en") {
    unidade <- ifelse(exp(x)>1000000, "millions", "thousand")
  }
  
  if( lan == "pt") {
    unidade <- ifelse(exp(x)>1000000, "milhoes", "mil")
  }
  
  divisao <- ifelse(exp(x)>1000000, 1000000, 1000)
  digitos <- ifelse(exp(x)>1000000, 1, 0)
  
  exp(x) %>% 
    "/"(divisao) %>% 
    round(digitos) %>% 
    format(big.mark = ".", decimal.mark = ",") %>% 
    paste(unidade) %>% 
    trimws
}