# Função para executar um match parcial nas cidades 
## (ex: descobre que "Alvorada Do Norte" e "Alvorada Da Norte" são os mesmos)
## Tem a opção de realizar essa busca Utilizando Filtros

# debug:
# x = "AGUA AZUL DO NORTE"
# CompareData = UGtoCodIBGE.ref6$Munic_Nome
# FilterList = UGtoCodIBGE.ref6$UF_Sigla == "PA"
# 
# 
# CompareData <- c("Alvorada Da Norte", "Alvorada Do oeste", "CACOAL", 
#                  "CEREJEIRAS", "COLORADO DO OESTE"  )
# FilterList = NULL

PartialMatchFilter <- function(x, CompareData, FilterList = NULL) {
  
  if(!is.character(x)) {
    stop("x must be a character string")
  }
  
  if(!is.character(CompareData)) {
    stop("CompareData must be a character string")
  }
  
  # Caso houve a indicação de região, faz um subset no banco. 
  if(!is.null(FilterList)) {
    if(!is.logical(FilterList)) {
      stop("FilterList must be a logical string")
    }
    CompareData.Filter <- CompareData[FilterList]
    output <- which(CompareData == CompareData.Filter[agrep(x, CompareData.Filter)[1]])
  } else {
    # Retorna o número da linha em que existe o match parcial
    output <- agrep(x, CompareData)
  }
  
  if(length(output) == 0) {
    return(NA)
  } else {
    return(output[[1]])
  }
  
}
# End