
# Função para criar uma variável que é um número sequencial (ideal para criar chaves primárias). 
# Se a variável já existe, apenas modifica os NAs

# x = base de dados na qual a variável será criada
# VarName = nome da variável autosequencial

# Debug:
# x <- data.frame(var1 = 31:50,
#                 var2 = c(31:40, rep(NA, 10)))
# x
# VarName = "var2"
# 
# AutosequencialVar(x, "var2")

AutosequentialVar <- function(x, VarName) {
  
  # Test if x is a data frame
  if (!(class(x)[1] %in% c("data.table", "data.frame", "tbl_df"))) stop("x is not a data frame type")
  
  if(!(VarName %in% names(x))) {
    
    # Creat and sequencial new variable
    x[[VarName]] <- 1:nrow(x)
    
  } else {
    
    # Stop if the existing VarName is not an integer
    if(class(x[[VarName]]) != "integer") stop("VarNme is not an integer vector")
    
    # Boundaries of the new values
    MaxExistingIndex <- max(x[[VarName]], na.rm = TRUE)
    NumberofNAs <- MaxExistingIndex + length(x[[VarName]][is.na(x[[VarName]])])
    
    # Add new values
    x[[VarName]][is.na(x[[VarName]])] <-  (MaxExistingIndex + 1):NumberofNAs
      
  }
  return(x)
}
