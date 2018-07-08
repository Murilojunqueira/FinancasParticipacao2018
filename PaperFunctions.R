# Valores preditos do modelo em gráficos:

## Working paper "Why has the Participatory Budgeting declined in Brazil?"

# Functions for PaperModel.R. File 2 of 3


# By Murilo Junqueira & Carla Bezzera

# Created: 2018-05-09
# Last Modified: 2018-07-07

################## Setup Working Space ##################


# instala o pacote de extração dos dados, se necessário
library(data.table)
#library(zeligverse)
library(scales)
library(tidyverse)
library(Zelig)



##################### Functions #####################

# # model.x <- LPM.pb.Min
# 
# Randon.Betas.Function <- function(model.x, M = 1000) {
#   
#   # Save coeficients
#   coefficients <- model.x$coefficients
#   
#   # Save Standard error
#   se <- summary(model.x)[[4]][,2]
#   
#   # Save sigma
#   model_sigma <- sigma(model.x)
#   
#   # Save dregrees of freedom
#   df <- summary(model.x)$df[2]
#   
#   # Create an empyt dataset to gather all beta values
#   randonBetas <- data.frame(matrix(nrow=M, ncol=0))
#   
#   for (i in seq_len(length(coefficients))) {
#     
#     # Debug line
#     # i <- 1
#     
#     # Generate M radom values between 0 and 1.
#     randonVector <- runif(M)
#     
#     # Vector to save beta values
#     randonBeta.i <- numeric(M)
#     
#     for (j in seq_len(length(randonVector))) {
#       
#       # Debug line
#       # j <- 1
#       
#       # Normal distribution
#       #newBeta <- qnorm(p = randonVector[j], 
#       #                 mean = coefficients[i], 
#       #                 sd = se[i])
#       
#       # t of student distribution
#       newBeta <- qt(p = randonVector[j], 
#                     df = df)
#       
#       # Set betas in the same scale of variables
#       newBeta <- (newBeta * se[i]) + coefficients[i]
#       
#       # Add to gathering vector
#       randonBeta.i[j] <- as.numeric(newBeta)
#     }
#     
#     # randonBeta.i
#     
#     # Transform in data frame and correct variables names
#     randonBeta.i.Format <- randonBeta.i %>% 
#       as.data.frame() %>% 
#       set_names(names(coefficients[i])) 
#     
#     # Add to main randon beta's data frame
#     randonBetas <- cbind(randonBetas, randonBeta.i.Format)
#     
#     # Free memory
#     rm(randonVector, j, randonBeta.i, newBeta, randonBeta.i.Format)
#   }
#   # Free memory
#   rm(i)
#   
#   return(randonBetas)
# }
# 
# 
# 
# 
# Interactive.Generator <- function(Model.Data, Interaction.Vars){
#   
#   for(i in seq_len(length(Interaction.Vars))){
#     
#     Colon.Position <- gregexpr(":", Interaction.Vars[i])[[1]][1]
#     
#     Interaction.Vars.i <- Interaction.Vars[i]
#     
#     var1 <- substr(Interaction.Vars.i, 1, Colon.Position-1)
#     var2 <- substr(Interaction.Vars.i, Colon.Position+1, nchar(Interaction.Vars.i))
#     
#     NewVarName <- paste0(var1, ":",var2)
#     
#     Model.Data[[NewVarName]] <- Model.Data[[var1]] * Model.Data[[var2]]
#     
#   }
#   
#   return(Model.Data)
# }
# 
# 
# genSimValues <- function(Model.Data, randonBetas, Vars.Simulate, MinValue, MaxValue, Step){
#   
#   # Set values of interest: 
#   
#   PointEstimates <- as.data.frame(t(as.matrix(MinValue)))
#   
#   while(any(map_dbl(PointEstimates, max) < MaxValue)) {
#     new.Data <- as.numeric(PointEstimates[nrow(PointEstimates),]) + Step
#     PointEstimates <- rbind(PointEstimates, new.Data)
#   }
#   rm(new.Data)
#   
#   # Checo values of interest:
#   # PointEstimates
#   
#   # Set all parameter at means
#   X <- map_dbl(Model.Data[c(1, 3:ncol(Model.Data))], mean)
#   # X
#   
#   # Mean as parameters
#   Coefs <- as.data.frame(X)
#   
#   # Set values of interests, holding all other paramenter at means
#   for(i in seq_len(nrow(PointEstimates))) {
#     X.New <- X
#     for(j in seq_len(length(Vars.Simulate))){
#       X.New[which(names(X.New) == Vars.Simulate[j])] <- PointEstimates[i, j]
#     }
#     Coefs[[paste0("X", i)]] <- as.numeric(X.New)
#   }
#   
#   # Data Frame to Store all values
#   Sim.Values <- data.frame(matrix(nrow=nrow(randonBetas), ncol=0))
#   
#   # Predicted values
#   for(i in seq_len(ncol(Coefs)-1)){
#     yhat <- as.matrix(randonBetas) %*% as.matrix(Coefs[[i+1]])
#     Sim.Values <- cbind(Sim.Values, yhat)
#     names(Sim.Values)[ncol(Sim.Values)] <- names(Coefs)[i+1]
#   }
#   
#   #names(Sim.Values)
#   return(Sim.Values)
# }


checkModel <- function(model, predict.level = 0.5) {
  
  # AIC 
  AIC <- AIC(model)
  BIC <- BIC(model)
  
  # Accuracy 
  predict <- predict(model, type = 'response')
  predict <- ifelse(predict > predict.level, 1, 0)
  
  
  dependent.var <- model[["model"]][1][[1]]
  
  misClasificError <- mean(predict != dependent.var)
  
  Accuracy <- paste('The Accuracy level is:', format(1-misClasificError, digits = 6))
  
  #confusion matrix
  predict <- predict(model, type = 'response')
  
  confusion.matrix <- table(dependent.var, predict > predict.level)
  
  # False Positive rate
  
  False.Positive.Rate <- as.numeric(confusion.matrix[1,2]) / rowSums(confusion.matrix)[[1]] %>% 
    as.numeric() 
  False.Positive.Rate <- scales::percent(False.Positive.Rate)
  
  # False Negative rate
  
  False.Negative.Rate <- as.numeric(confusion.matrix[2,1]) / rowSums(confusion.matrix)[[2]] %>% 
    as.numeric() 
  False.Negative.Rate <- scales::percent(False.Negative.Rate)
  
  
  # Print Results:
  print(summary(model))
  cat(paste("AIC:", AIC, "\n"))
  cat(paste("BIC:", BIC, "\n"))
  cat(paste(Accuracy, "\n"))
  print(confusion.matrix)
  cat(paste("\n", "The false positive rate is", False.Positive.Rate, "\n"))
  cat(paste0("The false negative rate is ", False.Negative.Rate))
  
}

getSimData <- function(Sim.Objetc, var, ci = 95) {
  
  #extract ev
  myev <- Sim.Objetc$get_qi(qi='ev', xvalue = 'range')
  
  #convert the list into matrix
  myev2 <- as.data.frame(matrix(unlist(myev), nrow = nrow(myev[[1]])))
  
  #create plot data
  #This step is to create quantiles
  
  AlphaValues <- c((1-ci/100), 1-(1-ci/100)) 
  
  a<- apply(myev2, 2, quantile, probs = AlphaValues)
  low <- a[1,]
  high <- a[2,]
  mean <- apply(myev2, 2, mean) 
  
  plotdata <- as.data.frame(cbind(low, high, mean)) 
  
  # qi names
  qi.Vars <- names(Sim.Objetc[[".self"]][["range"]])
  
  # qi in x axis
  Var.Ref <- which(qi.Vars == var)
  
  # range of x
  Var.Range <- Sim.Objetc[[".self"]][["range"]][[Var.Ref]]
  
  # Insert range of x
  plotdata[[var]] <- Var.Range
  
  return(plotdata)
}


Graph.Data <- function(qi.Values, model.x, var, ci = 95) {
  
  lineData <- list()
  plotdata <- data.frame()
  
  for (i in seq_along(qi.Values)) {
    Sims.Data <- sim(model.x, x = qi.Values[[i]])
    lineData[[i]] <- getSimData(Sims.Data, var, ci = ci)
    
    bindData <- as.data.frame(lineData[[i]]) %>% 
      mutate(Group = i)
    
    plotdata <- rbind(plotdata, bindData)
  }
  
  plotdata <-  plotdata %>% 
    mutate(Group = as.factor(Group))
  
}


exp.f <- function(x) {
  
  unidade <- ifelse(exp(x)>1000000, "milhões", "mil")
  divisao <- ifelse(exp(x)>1000000, 1000000, 1000)
  digitos <- ifelse(exp(x)>1000000, 1, 0)
  
    exp(x) %>% 
    "/"(divisao) %>% 
    round(digitos) %>% 
    format(big.mark = ".", decimal.mark = ",") %>% 
    paste(unidade) %>% 
      trimws
}

# End