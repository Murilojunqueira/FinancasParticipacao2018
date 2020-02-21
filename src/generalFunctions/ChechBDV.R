# Function to Check Binary Dependent Variable Model

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created: 2018-05-09

################## Setup Working Space ##################


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


# End