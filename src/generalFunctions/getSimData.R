# Get simulated data from zelig objetc

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created: 2018-05-09

# Dependencies
library(tidyverse)

getSimData <- function(Sim.Objetc, var, ci = 95, discrete = FALSE) {
  
  # Sim.Objetc <- Zelig.pb.Min
  # qi.Values <- setx(Zelig.pb.Min, InvestPer = seq(0, 0.3, by=0.01), lag.pb = 0)
  # Sim.Objetc <- Sims.Data
  
  #extract ev
  if(discrete == FALSE) {
    myev <- Sim.Objetc$get_qi(qi='ev', xvalue = 'range')
  } else {
    myev <- Sim.Objetc$get_qi(qi='ev')
  }
  
  
  #convert the list into matrix
  nrowData <- ifelse(discrete == FALSE, length(myev[[1]]), nrow(myev))
  
  myev2 <- unlist(myev) %>% 
    matrix(nrow = nrowData) %>% 
    as.data.frame()
  
  #create plot data
  #This step is to create quantiles
  
  AlphaValues <- c((1-ci/100), 1-(1-ci/100)) 
  
  a <- apply(myev2, 2, quantile, probs = AlphaValues)
  low <- a[1,]
  high <- a[2,]
  mean <- apply(myev2, 2, mean) 
  
  plotdata <- as.data.frame(cbind(low, high, mean)) 
  
  
  if(discrete == FALSE) {
    # qi names
    qi.Vars <- names(Sim.Objetc[[".self"]][["range"]])
    
    # qi in x axis
    Var.Ref <- which(qi.Vars == var)
    
    # range of x
    Var.Range <- Sim.Objetc[[".self"]][["range"]][[Var.Ref]]
    
    # Insert range of x
    plotdata[[var]] <- Var.Range
    
    # Frequency table
    plotdata$Freq <- Sim.Objetc[["data"]][[var]] %>% # var data
      cut(c(Var.Range[1] - Var.Range[2], Var.Range), # create one more leval ant the beginning
          right=FALSE) %>%  # get frequency table
      table() %>% as.numeric() # table frequency
    
  }
  
  return(plotdata)
}