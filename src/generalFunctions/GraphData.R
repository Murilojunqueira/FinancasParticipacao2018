
# Get simulations data to be used in graphs using zelig

# Function to Check Binary Dependent Variable Model

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created: 2018-05-09

# Dependencies
library(tidyverse)


GraphData <- function(qi.Values, model.x, var, ci = 95, discrete = FALSE) {
  
  # model.x <- Zelig.pb.Min
  
  lineData <- list()
  plotdata <- data.frame()
  
  for (i in seq_along(qi.Values)) {
    # i <- 1
    Sims.Data <- sim(model.x, x = qi.Values[[i]])
    lineData[[i]] <- getSimData(Sims.Data, var, ci = ci, discrete = discrete)
    
    bindData <- as.data.frame(lineData[[i]]) %>% 
      mutate(Group = i)
    
    plotdata <- bind_rows(plotdata, bindData)
  }
  
  plotdata <-  plotdata %>% 
    mutate(Group = as.factor(Group))
  
}

# End