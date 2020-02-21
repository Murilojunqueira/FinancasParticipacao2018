# Função para instalar um pacote (caso necessário) e então ler ele

# Criação: Murilo Junqueira (m.junqueira@yahoo.comn.br)

# Data Criação: 2019-12-14

# ListaPacotes <- c("nloptr", "RcppEigen", "carData", "abind", "pbkrtest", "maptools", "rio",
#                   "lme4", "expm", "gmm", "iterators", "car", "lmtest", "zoo", "miscTools",
#                   "mcmc", "SparseM", "MatrixModels", "minqa", "numDeriv", "mitools", "msm",
#                   "mvtnorm", "tmvtnorm", "ellipse", "plotrix", "ucminf", "cubature", "mnormt",
#                   "foreach", "sp", "RcppArmadillo", "Matching", "lpSolve", "pbmcapply", "AER",
#                   "coda", "Formula", "geepack", "sandwich", "maxLik", "MCMCpack", "quantreg",
#                   "survey", "VGAM", "eiPack", "ei", "Amelia", "MatchIt", "WhatIf", "Zelig",
#                   "ZeligChoice", "ZeligEI")

LoadInstall(ListaPacotes)

LoadInstall <- function(ListaPacotes) {
  for (i in seq_along(ListaPacotes)) {
    # i <- 1
    if (!(ListaPacotes[i] %in% rownames(installed.packages()))) {
      install.packages(ListaPacotes[i])
    }
    library(ListaPacotes[i], character.only = TRUE)
  }
}
