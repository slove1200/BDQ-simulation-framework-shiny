Pop_generation <- function(input) {
  nsubjects <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory
  
  # Read in dataset for conditional distribution modeling for covariates distribution
  myCovSimMICE <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Big_Virtual_Population_SimulatedforUse.csv", 
                        header = T)
  
  df_virtualPop <- myCovSimMICE %>% filter(ID == c(1:(nsubjects*num_regimens)))
  
  return(df_virtualPop)
}