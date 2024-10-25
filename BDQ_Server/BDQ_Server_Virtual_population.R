simCovMICE <- function(m = 5, 
                       orgCovs, 
                       catCovs = c("SEX","RACE","TBTYPE"), 
                       seedCovs = NULL,
                       targetRangeSeedCovs = NULL,
                       seedCovsValues = NULL,
                       nsubj = nrow(orgCovs),
                       contMeth = "pmm",
                       sampleFromReal = T) {
  
  # names of continuous covariates
  contCovs <- setdiff(names(orgCovs),catCovs)
  
  # create copy of the original data set with factor version of categorical covariates
  orgCovsF <- orgCovs %>% dplyr::mutate_at(catCovs,function(x) as.factor(x))
  
  ## find covariates with missing values
  missVars <- names(orgCovs)[colSums(is.na(orgCovs)) > 0]
  
  # impute missing data once with mice  
  if(length(missVars)>0)
  {
    imp1 <- mice::mice(orgCovsF, m=1, printFlag=FALSE, maxit = 15)
    orgCovs <- mice::complete(imp1)
  }
  
  miCovs <- orgCovs[1:nsubj,] %>% mutate_all(function(x) NA)
  
  
  if(!is.null(seedCovs)) 
  {
    if(sampleFromReal)
    {
      ## create vector of seedCov values contained in original data set
      poolSeed <-  orgCovs[orgCovs[,seedCovs]>=min(targetRangeSeedCovs) & orgCovs[,seedCovs]<=max(targetRangeSeedCovs),seedCovs]
      ## do the actual sampling
      miCovs[seedCovs] <- sample(poolSeed,nsubj,replace = T)
    }
    else
      miCovs[seedCovs] <-  seedCovsValues 
  }
  
  combCovs <- orgCovs %>% mutate(Type="Original") %>% 
    bind_rows(miCovs %>% mutate(Type="Simulated")) %>%
    mutate_at(catCovs,function(x) as.factor(x))
  
  myPredMat <- mice::make.predictorMatrix(combCovs)
  myPredMat[,c("Type")] <- 0
  
  myMethods <- mice::make.method(combCovs)
  myMethods[contCovs] <- contMeth
  
  imp2 <-mice::mice(combCovs, m=m,printFlag=FALSE,predictorMatrix = myPredMat, method = myMethods)
  
  impCovs <- mice::complete(imp2,action="long") %>% 
    filter(Type=="Simulated") %>% 
    mutate(NSIM=.imp) %>% 
    select(everything(),-.id,-Type,-.imp)
  
  return(impCovs)
  
}


Pop_generation <- function(input) {
  nsubjects <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory
  
  # Read in dataset for conditional distribution modeling for covariates distribution
  orgCovsEx <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/Simulated_population_for_BDQ_virtural_population.csv", 
                        header = T)
  # Differentiate categorical and continuous variables
  categorical_vars <- c("SEX", "RACE", "TBTYPE")  # Replace with your actual categorical variables
  continuous_vars <- c("AGE", "MTTP", "CACOR", "K")  # Replace with your actual continuous variables
  
  # Conditional distribution modeling for covariates distribution
  # Numbers of subjects depending on input (numbers of simulated individuals)
  set.seed(3468)
  myCovSimMICE <- simCovMICE(m = 1, 
                             orgCovs = orgCovsEx,
                             catCovs = c("SEX", "RACE", "TBTYPE"),
                             nsubj = nsubjects*num_regimens)

  myCovSimMICE <- myCovSimMICE %>% ungroup() %>% 
    mutate(regimen = rep(1:num_regimens, each = nsubjects)) %>%
    # correspondent to unique ID defined in Server_PK
    group_by(regimen) %>%
    mutate(ID = row_number()) %>%
    mutate(ID = ifelse(regimen == 1, ID, ID+nsubjects*(regimen-1))) %>% 
    select(ID, everything(), -NSIM) %>%
    mutate_if(is.factor, ~ as.numeric(as.character(.)))
  
  return(myCovSimMICE)
}