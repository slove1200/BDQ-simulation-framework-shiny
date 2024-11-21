sim_QT <- function(input, sim_PKtable) {
  ## Simulation settings
  # 2. "simtime" and "simunit"
  sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)
  
  # Create dataset for simulation
  dfQT <- sim_PKtable
  dfQT <- subset(dfQT, select = c(ID, regimen, time, IPREDM2, RACE, AGE, SEX, CACOR, K))
  dfQT <- dfQT %>% mutate(CONCM2 = exp(IPREDM2) * 1000) ## mg/L to ng/mL (concentration unit used in QT model)
  dfQT$TIMW <- 0 #dfQT$time / 24 / 7 ## TIMW = TAST/24/7 for time effect in QT model
  
  # if UI input is to simulate in an individual-level
  if (input$population_radio == "Individual") {
    
    ## Covariates
    # 1. "SEX"
    if (input$SEX == "Male") {
      dfQT$SEX <- 0
    } else {
      dfQT$SEX <- 1
    }
    
    # 2. Electrolytes level (Corrected Ca and Potassium level)
    dfQT$CACOR <- input$CACOR # 2.440
    dfQT$K <- input$K # 4.200
  }
  
  # 3. Co-medication QT
  dfQT$CLOFA <- 0
  dfQT$MOXI <- 0
  
  # Create a list to hold the selected regimens
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory
  
  for (i in 1:num_regimens) {
    
    IE <- input[[paste0("IE_", i, "_QT")]]
    
    if (!is.null(IE)) {
      dfQT <- dfQT %>%
        mutate(
          CLOFA = ifelse(regimen == i & IE %in% c("Clofazimine", "Both"), 1, CLOFA),  # Set CLOFA based on IE
          MOXI  = ifelse(regimen == i & IE %in% c("Moxifloxacin", "Both"), 1, MOXI)   # Set MOXI based on IE
        )
    }
  }
  
  # QT simulation ########
  modQT <- mcode("BDQQT", codeQT)
  set.seed(3468)
  
  ## Interindividual variability ON/OFF
  if (input$IIV == "OFF") {
    outQT <- modQT %>%
      zero_re() %>%
      data_set(dfQT) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  } else {
    outQT <- modQT %>%
      zero_re(sigma) %>%
      data_set(dfQT) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  }
  
  return(outQT)
}