sim_QT <- function(input, sim_PKtable) {
  ## Simulation settings
  # 1. "nsim"
  nsamples <- input$nsim      # Number of simulated individuals
  
  # 2. "simtime" and "simunit"
  sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)
  sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
  
  # Create dataset for simulation
  dfQT <- sim_PKtable()
  dfQT <- subset(dfQT, select = c(ID, regimen, time, IPREDM2, RACE, AGE))
  dfQT <- dfQT %>% mutate(CONCM2 = exp(IPREDM2) * 1000) ## mg/L to ng/mL (concentration unit used in QT model)
  dfQT$TIMW <- 0 #dfQT$time / 24 / 7 ## TIMW = TAST/24/7 for time effect in QT model
  
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
  
  # 3. Co-medication QT
  dfQT$CLOFA <- 0
  dfQT$MOXI <- 0
  
  for (i in 1:num_regimens) {
    
    IE <- input[[paste0("IE_", i)]]
    
    if (!is.null(IE)) {
      dfQT <- dfQT %>%
        mutate(
          CLOFA = ifelse(regimen == i & IE == "Clofazimine", 1, CLOFA),  # Set CLOFA based on IE
          MOXI  = ifelse(regimen == i & IE == "Moxifloxacin", 1, MOXI)   # Set MOXI based on IE
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
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  } else {
    outQT <- modQT %>%
      zero_re(sigma) %>%
      data_set(dfQT) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  }
  
  return(outQT)
}