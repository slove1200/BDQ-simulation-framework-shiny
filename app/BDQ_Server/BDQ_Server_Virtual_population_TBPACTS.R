Pop_generation <- function(input) {
  nsubjects <- input$nsim
  
  # Read in dataset
  myCovSimMICE <- read.csv(paste0(Server.directory, "TBPACTS_Big_Virtual_Population_SimulatedforUse.csv"), 
                        header = T)
  
  # Calculate exact numbers needed for each category
  n_rows_needed <- nsubjects 
  n_females <- round(n_rows_needed * (input$SEX_female/100))
  n_males <- n_rows_needed - n_females
  
  # Calculate exact numbers for race
  n_black <- round(n_rows_needed * (input$popRACE/100))
  n_nonblack <- n_rows_needed - n_black
  
  # Filter and sample females
  females_data <- myCovSimMICE %>%
    filter(
      SEX == 1,  # Female
      AGE >= input$AGE_min & AGE <= input$AGE_max &
        WT >= input$WT_min & WT <= input$WT_max &
        ALB >= input$ALB_min & ALB <= input$ALB_max &
        CACOR >= input$CACOR_min & CACOR <= input$CACOR_max &
        K >= input$K_min & K <= input$K_max &
        MTTP >= input$MTTP_min * 24 & MTTP <= input$MTTP_max * 24
    ) %>%
    slice_head(n = n_females)
  
  # Filter and sample males
  males_data <- myCovSimMICE %>%
    filter(
      SEX == 0,  # Male
      AGE >= input$AGE_min & AGE <= input$AGE_max &
        WT >= input$WT_min & WT <= input$WT_max &
        ALB >= input$ALB_min & ALB <= input$ALB_max &
        CACOR >= input$CACOR_min & CACOR <= input$CACOR_max &
        K >= input$K_min & K <= input$K_max &
        MTTP >= input$MTTP_min * 24 & MTTP <= input$MTTP_max * 24
    ) %>%
    slice_head(n = n_males)
  
  set.seed(3468)
  
  # Combine the datasets
  df_virtualPop <- bind_rows(females_data, males_data) %>%
    # Create sequential ID
    mutate(ID = row_number()) %>%
    # Randomly assign RACE
    mutate(
      RACE = sample(c(rep(1, n_black), rep(0, n_nonblack)))
    )
  
  return(df_virtualPop)
}