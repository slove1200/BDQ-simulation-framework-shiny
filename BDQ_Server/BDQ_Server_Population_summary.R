Virtual_population_summary <- function(df_virtualPop, input) {
  
  if(input$population_radio == "Individual") {
    # Create a single-row dataframe for individual case
    df_virtualPop <- data.frame(
      AGE = input$AGE,
      WT = input$WT,
      SEX = ifelse(input$SEX == "Female", 1, 0),
      RACE = ifelse(input$RACE == "Black", 1, 0),
      ALB = input$ALB,
      CACOR = input$CACOR,
      K = input$K,
      MTTP = input$MTTP * 24,  # Convert days to hours
      TBTYPE = case_when(
        input$XDR == "MDR-TB" ~ 2,
        input$XDR == "pre-XDR-TB" ~ 3,
        input$XDR == "XDR-TB" ~ 4
      )
    )
}

  # MTTP in hours to days first
  df_virtualPop$MTTPd <- df_virtualPop$MTTP/24
  
  # Function to calculate median and range for continuous variables
  calc_median_range <- function(x) {
    med <- round(median(x), 2)
    rng <- round(range(x), 2)
    paste0(med, " (", rng[1], "-", rng[2], ")")
  }
  
  # Function to calculate percentage for categorical variables
  calc_percentage <- function(x, value) {
    n <- sum(x == value)
    pct <- round(n / length(x) * 100, 1)
    paste0(n, " (", pct, "%)")
  }
  
  # Create data frame with both columns at once
  summary_table <- data.frame(
    Variable = c(
      "<strong>Patient Characteristics</strong>",
      "Age (years)", 
      "Weight (kg)",
      "Sex (female)", 
      "Race (black)",
      "<strong>Laboratory Values</strong>",
      "Albumin (g/dL)",
      "Corrected calcium level (mmol/L)",
      "Potassium level (mmol/L)",
      "Baseline time-to-positivity (days)",
      "<strong>Drug Resistance Profile</strong>",
      "DS or MDR-TB",
      "pre-XDR-TB",
      "XDR-TB"
    ),
    Value = c(
      "", 
      calc_median_range(df_virtualPop$AGE),
      calc_median_range(df_virtualPop$WT),
      calc_percentage(df_virtualPop$SEX, 1),
      calc_percentage(df_virtualPop$RACE, 1),
      "", 
      calc_median_range(df_virtualPop$ALB),
      calc_median_range(df_virtualPop$CACOR),
      calc_median_range(df_virtualPop$K),
      calc_median_range(df_virtualPop$MTTPd),
      "", 
      calc_percentage(df_virtualPop$TBTYPE, 2),
      calc_percentage(df_virtualPop$TBTYPE, 3),
      calc_percentage(df_virtualPop$TBTYPE, 4)
    ),
    stringsAsFactors = FALSE
  )
  
  return(summary_table)
}