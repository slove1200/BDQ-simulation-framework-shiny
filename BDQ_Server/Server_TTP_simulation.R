# Function to calculate proportion_no_scc and median_TSCC
calculate_metrics <- function(output) {
  
  # Process data to compute TSCC for each patient
  TSCCdf <- output %>% filter(RTTE == 1)
  
  # Identify all-negative weeks
  df_tscc <- TSCCdf %>%
    group_by(ID, WEEKP) %>%
    summarise(
      all_negative = all(TTPD == 42),  # All cultures must be negative
      .groups = "drop"
    ) %>%
    arrange(ID, WEEKP) %>%
    group_by(ID) %>%
    mutate(
      # Check if all next 4 weeks are also negative
      future_4_weeks_negative = sapply(seq_along(WEEKP), function(i) {
        all(all_negative[i:min(i+3, length(all_negative))])  # Ensure 4-week window is negative
      }),
      
      # First week that meets the conversion criteria
      conversion_possible = all_negative & future_4_weeks_negative,
      
      # Ensure sustained conversion
      sustained = rev(cumall(rev(conversion_possible)))
    ) %>%
    summarise(
      TSCC = if (any(sustained)) min(WEEKP[sustained]) else NA_real_,
      .groups = "drop"
    )
  
  # Function to calculate proportion without SCC over time
  # Get total number of patients
  total_patients <- nrow(df_tscc)
  
  # Get maximum follow-up week
  max_week <- max(output$WEEKP)
  
  # Create sequence of weeks
  weeks <- seq(0, max_week, by = 1)
  
  # Calculate proportions without SCC at each week
  survival_data <- sapply(weeks, function(week) {
    # Count patients who haven't converted by this week
    # (either NA or TSCC > week)
    not_converted <- sum(is.na(df_tscc$TSCC) | df_tscc$TSCC > week)
    prop_not_converted <- not_converted / total_patients
    return(prop_not_converted)
  })
  
  # Calculate proportion of patients without SCC across WEEKP
  proportion_no_scc <- data.frame(
    WEEKP = weeks,
    prop_without_scc = survival_data,
    n_without_scc = survival_data * total_patients
  )
  
  # Create survival object
  surv_obj <- Surv(time = df_tscc$TSCC, event = !is.na(df_tscc$TSCC))
  
  # Fit survival model
  km_fit <- survfit(surv_obj ~ 1)
  
  # Get median survival time
  median_TSCC <- km_fit$time[min(which(km_fit$surv <= 0.5))]
  
  return(list(proportion_no_scc = proportion_no_scc, median_TSCC = median_TSCC))
}

TTPsimplots <- function(input) {

  TTPdf   <- tidyr::crossing(
    ID    = seq(input$nsim_TTP),
    WEEKP = seq(1, input$simtime_TTP, by = input$simint_TTP),
    REP   = c(1:input$REP_TTP),
    EVID  = 0,
    AMT   = 0,
    FLAG  = 1,
    TTPD  = c(0, 1:42),
    LASTR = 0) 
  
  TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2)
  
  TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
    mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID),
           AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT),
           LASTR = ifelse(TTPD == 42, 1, LASTR),
           TASTW = WEEKP) %>%
    mutate(CMT = ifelse(AMT == 1, 1, 0)) %>%
    arrange(ID, WEEKP, REP, desc(FLAG), TTPD) %>%
    mutate(TIME = seq_along(ID) - 1, 
           MTTP  = input$MTTP_TTP*24, 
           HLEFF = input$HLEFF_TTP)
  

  if (input$STUDY_TTP == "Treatment-naïve") {
    modTTPsim <- mcode("BDQTTP_sim", codeTTP_sim)
    modTTPsim <- update(modTTPsim, outvars = outvars(modTTPsim)$capture)
  } else {
    modTTPsim <- mcode("BDQTTP_TrtExperienced_sim", codeTTP_TrtExperienced)
  }
  
  set.seed(3468)
  
  outTTP <- modTTPsim %>%
    data_set(TTPdf_fin) %>%
    mrgsim(end = input$simtime_TTP * 168, delta = 24) %>%
    as.data.frame()
  
  # Calculate individual TSCC and median TSCC
  metrics_run1 <- calculate_metrics(outTTP)
  metrics_run1$proportion_no_scc$HLEFF <- input$HLEFF_TTP
  
  combined_proportion_no_scc <- metrics_run1$proportion_no_scc
  
  # # Combine the proportion_no_scc data from both runs
  # combined_proportion_no_scc <- rbind(metrics_run1$proportion_no_scc, metrics_run2$proportion_no_scc, metrics_run3$proportion_no_scc)
  # combined_proportion_no_scc$HLEFF <- factor(combined_proportion_no_scc$HLEFF, c("-50%", "-30%", "0%"))
  
  # Function to plot the results with combined data
  plot_combined_results <- function(combined_data) {
    ggplot(combined_data, 
           aes(x = WEEKP, y = prop_without_scc * 100, color = HLEFF, group = HLEFF)) +
      geom_line(size = 1.2, color = "#D73027") +
      geom_point(size = 3, shape = 1, color = "#D73027") +
      labs(
        x = "Week after start of treatment",
        y = "Proportion of Patients Without SCC (%)"
      ) +
      scale_x_continuous(breaks = seq(0, 24, by = 4)) +
      scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
      #scale_color_discrete(values = c("#D73027", "#4575B4", "#2A9D8F")) +  # Use unique HLEFF values for labels
      theme_bw() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.position = c(.80, .99),
        legend.justification = c("left", "top")
      ) 
  }
  
  plotTTPsim <- plot_combined_results(combined_proportion_no_scc)
  
  return(plotTTPsim)
}