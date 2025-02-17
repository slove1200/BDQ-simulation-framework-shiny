# Look for next timepoint that's at least def_window away
# Check if that timepoint is negative
# Verify no positive cultures between current and next timepoint
# Mark as sustained only if all conditions are met

# Function to calculate proportion_no_scc and median_TSCC
calculate_metrics <- function(output, time_unit = "2", def_window) {
  # time_unit: "1" for days, "2" for weeks
  # def_window: number of days required for conversion definition
  
  # Convert def_window to match time unit if needed
  if (time_unit == "2") { # if weeks
    def_window <- ceiling(def_window / 7) # convert days to weeks
  }
  
  # Process data to compute TSCC for each patient
  TSCCdf <- output %>% filter(RTTE == 1)
  
  # Helper function to check for sustained conversion
  check_sustained <- function(TAST, TTPD, REP, def_window) {
    # First, summarize by timepoint to check if all replicates are negative
    df <- data.frame(TAST = TAST, TTPD = TTPD, REP = REP) %>%
      group_by(TAST) %>%
      summarise(
        is_negative = all(TTPD == 42),  # TRUE only if all replicates are negative
        .groups = "drop"
      )
    
    n <- nrow(df)
    is_negative <- df$is_negative
    sustained <- rep(FALSE, n)
    
    for (i in 1:(n-1)) {  # Only check up to second-to-last point
        if (!is_negative[i]) next # Skip if current point not negative
        
        current_time <- df$TAST[i]
        
        # Find next timepoint that's at least def_window away
        future_indices <- which(df$TAST >= (current_time + def_window/7))  # convert days to weeks
        if (length(future_indices) == 0) next  # No future points far enough
        
        next_time_index <- min(future_indices)
        
        # Check if next qualifying timepoint is negative
        if (is_negative[next_time_index]) {
            # Check if all points between are negative (if any exist)
            between_indices <- which(df$TAST > current_time & df$TAST < df$TAST[next_time_index])
            if (length(between_indices) > 0) {
                if (!all(is_negative[between_indices])) next  # Skip if any positives between
            }
            sustained[i] <- TRUE
        }
    }
    
    # Expand results back to match original data length
    return(rep(sustained, each = max(REP)))
  }
  
  # Calculate TSCC for each patient
  df_tscc <- TSCCdf %>%
    arrange(ID, TAST, REP) %>%
    group_by(ID) %>%
    mutate(sustained = check_sustained(TAST, TTPD, REP, def_window)) %>%
    summarise(TSCC = if (any(sustained)) min(TAST[sustained]) else NA_real_,
              .groups = "drop")
  
  # For time_unit = "1", return only TSCCdf
  if (time_unit == "1") {
    return(list(
      TSCCdf = TSCCdf,
      proportion_no_scc = NA,
      median_TSCC = NA,
      conversion_times = NA
    ))
  }
  
  # Calculate proportion_no_scc for time_unit = "2"
  
  # Get total number of patients and max follow-up time
  total_patients <- nrow(df_tscc)
  max_time <- max(output$TAST)
  
  # Create sequence of timepoints
  timepoints <- seq(0, max_time, by = 1)
  
  # Vectorized calculation of proportion without SCC
  get_prop_not_converted <- function(t, tscc_values) {
    not_converted <- sum(is.na(tscc_values) | tscc_values > t)
    return(not_converted / length(tscc_values))
  }
  
  # Apply calculation to all timepoints
  props <- vapply(timepoints, get_prop_not_converted, numeric(1), df_tscc$TSCC)
  
  # Create proportion_no_scc dataframe
  proportion_no_scc <- data.frame(
    TAST = timepoints,
    prop_without_scc = props,
    n_without_scc = props * total_patients
  )
  attr(proportion_no_scc, "time_unit") <- "weeks"
  
  # Calculate median_TSCC
  if (all(is.na(df_tscc$TSCC))) {
    warning("All TSCC values are NA. No events to analyze.")
    median_TSCC <- NULL
  } else {
    # Create survival object and fit model
    surv_obj <- Surv(time = df_tscc$TSCC, event = !is.na(df_tscc$TSCC))
    km_fit <- survfit(surv_obj ~ 1)
    
    # Get median survival time
    if (length(which(km_fit$surv <= 0.5)) > 0) {
      median_TSCC <- km_fit$time[min(which(km_fit$surv <= 0.5))]
    } else {
      median_TSCC <- NA_real_
    }
    attr(median_TSCC, "time_unit") <- "weeks"
  }
  
  # Return the full results
  return(list(
    TSCCdf = TSCCdf,
    proportion_no_scc = proportion_no_scc,
    median_TSCC = median_TSCC,
    conversion_times = df_tscc
  ))
}

# Function to plot the results with combined data
plot_combined_results <- function(combined_data) {
  ggplot(combined_data, 
         aes(x = TAST, y = prop_without_scc * 100, color = HLEFF, group = HLEFF)) +
    geom_line(size = 1.2, color = "#D73027") +
    geom_point(size = 3, shape = 1, color = "#D73027") +
    labs(
      x = paste0("Time after start of treatment (", ifelse(input$simunit_TTP == "2", "weeks", "days"), ")"),
      y = "Proportion of Patients Without SCC (%)"
    ) +
    # scale_x_continuous(breaks = seq(0, 24, by = 4)) +
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

TTPsimplots <- function(input) {

  TTPdf   <- tidyr::crossing(
    ID    = seq(input$nsim_TTP),
    TAST  = as.numeric(unlist(strsplit(input$simtime_TTP, ","))),
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
           LASTR = ifelse(TTPD == 42, 1, LASTR)) %>%
    mutate(TASTW = 
             if (input$simunit_TTP == "2") {
               TAST  # keep original TAST value
             } else {
               TAST/7  # divide by 7 for all other cases
             }) %>%
    mutate(CMT = ifelse(AMT == 1, 1, 0)) %>%
    arrange(ID, TAST, REP, desc(FLAG), TTPD) %>%
    mutate(TIME = seq_along(ID) - 1, 
           MTTP  = input$MTTP_TTP*24,   
           HLEFF = input$HLEFF_TTP)
  
  
  if (input$STUDY_TTP == "Treatment-naïve") {
    modTTPsim <- mcode("BDQTTP_sim", codeTTP_sim)
    modTTPsim <- update(modTTPsim, outvars = outvars(modTTPsim)$capture)
  } else {
    modTTPsim <- mcode("BDQTTP_TrtExperienced_sim", codeTTP_TrtExperienced_sim)
    modTTPsim <- update(modTTPsim, outvars = outvars(modTTPsim)$capture)
  }
  
  set.seed(3468)
  outTTP <- modTTPsim %>%
    data_set(TTPdf_fin) %>%
    mrgsim(
      end = max(TTPdf_fin$TAST) * (if (input$simunit_TTP == "2") 168 else 24),
      delta = if (input$simunit_TTP == "2") 24 else 1
    ) %>%
    as.data.frame()
  
  # Calculate individual TSCC and median TSCC
  metrics_run1 <- calculate_metrics(outTTP, 
                                    time_unit = input$simunit_TTP,   # "1" for days, "2" for weeks
                                    def_window = input$DEF_TTP)    # e.g., 28 days
  
  # Only create plot data if in weekly mode
  if (input$simunit_TTP == "2") {
    metrics_run1$proportion_no_scc$HLEFF <- input$HLEFF_TTP
    combined_proportion_no_scc <- metrics_run1$proportion_no_scc
    
    # Function to plot the results with combined data
    plotTTPsim <- plot_combined_results(combined_proportion_no_scc)
  } else {
    plotTTPsim <- NULL
  }
  
  return(list(plotTTPsim = plotTTPsim, TSCCdf = metrics_run1$TSCCdf))
}