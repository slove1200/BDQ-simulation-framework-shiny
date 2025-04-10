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
    df <- data.frame(TAST = TAST, TTPD = TTPD, REP = REP) %>%
      group_by(TAST) %>%
      summarise(
        is_negative = all(TTPD == 42),
        .groups = "drop"
      )
    
    n <- nrow(df)
    is_negative <- df$is_negative
    sustained <- rep(FALSE, n)
    max_time <- max(df$TAST)
    time_needed <- def_window
    
    for (i in 1:n) {
      if (!is_negative[i]) next
      
      current_time <- df$TAST[i]
      target_time <- current_time + time_needed
      
      # CRITICAL CHANGE: First check if we have enough follow-up time
      if (target_time > max_time) {
        next
      }
      
      # Find if we have any timepoints at or after target_time
      future_indices <- which(df$TAST >= target_time)
      if (length(future_indices) == 0) {
        next
      }
      
      # Check all intermediate points
      all_future_indices <- which(df$TAST > current_time)
      if (!all(is_negative[all_future_indices])) {
        next
      }
      
      sustained[i] <- TRUE
    }
    
    return(rep(sustained, each = max(REP)))
  }
  
  # Calculate TSCC for each patient
  df_tscc <- TSCCdf %>%
    arrange(ID, TAST, REP) %>%
    group_by(ID) %>%
    mutate(sustained = check_sustained(TAST, TTPD, REP, def_window)) %>%
    summarise(TSCC = if (any(sustained)) min(TAST[sustained]) else NA_real_,
              .groups = "drop")
  
  # Calculate proportion_no_scc for time_unit = "2"
  if (time_unit == "2") {
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
  }  
  
  # merge individual TSCC into the main table
  TSCCdf_mrg <- TSCCdf %>% 
    left_join(df_tscc, by = "ID") %>%
    mutate(TSCC = coalesce(ifelse(TAST == TSCC, 1, 0), 0))
  
  # Return the full results if simulate in days
  if (time_unit == "1") {
  return(list(
    TSCCdf = TSCCdf_mrg,
    proportion_no_scc = NA,
    median_TSCC = NA,
    conversion_times = df_tscc
    ))
  }

  # Return the full results
  return(list(
    TSCCdf = TSCCdf_mrg,
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
      x = paste0("Time after start of treatment (weeks)"),
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
    arrange(ID, TAST, REP, TTPD) %>%
    mutate(TIME = seq_along(ID) - 1,
           # median HL in BDQ-TTP simulation = 0.81
           HLEFF = (input$HLEFF_TTP/0.81-1)*100)
  

  
  # Read in dataset to sample MTTP from
  Server.directory <- "BDQ_Server/"
  
  myCovSimMICE <- read.csv(paste0(Server.directory, "TBPACTS_Big_Virtual_Population_SimulatedforUse.csv"), 
                        header = T) %>% select(ID, MTTP)

  myCovSimMICE_custom <- myCovSimMICE %>% filter(MTTP >= input$MTTP_TTP_min*24 & MTTP <= input$MTTP_TTP_max*24)

  if (input$TTP_distribution_choice == "Default") {
    indvBaselineTTP <- myCovSimMICE %>% filter(ID <= input$nsim_TTP)
  } else {
    indvBaselineTTP <- myCovSimMICE_custom %>% filter(ID <= input$nsim_TTP)
  }

  TTPdf_fin <- TTPdf_fin %>% left_join(indvBaselineTTP, by = "ID")
  
  modTTPsim <- mcode("BDQTTP_sim", codeTTP_sim)
  modTTPsim <- update(modTTPsim, outvars = outvars(modTTPsim)$capture)
  
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
    metrics_run1$proportion_no_scc$HLEFF <-(input$HLEFF_TTP/0.81-1)*100
    combined_proportion_no_scc <- metrics_run1$proportion_no_scc
    
    # Function to plot the results with combined data
    plotTTPsim <- plot_combined_results(combined_proportion_no_scc)
  } else {
    plotTTPsim <- NULL
  }
  
  return(list(plotTTPsim = plotTTPsim, TSCCdf = metrics_run1$TSCCdf))
}