library(dplyr)
library(survival)
library(mrgsolve)
library(ggplot2)
library(survminer)

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
    def_window_wk <- ceiling(def_window / 7) # convert days to weeks
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
    weeks_needed <- def_window_wk
    
    for (i in 1:n) {
      if (!is_negative[i]) next
      
      current_time <- df$TAST[i]
      target_time <- current_time + weeks_needed
      
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
  if (time_unit == "2") {
    df_tscc <- TSCCdf %>%
      arrange(ID, TAST, REP) %>%
      group_by(ID) %>%
      mutate(sustained = check_sustained(TAST, TTPD, REP, def_window)) %>%
      summarise(TSCC = if (any(sustained)) min(TAST[sustained]) else NA_real_,
                .groups = "drop")
  }
  
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
  
  # merge individual TSCC into the main table
  TSCCdf_mrg <- TSCCdf %>% 
    left_join(df_tscc, by = "ID") %>%
    mutate(TSCC = coalesce(ifelse(TAST == TSCC, 1, 0), 0))
  
  # Return the full results
  return(list(
    TSCCdf = TSCCdf_mrg,
    proportion_no_scc = proportion_no_scc,
    median_TSCC = median_TSCC,
    conversion_times = df_tscc
  ))
}

# TTP ####
input <- c()
input$nsim_TTP <- 10
input$simtime_TTP <- "1,2,3,4,5,6,7,8,9,10,22,23,24"
input$REP_TTP <- 1
input$simunit_TTP <- "2"
input$MTTP_TTP <- 6.8
input$HLEFF_TTP <- -30
input$DEF_TTP <- 28
input$STUDY_TTP <- "Treatment-naïve"


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
         MTTP  = input$MTTP_TTP*24,   
         HLEFF = input$HLEFF_TTP)

source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/TTPsim.R")

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
                                  def_window = input$DEF_TTP)
metrics_run1$proportion_no_scc$HLEFF <- "-50%"
View(metrics_run1$conversion_times)

metrics_run2 <- calculate_metrics(outTTP2, 
                                  time_unit = input$simunit_TTP,   # "1" for days, "2" for weeks
                                  def_window = input$DEF_TTP)
metrics_run2$proportion_no_scc$HLEFF <- "-30%"

metrics_run3 <- calculate_metrics(outTTP3, 
                                  time_unit = input$simunit_TTP,   # "1" for days, "2" for weeks
                                  def_window = input$DEF_TTP)
metrics_run3$proportion_no_scc$HLEFF <- "0%"

#combined_proportion_no_scc <- metrics_run3$proportion_no_scc

# Combine the proportion_no_scc data from both runs
combined_proportion_no_scc <- rbind(metrics_run1$proportion_no_scc, metrics_run2$proportion_no_scc, metrics_run3$proportion_no_scc)
combined_proportion_no_scc$HLEFF <- factor(combined_proportion_no_scc$HLEFF, c("-50%", "-30%", "0%"))

# Function to plot the results with combined data
plot_combined_results <- function(combined_data) {
  ggplot(combined_data, 
         aes(x = TAST, y = prop_without_scc * 100, color = HLEFF, group = HLEFF)) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 1) +
    labs(
      x = "Week After Start of Treatment",
      y = "Proportion of Patients Without SCC (%)"
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
    scale_color_manual(values = c("#D73027", "#4575B4", "#2A9D8F")) +  # Use unique HLEFF values for labels
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

plot_combined_results(combined_proportion_no_scc)
metrics_run1$median_TSCC
metrics_run2$median_TSCC
metrics_run3$median_TSCC


library(survival)
library(survminer)

# Create a data frame for survival analysis
# Process data to compute TSCC for each patient
TSCCdf <- outTTP3 %>% filter(RTTE == 1)

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

survival_data <- df_tscc %>% 
  mutate(status = ifelse(is.na(TSCC), 0, 1)) %>%
  mutate(TSCC = ifelse(is.na(TSCC), 24, TSCC)) %>%
  rename("time" = "TSCC") %>% select(-ID)

# Create a survival object
surv_object <- Surv(time = survival_data$time, event = survival_data$status)

# Fit the Kaplan-Meier model
km_fit <- survfit(surv_object ~ 1, data = survival_data)

# Plot the Kaplan-Meier curve
survminer::ggsurvplot(km_fit, 
           data = survival_data, 
           xlab = "Week After Start of Treatment", 
           ylab = "Proportion of Patients Without SCC", 
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), 
           font.x = c(16),
           font.y = c(16),
           font.tickslab = c(14, "plain"), 
           legend = c("none"), 
           break.x.by = 4, 
           break.y.by = 0.2, 
           surv.median.line = "hv")
