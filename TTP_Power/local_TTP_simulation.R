library(dplyr)
library(mrgsolve)
library(zoo)
library(ggplot2)

# Function to read CSV and calculate proportion_no_scc and median_TSCC
calculate_metrics <- function(output) {
  # Read the CSV file
  TTPsim <- output
  
  # Process data to compute TSCC for each patient
  TSCCdf <- TTPsim %>% filter(RTTE == 1)
  
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
  
  # Simple median calculation (excluding NAs)
  median_tscc <- median(df_tscc$TSCC, na.rm = TRUE)
  
  # Or for more robust survival analysis using survival package:
  library(survival)
  
  # Create survival object
  surv_obj <- Surv(time = df_tscc$TSCC, event = !is.na(df_tscc$TSCC))
  
  # Fit survival model
  km_fit <- survfit(surv_obj ~ 1)
  
  # Get median survival time
  median_TSCC <- km_fit$time[min(which(km_fit$surv <= 0.5))]
  
  
  return(list(proportion_no_scc = proportion_no_scc, median_TSCC = median_TSCC, 
              km_fit = km_fit, survival_data = survival_data))
}

# TTP ####
input <- c()

input$nsim <- 500
input$simtime <- 24 # in weeks
input$simint <- 1   # interval in weeks, consider having a vector to input in WEEK
input$REP <- 2

input$HLEFF <- 0 # Half-life of mycobacterial load % longer (suggest ranging from -90 to 90)
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
input$DEF <- 28 # time-to-sputum conversion definition (default 28 days)
input$MTTP <- 6.8



TTPdf   <- tidyr::crossing(
  ID    = seq(input$nsim),
  WEEKP = seq(1, input$simtime, by = input$simint),
  REP   = c(1:input$REP),
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
  mutate(TIME = seq_along(ID) - 1)

#### Covariates
TTPdf_fin$MTTP <- input$MTTP*24
TTPdf_fin$HLEFF <- input$HLEFF

source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/TTPsim.R")

if (input$STUDY == "Treatment-naïve") {
  modTTP <- mcode("BDQTTP", codeTTP)
  modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)
} else {
  modTTP <- mcode("BDQTTP_TrtExperienced", codeTTP_TrtExperienced)
}

set.seed(3468)

outTTP3 <- modTTP %>%
  data_set(TTPdf_fin) %>%
  mrgsim(end = input$simtime * 168, delta = 24) %>%
  as.data.frame()

# Calculate individual TSCC and median TSCC
metrics_run1 <- calculate_metrics(outTTP)
metrics_run1$proportion_no_scc$HLEFF <- "-50%"

metrics_run2 <- calculate_metrics(outTTP2)
metrics_run2$proportion_no_scc$HLEFF <- "-30%"

metrics_run3 <- calculate_metrics(outTTP3)
metrics_run3$proportion_no_scc$HLEFF <- "0%"

combined_proportion_no_scc <- metrics_run3$proportion_no_scc

# Combine the proportion_no_scc data from both runs
combined_proportion_no_scc <- rbind(metrics_run1$proportion_no_scc, metrics_run2$proportion_no_scc, metrics_run3$proportion_no_scc)
combined_proportion_no_scc$HLEFF <- factor(combined_proportion_no_scc$HLEFF, c("-50%", "-30%", "0%"))

# Function to plot the results with combined data
plot_combined_results <- function(combined_data) {
  ggplot(combined_data, 
         aes(x = WEEKP, y = prop_without_scc * 100, color = HLEFF, group = HLEFF)) +
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
