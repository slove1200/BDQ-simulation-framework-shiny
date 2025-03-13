library(purrr)
library(survival)

# 1. Get PK output from LocalTest_PKSimulation.R ####
# Using 500 individuals from a virtual population with IIV under the approved BDQ regimen 
# Age, Sex, Race, WT, Albumin, basMTTP, TRT-naïve

# 2. TTP simulation under different value of HLEFF, half-life of bacterial load % longer ####
# input$HLEFF: -50, ..., -10, 0, 10, 20, 30, 40, 50, 80, 100
# (suggest ranging from -50 to 100)

# TTP ####
input$REP <- 1
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
#input$MTTP <- 6.8

# User input
num_REPs <- input$REP

# Retrieve information from simulated PK profiles
Cavg_weekly <- out %>% filter(AMT == 0)

####### Use the substituted new data frame d1
Cavg_weekly <- Cavg_weekly %>% filter(time %% 168 == 0)
Cavg_weekly$WEEK <- Cavg_weekly$time / 168
Cavg_weekly <- subset(Cavg_weekly, select = c(ID, time, regimen, WEEK, AAUCBDQ))

# Calculate weekly AUC
Cavg_weekly$AUCWBDQ <- 0

i <- 2
while (i <= length(Cavg_weekly$ID)) {
  Cavg_weekly$AUCWBDQ[i] <- Cavg_weekly$AAUCBDQ[i] - Cavg_weekly$AAUCBDQ[i - 1]
  
  i <- i + 1
}

Cavg_weekly <- Cavg_weekly %>% mutate(
  AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ)
)

###### summarise by
Cavg_weekly <- Cavg_weekly %>%
  group_by(ID, time, regimen, WEEK) %>%
  dplyr::summarise(
    weekly_BDQ = mean(AUCWBDQ, na.rm = T)
  ) %>%
  mutate(weekly_BDQ = 0)

# Create dataset for simulation
input$simtime_TTP <- "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24"
input$REP_TTP <- 1
input$simunit_TTP <- "2"
input$DEF_TTP <- 28
input$STUDY_TTP <- "Treatment-naïve"


TTPdf   <- tidyr::crossing(
  ID    = c(1:500),
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
  mutate(TIME = seq_along(ID) - 1)


dfCAVG <- Cavg_weekly %>% rename("WEEKP" ="WEEK") %>%
  filter(WEEKP != 0) %>%
  mutate(
    AUCW  =  0,         # unit µg*h/mL
    CAVG  =  0) %>% # unit µg/mL
  ungroup() %>% select(ID, WEEKP, AUCW, CAVG)

dfCAVG <- full_join(dfCAVG, Pop_generation(input) %>% select(-STUDYID, -USUBJID), by = c("ID")) %>%
  ungroup() %>% select(-regimen)


# Define the HLEFF values to iterate over
HLEFF_values <- c(-80, -70, -60, -50, -40, -30, -20, -10, 0, 
                  10, 20, 30, 40, 50, 80, 100, 120, 150)

# Calculate TSCC function
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
  
  # Return proportion no scc
  return(list(
    TSCCdf = TSCCdf,
    proportion_no_scc = proportion_no_scc,
    median_TSCC = median_TSCC,
    conversion_times = df_tscc
  ))
}

# Function to run the TTP simulation
run_simulation <- function(HLEFF_value) {
  # Update input$HLEFF
  input$HLEFF <- HLEFF_value
  
  # Run TTP simulation
  TTPdf_fin$HLEFF <- input$HLEFF
  
  dfTTP <- TTPdf_fin %>% full_join(dfCAVG %>% rename("TASTW" = "WEEKP"), by = c("ID", "TASTW"))
  
  modTTP <- mcode("BDQTTP", codeTTP_HLeffPlot)
  
  set.seed(3468)
  
  if (input$IIV == "OFF") {
    outTTP <- modTTP %>%
      zero_re() %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  } else {
    outTTP <- modTTP %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  }
  
  metrics_run <- calculate_metrics(outTTP, "2", 28)
  
  return(metrics_run$proportion_no_scc)
}

# Run the simulation for all HLEFF values and store results
source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_LocalTest/BDQTTP_HLeff.R")
outTSCC_results <- lapply(HLEFF_values, run_simulation)

# Combine all results into one dataframe
final_results <- bind_rows(outTSCC_results)


summary_HLeff2 <- final_results %>% filter(TAST == 8 | TAST == 24) %>% 
  mutate(HLeffNum = rep(HLEFF_values, each = 2)) %>%
  group_by(TAST, HLeffNum) %>%
  mutate(time_label = factor(TAST,
                             levels = c(8, 24),
                             labels = c("Month 2", "Month 6")))

summary_HLeff2$HL_value <- round((1+summary_HLeff2$HLeffNum/100) * 0.811166, 2)

# plot
p1 <- ggplot() +
  geom_line(data = summary_HLeff2, 
            aes(x = HL_value, y = (1-prop_without_scc) * 100, 
                color = time_label,
                group = TAST),
            size = 1.2) +
  # Add reference lines and their labels
  geom_text(data = data.frame(time_label = "Month 2", 
                              y = 31, 
                              x = 0.81444),  
            aes(x = x, y = y),
            label = "Month 2: 30.6%",
            color = "darkred",
            hjust = -0.1,  # adjust horizontal position
            vjust = 0,  # adjust vertical position
            size = 7) +
  geom_text(data = data.frame(time_label = "Month 6", 
                              y = 78, 
                              x = 0.81444),  
            aes(x = x, y = y),
            label = "Month 6: 78.2%",
            color = "darkblue",
            hjust = -0.1,  # adjust horizontal position
            vjust = -0.5,  # adjust vertical position
            size = 7) +
  geom_vline(xintercept = 0.81444, 
             color = "#666666", 
             size = 1, 
             linetype = "dashed") +
  theme_bw() +
  labs(x = "Half-life of mycobacterial load decline (weeks)", 
       y = "Conversion rate (%)", 
       color = NULL) +
  ggtitle("Conversion rate over half-life of mycobacterial load decline") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15),
    # legend.text = element_text(size = 14),
    # strip.text = element_text(size = 18),
    legend.position = "none"
    # legend.position = c(.10, .10),
    # legend.box.just = "left",
    # legend.margin = margin(3, 3, 3, 3), 
    # legend.key = element_rect(color = "transparent", fill = "transparent"), 
    # legend.key.height = unit(1, "cm")
  ) +
  scale_color_manual(values = c("Month 2" = "#c1121f",  # lighter red
                                "Month 6" = "#023e8a")) +  # lighter blue
  scale_x_continuous(breaks = seq(0.1, 2.1, by = 0.1), limits = c(0.15, 2.05), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(10, 100, by = 10)
  )

p1

# 
# setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_LocalTest")
# write.csv(summary_HLeff, "summary_HLeff.csv", row.names = F)

png.filename <- paste0('HLEFF_halfLife_TSCC_PCB.png')
png(units = 'mm',res=1000,filename = png.filename,width = 200,height = 150)

p1
dev.off()

