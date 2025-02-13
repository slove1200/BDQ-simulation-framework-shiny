# TTP ####
input$REP <- 1
input$STUDY <- "Treatment-naïve" # or Treatment-experienced
input$XDR <- "MDR-TB"
input$MTTP <- 6.8
input$HLEFF <- 100 # bacterial clearance % faster (suggest ranging from -50 to 150)

# User input
num_REPs <- input$REP

# Create dataset for simulation
nsamples    <- input$nsim
num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory

sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)

TTPdf   <- tidyr::crossing(
  ID    = seq(nsamples*num_regimens),
  WEEKP = c(1:sim_time),
  REP   = c(1:num_REPs),
  EVID  = 0,
  AMT   = 0,
  FLAG  = 1,
  TTPD  = c(0, 1:42),
  LASTR = 0) 

TTPdf   <- TTPdf %>%
  mutate(regimen = (ID - 1) %/% nsamples + 1) %>%
  mutate(TIME = seq_along(TTPD) - 1) # dummy time column

TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2, TIME = NA)

TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
  mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID),
         AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT),
         LASTR = ifelse(TTPD == 42, 1, LASTR),
         TASTW = WEEKP) %>%
  mutate(CMT = ifelse(AMT == 1, 1, NA)) %>%
  arrange(ID, WEEKP, REP, TTPD) %>%
  zoo::na.locf()

#### Covariates
# if UI input is to simulate in an individual-level
if (input$population_radio == "Individual") {
  
  # 1. Drug Resistance
  if (input$XDR == "MDR-TB") {
    TTPdf_fin$preAndXDR <- 0
    TTPdf_fin$preXDR <- 0
    TTPdf_fin$XDR <- 0
  } else if (input$XDR == "pre-XDR-TB") {
    TTPdf_fin$preAndXDR <- 1
    TTPdf_fin$preXDR <- 1
    TTPdf_fin$XDR <- 0
  } else {
    TTPdf_fin$preAndXDR <- 1
    TTPdf_fin$preXDR <- 1
    TTPdf_fin$XDR <- 1
  }
  
  # 2. Mean time-to-posistivity (MTTP)
  # input$MTTP unit in days, PK-efficacy model unit in hours
  TTPdf_fin$MTTP <- input$MTTP*24
} else {
  virtual_population_df <- Pop_generation(input)
  # Dynamically bind rows based on `num_regimens`
  virtual_population_df <- map_dfr(1:num_regimens, ~ {
      virtual_population_df %>%
        filter(ID %in% 1:nsamples) %>%
        mutate(regimen  = .x) # Optional: Add a column to indicate duplication
    }) %>% ungroup() %>%
    mutate(ID = row_number())
  
  TTPdf_fin <- full_join(TTPdf_fin, virtual_population_df, by = c("ID", "regimen"))
  
}

TTPdf_fin$HLEFF <- input$HLEFF

dfTTP <- TTPdf_fin

# TTP simulation ########
## Simulation settings
# 2. "simtime" and "simunit"
source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/TTP_power/BDQTTP.R")

if (input$STUDY == "Treatment-naïve") {
  modTTP <- mcode("BDQTTP", codeTTP)
} else {
  modTTP <- mcode("BDQTTP_TrtExperienced", codeTTP_TrtExperienced)
}

## Interindividual variability ON/OFF
if (input$IIV == "OFF") {
  set.seed(3468)
  outTTP <- modTTP %>%
    zero_re() %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * 168, delta = 1) %>%
    as.data.frame()
} else { #### no sigma in TTP
  # Filter and run simulation separately
  outTTP <- map_dfr(1:num_regimens, ~ {
    start_idx <- (.x - 1) * nsamples + 1
    end_idx <- .x * nsamples
    
    # Filter the dataset for the current regimen
    filtered_dfTTP <- dfTTP %>%
      filter(ID %in% start_idx:end_idx)
    
    # Set random seed and run the simulation for the current dataset
    set.seed(3468)
    modTTP %>%
      data_set(filtered_dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  })
}

setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/TTP_Power")
write.csv(outTTP, "HLEFF100_500indv.csv", row.names = FALSE)

# Function to read CSV and calculate proportion_no_scc and median_TSCC
calculate_metrics <- function(file_path) {
  # Read the CSV file
  TTPsim <- read.csv(file_path, header = TRUE)
  
  # Process the data to compute TSCC for each ID
  TSCCdf <- TTPsim %>% filter(RTTE == 1)
  
  df_tscc <- TSCCdf %>%
    group_by(ID) %>%
    arrange(WEEKP) %>%
    mutate(
      converted = (RTTE == 1 & TTPD == 42),
      sustained = rev(cumall(rev(converted)))
    ) %>%
    summarise(
      TSCC = if (any(sustained)) min(WEEKP[sustained]) else NA_real_
    ) %>%
    ungroup()
  
  # Calculate the proportion of patients without SCC across WEEKP
  proportion_no_scc <- TSCCdf %>%
    group_by(WEEKP) %>%
    summarise(
      n_total = n_distinct(ID),
      n_no_scc = n_total - sum(ID %in% df_tscc$ID[!is.na(df_tscc$TSCC) & df_tscc$TSCC <= WEEKP]),
      proportion_no_scc = n_no_scc / n_total
    ) %>%
    ungroup()
  
  # Calculate median TSCC
  crossing_points <- proportion_no_scc %>%
    filter(proportion_no_scc * 100 >= 50) %>%
    slice_tail(n = 1)
  
  prev_point <- proportion_no_scc %>%
    filter(WEEKP < crossing_points$WEEKP) %>%
    slice_tail(n = 1)
  
  x1 <- prev_point$WEEKP
  y1 <- prev_point$proportion_no_scc * 100
  x2 <- crossing_points$WEEKP
  y2 <- crossing_points$proportion_no_scc * 100
  
  median_TSCC <- x1 + (50 - y1) * (x2 - x1) / (y2 - y1)
  
  return(list(proportion_no_scc = proportion_no_scc, median_TSCC = median_TSCC))
}

# Main execution
setwd("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/TTP_Power")
# Calculate individual TSCC and median TSCC
metrics_run1 <- calculate_metrics("HLEFF100_500indv.csv")
metrics_run1$proportion_no_scc$HLEFF <- "100%"
metrics_run2 <- calculate_metrics("HLEFF50_500indv.csv")
metrics_run2$proportion_no_scc$HLEFF <- "50%"


# Combine the proportion_no_scc data from both runs
combined_proportion_no_scc <- rbind(metrics_run1$proportion_no_scc, metrics_run2$proportion_no_scc)

# Function to plot the results with combined data
plot_combined_results <- function(combined_data) {
  ggplot(combined_data, 
         aes(x = WEEKP, y = proportion_no_scc * 100, group = HLEFF, color = HLEFF)) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 1) +
    labs(
      x = "Week after start of treatment",
      y = "Proportion of Patients Without SCC (%)"
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
    scale_color_manual(values = c("#D73027", "#4575B4"), 
                       labels = unique(combined_data$HLEFF)) +  # Use unique HLEFF values for labels
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


# Plot combined results
plot_combined_results(combined_proportion_no_scc)

metrics_run1$median_TSCC
metrics_run2$median_TSCC


#### cross-check with NM ####
# Read the CSV file
TTPsim <- read.csv("HLEFF100_500indv.csv", header = TRUE)

# Process the data to compute TSCC for each ID
TSCCdf <- TTPsim %>% filter(RTTE == 1)


# Read the CSV file
TTPsimNM <- read.csv("Z:/Shiny/TTP_power/run1_extensive.csv", header = TRUE)

# Process the data to compute TSCC for each ID
TSCCdfNM <- TTPsimNM %>% filter(RTTE == 1)

View(TTPsim %>% filter(ID == 1)  %>% select(colnames(TTPsimNM)))
View(TTPsimNM %>% filter(ID == 1))



View(TTPsim %>% filter(ID == 3 & WEEKP == 10) %>% select(c("ID", "TTPD", "FLAG", "NEG", "USUR1", "P1", "ORTTE", "RTTE")))
View(TTPsimNM %>% filter(ID == 3 & WEEKP == 10) %>% select(c("ID", "TIME", "FLAG", "NEG", "ORTTE", "RTTE")))

View(outTTP %>% filter(ID == 3) %>% select(c("ID", "WEEKP", "TTPD", "FLAG", "NEG", "USUR1", "P1", "ORTTE", "RTTE")))
View(TTPsimNM %>% filter(ID == 3) %>% select(c("ID", "WEEKP", "TIME", "FLAG", "NEG", "USUR1", "P1", "ORTTE", "RTTE")))