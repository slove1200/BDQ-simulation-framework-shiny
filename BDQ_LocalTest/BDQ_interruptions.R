library(tibble)
library(dplyr)

# Function to create a scenario dataframe
create_scenario <- function(scenario_num, weeks_on_treatment, weeks_since_last_bdq, dosing_strategy) {
  time_offset <- 168*weeks_on_treatment + 168 * weeks_since_last_bdq
  
  if (weeks_on_treatment == 24) {
    
    if (dosing_strategy == "500 mg daily × 2 wks + 200 mg daily × 6 wks") {
      time <- c(0, 336, 384, 432, time_offset, time_offset + 336)
      amt <- c(400, 200, 200, 200, 500, 200)
      ii <- c(24, 168, 168, 168, 24, 24)
      addl <- c(13, 21, 21, 21, 13, 41)
    } else { # "200 mg daily (24 weeks)"
      time <- c(0, 336, 384, 432, time_offset)
      amt <- c(400, 200, 200, 200, 200)
      ii <- c(24, 168, 168, 168, 24)
      addl <- c(13, 21, 21, 21, 55)
    }
    
  } else {
    
    if (dosing_strategy == "500 mg daily × 2 wks + 200 mg daily × 6 wks") {
      time <- c(0, 336, 384, 432, time_offset, time_offset + 336)
      amt <- c(400, 200, 200, 200, 500, 200)
      ii <- c(24, 168, 168, 168, 24, 24)
      addl <- c(13, 33, 33, 33, 13, 41)
    } else { # "200 mg daily (24 weeks)"
      time <- c(0, 336, 384, 432, time_offset)
      amt <- c(400, 200, 200, 200, 200)
      ii <- c(24, 168, 168, 168, 24)
      addl <- c(13, 33, 33, 33, 55)
    }
    
  }
  
  tibble(
    ID = 1,
    time = time,
    amt = amt,
    ii = ii,
    addl = addl,
    cmt = 1,
    evid = 1,
    THETA25 = 1,
    THETA26 = 1,
    regimen = scenario_num
  )
}

# Create the scenarios with updated week-based offset
scenarios <- bind_rows(
  create_scenario(1, 24, 8, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 1),  # 2 months = 8 weeks
  create_scenario(2, 36, 8, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 2),  # 2 months = 8 weeks
  create_scenario(3, 24, 16, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 3), # 4 months = 16 weeks
  create_scenario(4, 36, 16, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 4), # 4 months = 16 weeks
  create_scenario(5, 24, 24, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 5), # 6 months = 24 weeks
  create_scenario(6, 36, 24, "500 mg daily × 2 wks + 200 mg daily × 6 wks") %>% mutate(ID = 6), # 6 months = 24 weeks
  create_scenario(7, 24, 8, "200 mg daily × 8 wks") %>% mutate(ID = 7),                 # 2 months = 8 weeks
  create_scenario(8, 36, 8, "200 mg daily × 8 wks") %>% mutate(ID = 8),                 # 2 months = 8 weeks
  create_scenario(9, 24, 16, "200 mg daily × 8 wks") %>% mutate(ID = 9),                # 4 months = 16 weeks
  create_scenario(10, 36, 16, "200 mg daily × 8 wks") %>% mutate(ID = 10),                # 4 months = 16 weeks
  create_scenario(11, 24, 24, "200 mg daily × 8 wks")  %>% mutate(ID = 11),               # 6 months = 24 weeks
  create_scenario(12, 36, 24, "200 mg daily × 8 wks")  %>% mutate(ID = 12)               # 6 months = 24 weeks
)

# Print combined dataframe
print(scenarios)

############# PK simulation  #####
# Load mrgsolve model
source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/BDQOMAT.R")
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

nsamples <- 12
sim_time <- 96
num_regimens <- 1

# Run simulation
# Filter and run simulation separately
out <- map_dfr(1:num_regimens, ~ {
  start_idx <- (.x - 1) * nsamples + 1
  end_idx <- .x * nsamples
  
  # Filter the dataset for the current regimen
  filtered_df <- scenarios %>%
    filter(ID %in% start_idx:end_idx)
  
  # Set random seed and run the simulation for the current dataset
  set.seed(3468)
  PKSimulation(input$IIV, mod, filtered_df, sim_time)
})


# Filter and add the WEEK column
Cavg_weekly <- out %>%
  filter(time %% 168 == 0) %>%
  mutate(WEEK = time / 168)

# Subset relevant columns
Cavg_weekly <- Cavg_weekly %>% select(ID, time, regimen, WEEK, AAUCBDQ, AAUCM2)

# Calculate weekly AUC
Cavg_weekly$AUCWBDQ <- 0
Cavg_weekly$AUCWM2 <- 0

# Calculate daily AUC using the `lag` function for vectorized operations
Cavg_weekly <- Cavg_weekly %>%
  mutate(
    AUCWBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
    AUCWM2  = AAUCM2 - lag(AAUCM2, default = 0)
  )

# Ensure AUC values are zero for `time == 0` as required
Cavg_weekly <- Cavg_weekly %>%
  mutate(
    AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
    AUCWM2  = ifelse(time == 0, 0, AUCWM2)
  )

###### summarise by
dfForPlot_CavgW <- Cavg_weekly %>%
  group_by(time, regimen, WEEK) %>%
  dplyr::summarise(
    lower_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.05),
    median_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.5),
    upper_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.95),
    lower_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.05),
    median_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.5),
    upper_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.95),
  )   %>%
  mutate(month_on_treatment = ifelse(regimen %in% c(1,3,5,7,9,11), "6 months", "9 months"), 
         restart_bdq_dose   = ifelse(regimen %in% c(1:6), "500 mg daily × 2 wks + 200 mg daily × 6 wks", "200 mg daily × 8 wks"))


# Set dynamic ylim BDQ
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
  ylimitsBDQ <- maxBDQ
} else {
  q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
  max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
  ylimitsBDQ <- q97BDQ + max15BDQ
}

a1 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "6 months"),
             aes(x = time / 168, y = median_CavgWBDQ, 
                 color = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")), 
                 fill = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")))) +
  geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
  ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ), 
                  xlim = c(0,72)) +  
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8)))+
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) +
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )

# Set dynamic ylim M2
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
  ylimitsM2 <- maxM2
} else {
  q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
  max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
  ylimitsM2 <- q97M2 + max15M2
}

a2 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "6 months"), 
             aes(x = time / 168, y = median_CavgWM2, 
                 color = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")), 
                 fill = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")))) +
  geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = "Weekly Average M2 concentration (ng/mL)") +
  ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2), xlim = c(0, 72)) +
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8))) +
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18), 
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14), 
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) + 
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )


plot2 <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")

plot2



####### 9 months ###############

# Set dynamic ylim BDQ
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
  ylimitsBDQ <- maxBDQ
} else {
  q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
  max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
  ylimitsBDQ <- q97BDQ + max15BDQ
}

a3 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "9 months"),
             aes(x = time / 168, y = median_CavgWBDQ, 
                 color = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")), 
                 fill = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")))) +
  geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
  ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ), 
                  xlim = c(0,72)) +  
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8)))+
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) +
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )

# Set dynamic ylim M2
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
  ylimitsM2 <- maxM2
} else {
  q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
  max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
  ylimitsM2 <- q97M2 + max15M2
}

a4 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "9 months"), 
             aes(x = time / 168, y = median_CavgWM2, 
                 color = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")), 
                 fill = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")))) +
  geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = "Weekly Average M2 concentration (ng/mL)") +
  ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2), xlim = c(0, 72)) +
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8))) +
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 2wks + 200 QD * 6wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18), 
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14), 
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) + 
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )


plot4 <- ggarrange(a3, a4, ncol = 2, common.legend = TRUE, legend = "bottom")

plot4




#### alternative ####
library(tibble)
library(dplyr)

# Function to create a scenario dataframe
create_scenario <- function(scenario_num, weeks_on_treatment, weeks_since_last_bdq, dosing_strategy) {
  time_offset <- 168*weeks_on_treatment + 168 * weeks_since_last_bdq
  
  if (weeks_on_treatment == 24) {
    
    if (dosing_strategy == "500 mg daily × 3 days + 200 mg daily until 8wks") {
      time <- c(0, 336, 384, 432, time_offset, time_offset + 24*3)
      amt <- c(400, 200, 200, 200, 500, 200)
      ii <- c(24, 168, 168, 168, 24, 24)
      addl <- c(13, 21, 21, 21, 2, 53)
    } else { # "200 mg daily (24 weeks)"
      time <- c(0, 336, 384, 432, time_offset)
      amt <- c(400, 200, 200, 200, 200)
      ii <- c(24, 168, 168, 168, 24)
      addl <- c(13, 21, 21, 21, 55)
    }
    
  } else {
    
    if (dosing_strategy == "500 mg daily × 3 days + 200 mg daily until 8wks") {
      time <- c(0, 336, 384, 432, time_offset, time_offset + 24*3)
      amt <- c(400, 200, 200, 200, 500, 200)
      ii <- c(24, 168, 168, 168, 24, 24)
      addl <- c(13, 33, 33, 33, 2, 53)
    } else { # "200 mg daily (24 weeks)"
      time <- c(0, 336, 384, 432, time_offset)
      amt <- c(400, 200, 200, 200, 200)
      ii <- c(24, 168, 168, 168, 24)
      addl <- c(13, 33, 33, 33, 55)
    }
    
  }
  
  tibble(
    ID = 1,
    time = time,
    amt = amt,
    ii = ii,
    addl = addl,
    cmt = 1,
    evid = 1,
    THETA25 = 1,
    THETA26 = 1,
    regimen = scenario_num
  )
}

# Create the scenarios with updated week-based offset
scenarios <- bind_rows(
  create_scenario(1, 24, 8, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 1),  # 2 months = 8 weeks
  create_scenario(2, 36, 8, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 2),  # 2 months = 8 weeks
  create_scenario(3, 24, 16, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 3), # 4 months = 16 weeks
  create_scenario(4, 36, 16, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 4), # 4 months = 16 weeks
  create_scenario(5, 24, 24, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 5), # 6 months = 24 weeks
  create_scenario(6, 36, 24, "500 mg daily × 3 days + 200 mg daily until 8wks") %>% mutate(ID = 6), # 6 months = 24 weeks
  create_scenario(7, 24, 8, "200 mg daily × 8 wks") %>% mutate(ID = 7),                 # 2 months = 8 weeks
  create_scenario(8, 36, 8, "200 mg daily × 8 wks") %>% mutate(ID = 8),                 # 2 months = 8 weeks
  create_scenario(9, 24, 16, "200 mg daily × 8 wks") %>% mutate(ID = 9),                # 4 months = 16 weeks
  create_scenario(10, 36, 16, "200 mg daily × 8 wks") %>% mutate(ID = 10),                # 4 months = 16 weeks
  create_scenario(11, 24, 24, "200 mg daily × 8 wks")  %>% mutate(ID = 11),               # 6 months = 24 weeks
  create_scenario(12, 36, 24, "200 mg daily × 8 wks")  %>% mutate(ID = 12)               # 6 months = 24 weeks
)

# Print combined dataframe
print(scenarios)

############# PK simulation  #####
# Load mrgsolve model
source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/BDQOMAT.R")
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

nsamples <- 12
sim_time <- 96
num_regimens <- 1

# Run simulation
# Filter and run simulation separately
out <- map_dfr(1:num_regimens, ~ {
  start_idx <- (.x - 1) * nsamples + 1
  end_idx <- .x * nsamples
  
  # Filter the dataset for the current regimen
  filtered_df <- scenarios %>%
    filter(ID %in% start_idx:end_idx)
  
  # Set random seed and run the simulation for the current dataset
  set.seed(3468)
  PKSimulation(input$IIV, mod, filtered_df, sim_time)
})


# Filter and add the WEEK column
Cavg_weekly <- out %>%
  filter(time %% 168 == 0) %>%
  mutate(WEEK = time / 168)

# Subset relevant columns
Cavg_weekly <- Cavg_weekly %>% select(ID, time, regimen, WEEK, AAUCBDQ, AAUCM2)

# Calculate weekly AUC
Cavg_weekly$AUCWBDQ <- 0
Cavg_weekly$AUCWM2 <- 0

# Calculate daily AUC using the `lag` function for vectorized operations
Cavg_weekly <- Cavg_weekly %>%
  mutate(
    AUCWBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
    AUCWM2  = AAUCM2 - lag(AAUCM2, default = 0)
  )

# Ensure AUC values are zero for `time == 0` as required
Cavg_weekly <- Cavg_weekly %>%
  mutate(
    AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
    AUCWM2  = ifelse(time == 0, 0, AUCWM2)
  )

###### summarise by
dfForPlot_CavgW <- Cavg_weekly %>%
  group_by(time, regimen, WEEK) %>%
  dplyr::summarise(
    lower_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.05),
    median_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.5),
    upper_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.95),
    lower_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.05),
    median_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.5),
    upper_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.95),
  )   %>%
  mutate(month_on_treatment = ifelse(regimen %in% c(1,3,5,7,9,11), "6 months", "9 months"), 
         restart_bdq_dose   = ifelse(regimen %in% c(1:6), "500 mg daily × 3 days + 200 mg daily until  8wks", "200 mg daily × 8 wks"))


# Set dynamic ylim BDQ
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
  ylimitsBDQ <- maxBDQ
} else {
  q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
  max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
  ylimitsBDQ <- q97BDQ + max15BDQ
}

a1 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "6 months"),
             aes(x = time / 168, y = median_CavgWBDQ, 
                 color = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")), 
                 fill = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")))) +
  geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
  ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ), 
                  xlim = c(0,72)) +  
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8)))+
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) +
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )

# Set dynamic ylim M2
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
  ylimitsM2 <- maxM2
} else {
  q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
  max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
  ylimitsM2 <- q97M2 + max15M2
}

a2 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "6 months"), 
             aes(x = time / 168, y = median_CavgWM2, 
                 color = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")), 
                 fill = factor(regimen, levels = c("1", "7", "3", "9", "5", "11")))) +
  geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = "Weekly Average M2 concentration (ng/mL)") +
  ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2), xlim = c(0, 72)) +
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8))) +
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18), 
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14), 
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) + 
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )


plot2 <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")

plot2



####### 9 months ###############

# Set dynamic ylim BDQ
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
  ylimitsBDQ <- maxBDQ
} else {
  q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
  max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
  ylimitsBDQ <- q97BDQ + max15BDQ
}

a3 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "9 months"),
             aes(x = time / 168, y = median_CavgWBDQ, 
                 color = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")), 
                 fill = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")))) +
  geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
  ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ), 
                  xlim = c(0,72)) +  
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8)))+
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) +
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )

# Set dynamic ylim M2
if (input$nsim == 1 || input$IIV == "OFF") { # individual
  maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
  ylimitsM2 <- maxM2
} else {
  q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
  max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
  ylimitsM2 <- q97M2 + max15M2
}

a4 <- ggplot(dfForPlot_CavgW %>% filter(month_on_treatment == "9 months"), 
             aes(x = time / 168, y = median_CavgWM2, 
                 color = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")), 
                 fill = factor(regimen, levels = c("2", "8", "4", "10", "6", "12")))) +
  geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1.2) +
  theme_bw() +
  labs(x = "Time (weeks)", y = "Weekly Average M2 concentration (ng/mL)") +
  ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2), xlim = c(0, 72)) +
  scale_x_continuous(breaks = c(0, seq(8, 72, by = 8))) +
  scale_color_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  scale_fill_manual(
    values = c("#A084B5", "#A084B58D", "#D65D61", "#D65D618D", "#44BE5F", "#44BE5F8D"), 
    labels = c(
      "Last BDQ 2 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 2 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 4 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 4 months ago, reinitiation 200 QD * 8wks",
      "Last BDQ 6 months ago, reinitiation 500 QD * 3 days + 200 QD until 8wks",
      "Last BDQ 6 months ago, reinitiation 200 QD * 8wks"
    )
  ) +
  theme(
    plot.title = element_text(size = 18), 
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14), 
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 16)
  ) + 
  guides(
    color = guide_legend("Scenario", nrow = 2),
    fill  = guide_legend("Scenario", nrow = 2)
  )


plot4 <- ggarrange(a3, a4, ncol = 2, common.legend = TRUE, legend = "bottom")

plot4


