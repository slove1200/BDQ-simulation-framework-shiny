PKDavg_data <- function(input, sim_PKtable) {
  # Filter and add the DAY column
  Cavg_daily <- sim_PKtable %>% 
    filter(time %% 24 == 0) %>%
    mutate(DAY = time / 24)
  
  # Subset relevant columns
  Cavg_daily <- Cavg_daily %>% select(ID, time, regimen, DAY, AAUCBDQ, AAUCM2)
  
  # Calculate daily AUC using the `lag` function for vectorized operations
  Cavg_daily <- Cavg_daily %>%
    mutate(
      AUCDBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
      AUCDM2  = AAUCM2 - lag(AAUCM2, default = 0)
    )
  
  # Ensure AUC values are zero for `time == 0` as required
  Cavg_daily <- Cavg_daily %>%
    mutate(
      AUCDBDQ = ifelse(time == 0, 1e-6, AUCDBDQ),
      AUCDM2  = ifelse(time == 0, 1e-6, AUCDM2)
    )

  return(Cavg_daily)
}

PKWavg_data <- function(input, sim_PKtable) {
  # Filter and add the WEEK column
  Cavg_weekly <- sim_PKtable %>%
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
      AUCWBDQ = ifelse(time == 0, 1e-6, AUCWBDQ),
      AUCWM2  = ifelse(time == 0, 1e-6, AUCWM2)
    )
  
  return(Cavg_weekly)
}

PKDavg_plots <- function(input, Cavg_daily) {
  ###### summarise by
  dfForPlot_CavgD <- Cavg_daily %>%
    group_by(time, regimen, DAY) %>%
    dplyr::summarise(
      lower_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.05),
      median_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.5),
      upper_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.95),
      lower_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.05),
      median_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.5),
      upper_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.95),
    )
  
  # Set dynamic ylim BDQ
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxBDQ <- max(Cavg_daily$AUCDBDQ)/24*1000
    ylimitsBDQ <- maxBDQ 
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonBDQ <- max(dfForPlot_CavgD$upper_CavgDBDQ, na.rm = TRUE)
    ylimitsBDQ <- maxRibbonBDQ 
  }
  
  if (input$simtime > 48) {
    xbreaks <- seq(0, input$simtime, by = 8)
  } else {
    xbreaks <- seq(0, input$simtime, by = 4)
  }
  
  # Set dynamic ylim M2
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxM2 <- max(Cavg_daily$AUCDM2)/24*1000
    ylimitsM2 <- maxM2 
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonM2 <- max(dfForPlot_CavgD$upper_CavgDM2, na.rm = TRUE)
    ylimitsM2 <- maxRibbonM2 
  }
  
  a1 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDBDQ, 
                                 color = as.factor(regimen), 
                                 fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgDBDQ, ymax = upper_CavgDBDQ), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Daily Average BDQ concentration (ng/mL)")) +
    ggtitle("Daily Average BDQ Concentration (ng/mL) vs Time") +
    (if (input$PK_log == "Log scale") scale_y_log10(limits = c(100, 10000)) else coord_cartesian(ylim = c(0, ylimitsBDQ))) +
    coord_cartesian(xlim = c(0, input$simtime)) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    theme(
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16)
    ) + 
    guides(color = guide_legend("Regimen"),
           fill  = guide_legend("Regimen"))

  a2 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDM2, 
                                color = as.factor(regimen), 
                                fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgDM2, ymax = upper_CavgDM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Daily Average M2 concentration (ng/mL)")) +
    ggtitle("Daily Average M2 Concentration (ng/mL) vs Time") +
    (if (input$PK_log == "Log scale") scale_y_log10(limits = c(10, 1000)) else coord_cartesian(ylim = c(0, ylimitsM2))) +
    coord_cartesian(xlim = c(0, input$simtime)) +
    scale_x_continuous(breaks = xbreaks) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    theme(
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16)
    ) + 
    guides(color = guide_legend("Regimen"),
           fill  = guide_legend("Regimen"))
  
  plot <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")
  
  return(plot)
}


# Weekly average concentration
PKWavg_plots <- function(input, Cavg_weekly) {
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
    )
  
  # Set dynamic ylim BDQ
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
    ylimitsBDQ <- maxBDQ * 1.03
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonBDQ <- max(dfForPlot_CavgW$upper_CavgWBDQ, na.rm = TRUE)
    ylimitsBDQ <- maxRibbonBDQ * 1.03
  }
  
  if (input$simtime > 48) {
    xbreaks <- seq(0, input$simtime, by = 8)
  } else {
    xbreaks <- seq(0, input$simtime, by = 4)
  }
  
  a1 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWBDQ, 
                                    color = as.factor(regimen), 
                                    fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
    ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
    (if (input$PK_log == "Log scale") scale_y_log10(limits = c(100, 10000)) else coord_cartesian(ylim = c(0, ylimitsBDQ))) +
    coord_cartesian(xlim = c(0, input$simtime)) +
    scale_x_continuous(breaks = xbreaks) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    theme(
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16)
    ) + 
    guides(color = guide_legend("Regimen"),
           fill  = guide_legend("Regimen"))
  
  # Set dynamic ylim M2
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
    ylimitsM2 <- maxM2 * 1.03
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonM2 <- max(dfForPlot_CavgW$upper_CavgWM2, na.rm = TRUE)
    ylimitsM2 <- maxRibbonM2 * 1.03
  }
  
  a2 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWM2, 
                                    color = as.factor(regimen), 
                                    fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Weekly Average M2 concentration (ng/mL)")) +
    ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
    (if (input$PK_log == "Log scale") scale_y_log10(limits = c(10, 1000)) else coord_cartesian(ylim = c(0, ylimitsM2))) +
    coord_cartesian(xlim = c(0, input$simtime)) +
    scale_x_continuous(breaks = xbreaks) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    theme(
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16)
    ) + 
    guides(color = guide_legend("Regimen"),
           fill  = guide_legend("Regimen"))
  
  plot <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")
  
  return(plot)
}