PKDavg_plots <- function(input, sim_PKtable) {
  
  Cavg_daily <- sim_PKtable %>% 
    filter(time %% 24 == 0) %>%
    mutate(DAY = time/24)
  
  Cavg_daily <- subset(Cavg_daily, select = c(ID, time, regimen, DAY, AAUCBDQ, AAUCM2))
  
  # Calculate daily AUC
  Cavg_daily$AUCDBDQ <- 0
  Cavg_daily$AUCDM2 <- 0
  
  i <- 2
  while (i <= length(Cavg_daily$ID)) {
    Cavg_daily$AUCDBDQ[i] <- Cavg_daily$AAUCBDQ[i] - Cavg_daily$AAUCBDQ[i - 1]
    Cavg_daily$AUCDM2[i] <- Cavg_daily$AAUCM2[i] - Cavg_daily$AAUCM2[i - 1]
    
    i <- i + 1
  }
  
  Cavg_daily <- Cavg_daily %>% mutate(
    AUCDBDQ = ifelse(time == 0, 0, AUCDBDQ), 
    AUCDM2  = ifelse(time == 0, 0, AUCDM2)
  )
  
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
    q97BDQ <- quantile(Cavg_daily$AUCDBDQ/24*1000, probs = 0.97)
    max15BDQ <- max(Cavg_daily$AUCDBDQ)/24*1000*0.15
    ylimitsBDQ <- q97BDQ + max15BDQ
  }
  
  a1 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDBDQ, 
                                 color = as.factor(regimen), 
                                 fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgDBDQ, ymax = upper_CavgDBDQ), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Daily Average BDQ concentration (ng/mL)")) +
    ggtitle("Daily Average BDQ Concentration (ng/mL) vs Time") +
    coord_cartesian(ylim = c(0, ylimitsBDQ)) +
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
    maxM2 <- max(Cavg_daily$AUCDM2)/24*1000
    ylimitsM2 <- maxM2
  } else {
    q97M2 <- quantile(Cavg_daily$AUCDM2/24*1000, probs = 0.97)
    max15M2 <- max(Cavg_daily$AUCDM2)/24*1000*0.15
    ylimitsM2 <- q97M2 + max15M2
  }
  
  a2 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDM2, 
                                color = as.factor(regimen), 
                                fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgDM2, ymax = upper_CavgDM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Daily Average M2 concentration (ng/mL)")) +
    ggtitle("Daily Average M2 Concentration (ng/mL) vs Time") +
    coord_cartesian(ylim = c(0, ylimitsM2)) +
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
PKWavg_plots <- function(input, sim_PKtable) {
  
  Cavg_weekly <- sim_PKtable %>% 
    filter(time %% 168 == 0) %>%
    mutate(WEEK = time/168)
  
  Cavg_weekly <- subset(Cavg_weekly, select = c(ID, time, regimen, WEEK, AAUCBDQ, AAUCM2))
  
  # Calculate weekly AUC
  Cavg_weekly$AUCWBDQ <- 0
  Cavg_weekly$AUCWM2 <- 0
  
  i <- 2
  while (i <= length(Cavg_weekly$ID)) {
    Cavg_weekly$AUCWBDQ[i] <- Cavg_weekly$AAUCBDQ[i] - Cavg_weekly$AAUCBDQ[i - 1]
    Cavg_weekly$AUCWM2[i] <- Cavg_weekly$AAUCM2[i] - Cavg_weekly$AAUCM2[i - 1]
    
    i <- i + 1
  }
  
  Cavg_weekly <- Cavg_weekly %>% mutate(
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
    )
  
  # Set dynamic ylim BDQ
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
    ylimitsBDQ <- maxBDQ
  } else {
    q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
    max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
    ylimitsBDQ <- q97BDQ + max15BDQ
  }
  
  a1 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWBDQ, 
                                    color = as.factor(regimen), 
                                    fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
    ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
    coord_cartesian(ylim = c(0, ylimitsBDQ)) +
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
    ylimitsM2 <- maxM2
  } else {
    q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
    max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
    ylimitsM2 <- q97M2 + max15M2
  }
  
  a2 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWM2, 
                                    color = as.factor(regimen), 
                                    fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
    geom_line(size = 1) +
    theme_bw() +
    labs(x = "Time (weeks)", y = c("Weekly Average M2 concentration (ng/mL)")) +
    ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
    coord_cartesian(ylim = c(0, ylimitsM2)) +
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