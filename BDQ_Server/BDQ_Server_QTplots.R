QT_plots <- function(input, sim_QTtable) {
  
  dfForPlotQT <- sim_QTtable %>%
    ungroup() %>%
    group_by(time, regimen) %>%
    summarize(
      lower = quantile(IPRED, probs = 0.05),
      median = quantile(IPRED, probs = 0.5),
      upper = quantile(IPRED, probs = 0.95)
    )
  
  # Set dynamic ylim BDQ
  if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxQT <- max(sim_QTtable$IPRED)
    ymaxlimitsQT <- maxQT 
    
    minQT <- min(sim_QTtable$IPRED)
    yminlimitsQT <- minQT 
    
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonQT <- max(dfForPlotQT$upper, na.rm = TRUE)
    ymaxlimitsQT <- maxRibbonQT *1.02
  
    # Use the actual minimum of the lower ribbon values minus a buffer
    minRibbonQT <- min(dfForPlotQT$lower, na.rm = TRUE)
    yminlimitsQT <- minRibbonQT *1.02
  }

  if (input$simtime > 48) {
    xbreaks <- seq(0, input$simtime, by = 8)
  } else {
    xbreaks <- seq(0, input$simtime, by = 4)
  }
  
  plot <- ggplot(dfForPlotQT, aes(x = time / 168, y = median, 
                                  color = as.factor(regimen), 
                                  fill = as.factor(regimen))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, colour = NA) +
    geom_line(size = 1.2) +
    theme_bw() +
    labs(x = "Time (weeks)", y = ("QTc (ms)")) +
    ggtitle("QTc (ms) vs Time (weeks)") +
    coord_cartesian(ylim = c(yminlimitsQT, ymaxlimitsQT), xlim = c(0, input$simtime)) +
    scale_x_continuous(breaks = xbreaks) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
    theme(
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16), 
      legend.position = "bottom", 
      legend.spacing.x = unit(0.5, 'cm')          # Add space between legend labels
    ) + 
    guides(color = guide_legend("Regimen"),
           fill  = guide_legend("Regimen"))

  return(plot)
}