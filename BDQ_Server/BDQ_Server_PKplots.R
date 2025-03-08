TypPK_plots <- function(input, sim_PKtable) {

dfForPlotBDQ <- sim_PKtable %>%
  ungroup() %>%
  group_by(time, regimen) %>%
  summarize(
    lower = quantile(exp(IPRED)*1000, probs = 0.05),
    median = quantile(exp(IPRED)*1000, probs = 0.5),
    upper = quantile(exp(IPRED)*1000, probs = 0.95)
  )

# Set dynamic ylim BDQ
if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxBDQ <- exp(max(sim_PKtable$IPRED))*1000
    ylimitsBDQ <- maxBDQ
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonBDQ <- max(dfForPlotBDQ$upper, na.rm = TRUE)
    ylimitsBDQ <- maxRibbonBDQ * 0.75
  }

if (input$simtime > 48) {
  xbreaks <- seq(0, input$simtime, by = 8)
} else {
  xbreaks <- seq(0, input$simtime, by = 4)
}

a1 <- ggplot(dfForPlotBDQ, aes(x = time / 168, y = median, 
                               color = as.factor(regimen), 
                               fill = as.factor(regimen))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("BDQ concentration (ng/mL)")) +
  ggtitle("BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ), xlim = c(0, input$simtime)) +
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

dfForPlotM2 <- sim_PKtable %>%
  ungroup() %>%
  group_by(time, regimen) %>%
  summarize(
    lower = quantile(exp(IPREDM2)*1000, probs = 0.05),
    median = quantile(exp(IPREDM2)*1000, probs = 0.5),
    upper = quantile(exp(IPREDM2)*1000, probs = 0.95)
  )

# Set dynamic ylim M2
if (input$nsim == 1 || input$IIV == "OFF") { # individual
    maxM2 <- exp(max(sim_PKtable$IPREDM2))*1000
    ylimitsM2 <- maxM2 
  } else {
    # Use the actual maximum of the upper ribbon values plus a buffer
    maxRibbonM2 <- max(dfForPlotM2$upper, na.rm = TRUE)
    ylimitsM2 <- maxRibbonM2 * 1.03
  }

a2 <- ggplot(dfForPlotM2, aes(x = time / 168, y = median, 
                              color = as.factor(regimen), 
                              fill = as.factor(regimen))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("M2 concentration (ng/mL)")) +
  ggtitle("M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2), xlim = c(0, input$simtime)) +
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