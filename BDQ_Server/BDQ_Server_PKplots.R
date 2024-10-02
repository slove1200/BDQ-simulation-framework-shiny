create_plots <- function(sim_PKtable) {

dfForPlotBDQ <- sim_PKtable() %>%
  ungroup() %>%
  group_by(time, regimen) %>%
  summarize(
    lower = quantile(exp(IPRED)*1000, probs = 0.05),
    median = quantile(exp(IPRED)*1000, probs = 0.5),
    upper = quantile(exp(IPRED)*1000, probs = 0.95)
  )

ylimitsBDQ <- quantile(exp(sim_PKtable()$IPRED)*1000, probs = 0.99)

a1 <- ggplot(dfForPlotBDQ, aes(x = time / 168, y = median, 
                               color = as.factor(regimen), 
                               fill = as.factor(regimen))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("BDQ concentration (ng/mL)")) +
  ggtitle("BDQ Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsBDQ)) +
  scale_color_manual(values = c("#B6B6CC", "#C9A8AD", "#A9BFC2", "#CFC3B8")) +
  scale_fill_manual(values = c("#B6B6CC", "#C9A8AD", "#A9BFC2", "#CFC3B8")) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 12),      # Legend text
    strip.text = element_text(size = 16)
  ) + 
  guides(color = guide_legend("Regimen"),
         fill  = guide_legend("Regimen"))

dfForPlotM2 <- sim_PKtable() %>%
  ungroup() %>%
  group_by(time, regimen) %>%
  summarize(
    lower = quantile(exp(IPREDM2)*1000, probs = 0.05),
    median = quantile(exp(IPREDM2)*1000, probs = 0.5),
    upper = quantile(exp(IPREDM2)*1000, probs = 0.95)
  )

ylimitsM2 <- quantile(exp(sim_PKtable()$IPREDM2)*1000, probs = 0.99)

a2 <- ggplot(dfForPlotM2, aes(x = time / 168, y = median, 
                              color = as.factor(regimen), 
                              fill = as.factor(regimen))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Time (weeks)", y = c("M2 concentration (ng/mL)")) +
  ggtitle("M2 Concentration (ng/mL) vs Time") +
  coord_cartesian(ylim = c(0, ylimitsM2)) +
  scale_color_manual(values = c("#B6B6CC", "#C9A8AD", "#A9BFC2", "#CFC3B8")) +
  scale_fill_manual(values = c("#B6B6CC", "#C9A8AD", "#A9BFC2", "#CFC3B8")) +
  theme(
    plot.title = element_text(size = 18),       # Main title
    axis.title = element_text(size = 16),       # Axis titles
    axis.text = element_text(size = 14),        # Axis text
    legend.title = element_text(size = 14),     # Legend title
    legend.text = element_text(size = 12),      # Legend text
    strip.text = element_text(size = 16)
  ) + 
  guides(color = guide_legend("Regimen"),
         fill  = guide_legend("Regimen"))

plot <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")

return(plot)
}