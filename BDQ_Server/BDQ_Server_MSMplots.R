MSM_plots <- function(input, sim_MSMtable) {
  
  dfForPlotMSM <- sim_MSMtable() %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
    # Ensure all combinations of time and STATE are included, even if they have no observations
    complete(regimen, time, STATE, fill = list(prop = 0))
  ## proportion = num of positive sample/total samples in each WEEKP
  
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory

  # Custom labeller function to change facet titles
  state_labels <- as_labeller(function(STATE) {
    case_when(STATE == 1 ~ "Active TB",
              STATE == 2 ~ "Converted",
              STATE == 3 ~ "Recurrent TB",
              STATE == 5 ~ "Death")
  })

  plot <- ggplot(dfForPlotMSM %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)),
                 aes(x = time, y = prop*100, color = as.factor(regimen))) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 1) +
    facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
    xlab("Time after start of treatment (weeks)") +
    ylab("Proportion of patients (%)") +
    ggtitle("Proportions of patients being in each state") +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 120, by = 8)) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F", "#C7B73E")) +
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
    guides(color = guide_legend("Regimen"))
  
  return(plot)
}