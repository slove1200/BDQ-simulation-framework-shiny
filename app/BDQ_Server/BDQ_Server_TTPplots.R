TTP_plots <- function(input, sim_TTPtable) {

  dfForPlotTTP <- sim_TTPtable %>% 
    filter(FLAG == 2 & NEG == 1) %>% 
    group_by(WEEKP, regimen) %>% 
    summarise(prop = 1-(n()/nrow(sim_TTPtable %>% filter(regimen == 1 & FLAG == 2 & WEEKP == 1))))
  ## proportion = num of positive sample/total samples in each WEEKP
  
  # Create the dummy row you want to add (WEEKP = 0, regimen = c(1:n), prop = 1)
  dummy_row <- data.frame(
    WEEKP   = 0,
    regimen = c(1:max(sim_TTPtable$regimen)),
    prop    = 1
  )
  
  dfForPlotTTP <- rbind(dfForPlotTTP, dummy_row) %>% arrange(WEEKP)

  
  plot <- ggplot(dfForPlotTTP, aes(x = WEEKP, y = prop*100, 
                                color = as.factor(regimen))) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 1) +
    xlab("Time after start of treatment (weeks)") +
    ylab("Proportion of positive samples (%)") +
    ggtitle("Probability of positive samples") +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
    scale_x_continuous(breaks = seq(0, 24, by = 4), limits = c(0, 20.5)) +
    scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
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