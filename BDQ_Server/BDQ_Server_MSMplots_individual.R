MSMidv_plots <- function(input, sim_MSMtable) {
  
  outMSMLongFormat <- sim_MSMtable %>% group_by(regimen) %>%
    select(ID, time, P_1, P_2, P_3, P_5, regimen) %>%
    pivot_longer(
      cols = c(P_1, P_2, P_3, P_5),
      names_to = "State",
      values_to = "Probability"
    )
  
  # Split the data into two groups
  high_prob <- outMSMLongFormat %>% filter(State %in% c("P_1", "P_2"))
  low_prob <- outMSMLongFormat %>% filter(State %in% c("P_3", "P_5"))
  
  # First, determine which regimens are active
  active_regimens <- outMSMLongFormat %>%
    pull(regimen) %>%
    unique() %>%
    sort()
  
  # Create labels for active regimens
  regimen_labels <- paste("Regimen", active_regimens)
  names(regimen_labels) <- active_regimens
  
  # Define dynamic colors for each regimen
  strip_colors <- strip_themed(background_x = elem_list_rect(fill = c("#CBCAE39D", "#E1C3C89D", "#C1D4D79D")))
  
  plot <- ggplot() +
    # High probability states (P1, P2)
    geom_line(data = high_prob, 
              aes(x = time, y = Probability * 100, color = State), 
              size = 1.5) +
    # Low probability states (P3, P5) - scaled
    geom_line(data = low_prob, 
              aes(x = time, y = Probability * 1000, color = State), 
              size = 1.5) +
    # Dynamic faceting based on active regimens with free y scales
    facet_wrap2(~factor(regimen, 
                        levels = active_regimens,
                        labels = regimen_labels[as.character(active_regimens)]),
                scales = "free_y", 
                strip = strip_colors) +
    # Primary y-axis (0-100 scale)
    scale_y_continuous(
      name = "Probability for Active infection, Converted (%)",
      limits = c(0, 100),
      sec.axis = sec_axis(~./10, 
                          name = "Probability for Recurrent TB, Death (%)")
    ) +
    theme_bw() +
    labs(x = "Time (weeks)", 
         color = "State") +
    ggtitle("Individual trajectory prediction") +
    scale_x_continuous(breaks = seq(0, 120, by = 8)) +
    theme(
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 18, color = "black", face = "bold", margin = margin(t = 10, b = 10)),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.spacing = unit(1, "lines"),
      # Correct way to set strip background
      strip.background = element_rect(
        fill = c("#CBCAE39D", "#E1C3C89D", "#C1D4D79D")
      )
    ) +
    scale_color_manual(
      values = c("P_1" = "#cf597e", "P_2" = "#009392", 
                 "P_3" = "#eeb479", "P_5" = "#CACACA"),
      labels = c("Active infection", "Converted", "Recurrent TB", "Death")
    )
  
  return(plot)
}