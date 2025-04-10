TTP_plots <- function(input, sim_TTPtable) {
  
  reg1_dur <- NULL
  reg2_dur <- NULL
  reg3_dur <- NULL
  
  # calculate duration
  reg1_dur <- (if(input$LD1) input$ldur_1 * ifelse(input$lunit_1 == "2", 1, 1/7) else 0) + 
    (input$mdur_1 * ifelse(input$munit_1 == "2", 1, 1/7)) +
    (if(input$MD2_1) input$m2dur_1 * ifelse(input$m2unit_1 == "2", 1, 1/7) else 0)
  
  reg2_dur <- if(input$RG2){
    (if(input$LD2) input$ldur_2 * ifelse(input$lunit_2 == "2", 1, 1/7) else 0) + 
    (input$mdur_2 * ifelse(input$munit_2 == "2", 1, 1/7)) +
    (if(input$MD2_2) input$m2dur_2 * ifelse(input$m2unit_2 == "2", 1, 1/7) else 0)
  }
    
  reg3_dur <- if(input$RG3){
    (if(input$LD3) input$ldur_3 * ifelse(input$lunit_3 == "2", 1, 1/7) else 0) + 
    (input$mdur_3 * ifelse(input$munit_3 == "2", 1, 1/7)) +
    (if(input$MD2_3) input$m2dur_3 * ifelse(input$m2unit_3 == "2", 1, 1/7) else 0)
  }
    
  durations <- c(reg1_dur, reg2_dur, reg3_dur)
  valid_durations <- which(durations>0)
  
  dfForPlotTTP <- sim_TTPtable %>% 
    filter(FLAG == 2 & NEG == 1) %>% 
    group_by(WEEKP, regimen) %>%
    summarise(prop = 1-(n()/nrow(sim_TTPtable %>% filter(regimen == 1 & FLAG == 2 & WEEKP == 1))))
  ## proportion = num of positive sample/total samples in each WEEKP
  
  # Create a full WEEKP grid per regimen based on respective durations
  all_weeks <- map_dfr(valid_durations, function(rg) {
    data.frame(
      WEEKP = 0:floor(durations[rg]),
      regimen = rg
    )
  })
  
  # Left join to fill in missing combinations with NA
  dfComplete <- all_weeks %>%
    left_join(dfForPlotTTP, by = c("WEEKP", "regimen")) %>%
    mutate(prop = ifelse(is.na(prop), 1, prop)) # Fill NA with 1
  
  
  # Create the dummy row you want to add (WEEKP = 0, regimen = c(1:n), prop = 1)
  dummy_row <- data.frame(
    WEEKP   = 0,
    regimen = 1:max(sim_TTPtable$regimen),
    prop    = 1
  )
  
  dfForPlotTTP <- bind_rows(dfComplete, dummy_row) %>%
    distinct(WEEKP, regimen, .keep_all = TRUE) %>%
    arrange(regimen, WEEKP)
  
  plot <- ggplot(dfForPlotTTP, aes(x = WEEKP, y = prop*100, 
                                color = as.factor(regimen))) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 1) +
    xlab("Time after start of treatment (weeks)") +
    ylab("Proportion of positive samples (%)") +
    ggtitle("Proportion of positive samples") +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(xlim = c(0, max(dfForPlotTTP$WEEKP))) +
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