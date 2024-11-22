create_population_plots <- function(df_virtualPop) {
  
  # Import full simulated dataset for boxplot
  myCovSimMICE <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Big_Virtual_Population_SimulatedforUse.csv", 
                           header = T)
  
  # Convert MTTP value from hours to days
  df_virtualPop$MTTPd <- round(df_virtualPop$MTTP/24, 2)
  
  # Columns for prettier formating using in Facet_wrap
  df_virtualPop$AGETYPE   <- "Age"
  df_virtualPop$WTTYPE    <- "Weight"
  df_virtualPop$CACORTYPE <- "Corrected Ca"
  df_virtualPop$KTYPE     <- "Potassium"
  df_virtualPop$TTPTYPE   <- "Baseline TTP"
  df_virtualPop$ALBTYPE   <- "Albumin"
  
  # Define common theme with borders
  my_theme <- theme_bw() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 5, 5, 5), 
      plot.title = element_text(size = 18),       # Main title
      axis.title = element_text(size = 16),       # Axis titles
      axis.text = element_text(size = 14),        # Axis text
      legend.title = element_text(size = 16),     # Legend title
      legend.text = element_text(size = 14),      # Legend text
      strip.text = element_text(size = 16), 
      legend.position = "bottom", 
      legend.spacing.x = unit(0.5, 'cm')          # Add space between legend labels
    )
  
  # Create individual covariate plots (dots represent sampled population from the whole dataset)
  p1 <- ggplot(myCovSimMICE, aes(x = "Age", y = AGE)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "Age", y = AGE),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Age (years)", x = "") +
    facet_wrap(~AGETYPE) +
    my_theme
  
  p2 <- ggplot(myCovSimMICE, aes(x = "Weight", y = WT)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "Weight", y = WT),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Weight (kg)", x = "") +
    facet_wrap(~WTTYPE) +
    my_theme
  
  p3 <- ggplot(myCovSimMICE, aes(x = "Calcium", y = CACOR)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "Calcium", y = CACOR),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Corrected calcium level (mmol/L)", x = "") +
    facet_wrap(~CACORTYPE) +
    my_theme
  
  p4 <- ggplot(myCovSimMICE, aes(x = "Potassium", y = K)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "Potassium", y = K),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Potassium level (mmol/L)", x = "") +
    facet_wrap(~KTYPE) +
    my_theme
  
  p5 <- ggplot(myCovSimMICE %>% mutate(MTTPd = round(MTTP/24, 2)), aes(x = "TTP", y = MTTPd)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "TTP", y = MTTPd),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Baseline time-to-positivity (days)", x = "") +
    facet_wrap(~TTPTYPE) +
    my_theme
  
  p6 <- ggplot(myCovSimMICE, aes(x = "Albumin", y = ALB)) +
    geom_boxplot(fill = "lightblue", na.rm = TRUE, outliers = FALSE) +
    geom_point(data = df_virtualPop, aes(x = "Albumin", y = ALB),
               alpha = 0.6, position = position_jitter(width = 0.2),
               color = "gray10", 
               size = 2.5) +
    labs(y = "Albumin (g/dL)", x = "") +
    facet_wrap(~ALBTYPE) +
    my_theme
  
  # Combine plots using patchwork
  combined_plot <- p1 + p2 + p3 + p4 + p5 + p6 + 
    plot_layout(ncol = 3)
  
  return(combined_plot)
}