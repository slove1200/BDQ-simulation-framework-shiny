extract_contour_df <- function(contour_list, contour_levels, sim_nr = NULL, pair = NULL) {
  cont_df <- NULL
  for (i in 1:length(contour_list)) {
    # Get the current contour's level
    current_level <- contour_list[[i]]$level[1]
    
    # Find matching percentile(s) that match exactly 5%, 50%, or 95%
    matching_percentiles <- names(contour_levels[which(abs(contour_levels - current_level) < 1e-10)])
    valid_percentiles <- intersect(matching_percentiles, c("5%", "50%", "95%"))
    
    # Take the first valid matching percentile if there are any matches
    percentile <- if (length(valid_percentiles) > 0) valid_percentiles[1] else NA
    
    # Create dataframe for current contour
    current_df <- contour_list[[i]] |> 
      as.data.frame() |>
      dplyr::mutate(
        circle = i,
        percentile = percentile
      )
    
    cont_df <- rbind.data.frame(cont_df, current_df)
  }
  
  # Add simulation number if provided
  if (!is.null(sim_nr)) {
    cont_df <- cont_df |> dplyr::mutate(sim_nr = sim_nr)
  }
  
  # Add variable pair information if provided
  if (!is.null(pair)) {
    cont_df <- cont_df |> dplyr::mutate(var1 = pair[1], var2 = pair[2])
  }
  
  return(cont_df)
}

simulate_contours <- function(sim_data, percentiles, sim_nr, pairs_matrix = NULL, verbose = TRUE) {
  
  # calculate contours for every percentile and every variable pair combination
  sim_contours_list <- list()
  i <- 1
  for (b in 1 : sim_nr) {
    
    # message switch
    if (verbose == TRUE) {
      message("\r", "Calculate contours simulation:", b, "/", sim_nr)
    }
    
    sim_data_b <- sim_data[sim_data$simulation_nr == b, ]
    for (p in 1 : nrow(pairs_matrix)) {
      #use ks for density computation
      kd_sim <- ks::kde(sim_data_b[, pairs_matrix[p, ]], compute.cont = TRUE, approx.cont = FALSE)
      contour_sim <- with(kd_sim, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                          z = estimate, levels = cont[paste0(100-percentiles, "%")]))
      #extract information
      sim_contours_list[[i]] <- extract_contour_df(contour_sim, kd_sim$cont, b, pairs_matrix[p, ])
      i <- i + 1
      
    }
    
  }
  message("\n")
  sim_contours <- dplyr::bind_rows(sim_contours_list)
  
  return(sim_contours)
}

create_polygon <- function(ci_data) {
  polygon_list <- list()
  for (cr in unique(ci_data$circle)) {
    dat_matrix <- list(ci_data |> dplyr::filter(circle == cr) |> dplyr::select(x, y) |> as.matrix())
    polygon_list[[as.character(cr)]] <- sf::st_polygon(dat_matrix)
  }
  
  if (length(polygon_list) >= 2) {
    p_union <- polygon_list[[1]]
    for (cr in 2:length(polygon_list)) {
      p_union <- sf::st_union(p_union, polygon_list[[cr]])
    }
    
    polygon_pairs <- t(combinat::combn(1:length(polygon_list), 2))
    p_intersect <- list()
    for (cr in 1:nrow(polygon_pairs)) {
      p_intersect[[cr]] <- sf::st_intersection(polygon_list[[polygon_pairs[cr, 1]]], polygon_list[[polygon_pairs[cr, 2]]])
    }
    
    if (length(p_intersect) > 1){
      p_intersect_full <- p_intersect[[1]]
      for (cr in 2:length(p_intersect)) {
        p_intersect_full <- sf::st_union(p_intersect_full, p_intersect[[cr]])
      }
    } else {
      p_intersect_full <- p_intersect[[1]]
    }
    
    p_full <- sf::st_difference(p_union, p_intersect_full)
    
  } else {
    p_full <- polygon_list[[1]]
  }
  
  return(p_full)
}

create_geom_donutVPC <- function(sim_contours, conf_band = 95, colors_bands = c("#99E0DC", "#E498B4"), return_polygons = FALSE) {
    
    # percentiles <- as.numeric(gsub("%", "", unique(sim_contours$percentile), fixed = T))
    percentiles <- as.numeric(gsub("%", "", sort(unique(sim_contours$percentile)), fixed = T))
    pairs_matrix <- unique(sim_contours[, c("var1", "var2")])
    
    colors_bands <- colors_bands[((1:length(percentiles))/2 == round((1:length(percentiles))/2)) + 1] # colors were assigned to the bands depending on the odd or even order of percentiles
    names(colors_bands) <- percentiles
    
    contour_geoms <- list()
    conf_geom_data <- list()
    for (p in 1:nrow(pairs_matrix)) {
      var_pair <- paste0(pairs_matrix[p, 1], "-", pairs_matrix[p, 2])
      
      contour_geoms[[var_pair]] <- list()
      for (pr in percentiles) {
        sim_full_df <- sim_contours |>
          dplyr::filter(var1 == pairs_matrix[p, 1], var2 == pairs_matrix[p, 2]) |>
          dplyr::filter(percentile == paste0(pr, "%"))
        kd_sim_full <- ks::kde(sim_full_df[, c("x", "y")], compute.cont = TRUE, approx.cont = FALSE)
        # new contour was calculated based the coordinates of the previously calculated contours
        # different countours at the same density level will be merged
        contour_sim_full <- with(kd_sim_full, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                                      z = estimate, levels = cont[paste0(100-conf_band, "%")]))
        contour_data <- extract_contour_df(contour_sim_full, kd_sim_full$cont, pr, pairs_matrix[p, ])
        
        conf_geom_data[[paste0(var_pair, ": ", pr, "%")]] <- contour_data |>
          create_polygon()
        
        contour_geoms[[var_pair]][[paste0(pr, "%")]] <- ggplot2::geom_sf(data = conf_geom_data[[paste0(var_pair, ": ", pr, "%")]],
                                                                         color = colors_bands[as.character(pr)], fill = colors_bands[as.character(pr)])
      }
      
    }
    
    if (return_polygons) {
      return(conf_geom_data)
    } else {
      return(contour_geoms)
    }
  }

# 
# sim_nr <- 25
# verbose <- TRUE
# sim_data <- myCovSimMICE
# pairs_matrix <- matrix(c("K", "logMTTP"), 1, 2)
# percentiles <- c(5, 50, 95)
# 
# sim_contours <- simulate_contours(sim_data = sim_data,
#                                   percentiles = percentiles,
#                                   sim_nr = sim_nr,
#                                   pairs_matrix = pairs_matrix,
#                                   verbose = verbose)
# conf_band <- 95
# colors_bands <- c("#99E0DC", "#E498B4")
# 
# 
# sim_contours_gg <- create_geom_donutVPC(sim_contours = sim_contours,
#                                         conf_band = conf_band,
#                                         colors_bands = colors_bands)

get_donutVPC <- function(sim_data,
                         obs_data,
                         percentiles = c(10, 50, 90),
                         sim_nr,
                         pairs_matrix = NULL,
                         conf_band = 95,
                         colors_bands = c("#99E0DC", "#E498B4"),
                         verbose = TRUE) {
  
  # check if the pairs_matrix is valid
  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }
  
  if (!all(pairs_matrix %in% colnames(obs_data))) {
    stop("variable names in pairs_matrix do not exist in obs_data.")
  }
  
  # generate obs_contours
  obs_contours <- NULL
  for (p in 1:nrow(pairs_matrix)) {
    kd_obs <- ks::kde(obs_data |> dplyr::select(pairs_matrix[p, ]) |> na.omit(), compute.cont = TRUE)
    contour_obs <- with(kd_obs, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                             z = estimate, levels = cont[paste0(100-percentiles,"%")]))
    obs_contours <- rbind.data.frame(obs_contours, extract_contour_df(contour_obs, kd_obs$cont, 0, pairs_matrix[p, ]))
  }
  
  # generate sim_contours
  sim_contours <- simulate_contours(sim_data = sim_data,
                                    percentiles = percentiles,
                                    sim_nr = sim_nr,
                                    pairs_matrix = pairs_matrix,
                                    verbose = verbose)
  
  
  sim_contours_gg <- create_geom_donutVPC(sim_contours = sim_contours,
                                          conf_band = conf_band,
                                          colors_bands = colors_bands)
  
  # plot dountVPC(s) for every variable pair
  plot_list <- list()
  for (i in 1:nrow(pairs_matrix)) {
    var_pair <- paste0(pairs_matrix[i, 1], "-", pairs_matrix[i, 2])
    plot_list[[var_pair]] <- obs_contours |>
      dplyr::mutate(key = paste(percentile, var1, var2, sim_nr, circle)) |>
      dplyr::filter(var1 == pairs_matrix[i, 1], var2 == pairs_matrix[i, 2]) |>
      ggplot2::ggplot() +
      sim_contours_gg[[i]] +
      ggplot2::geom_path(ggplot2::aes(x = x, y = y, color = percentile, group = key), color = "black") +
      ggplot2::labs(x = pairs_matrix[i, 1], y = pairs_matrix[i, 2]) +
      ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
  }
  
  attr(plot_list, "obs_contours") <- obs_contours
  
  return(plot_list)
}



donutVPC <- function(sim_data,
                     obs_data,
                     percentiles = c(10, 50, 90),
                     sim_nr,
                     pairs_matrix = NULL,
                     conf_band = 95,
                     colors_bands = c("#99E0DC", "#E498B4"),
                     verbose = TRUE) {
  
  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }
  
  # generate the geom data for dount VPC
  donutVPC_geom <- get_donutVPC(sim_data = sim_data,
                                obs_data = obs_data,
                                percentiles = percentiles,
                                sim_nr = sim_nr,
                                pairs_matrix = pairs_matrix,
                                conf_band = conf_band,
                                colors_bands = colors_bands,
                                verbose = verbose)
  
  plot_donuts <- patchwork::wrap_plots(donutVPC_geom, nrow = floor(sqrt(nrow(pairs_matrix))))
  
  return(plot_donuts)
}

get_qqplot <- function(sim_data, obs_data, sim_nr = NULL, conf_band = 95, var = NULL, type = "ribbon", qq_color = "lightblue", title = FALSE) {
  
  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Variable names in var differ from names in obs_data")
  }
  
  # check if the simulation_nr exists in sim_data
  if (!("simulation_nr" %in% colnames(sim_data))) {
    stop("simulation_nr does not exist in sim_data")
  }
  
  # preparation of constants ----
  quant = seq(from = 0.01, to = 0.99, by = 0.01) # for qqplot, all the quantiles
  a  =  0.5* (100-conf_band)*0.01 # lower quantile for the summary statistics
  b = 1- a # upper quantile for the summary statistics
  
  quant_sims_sum <- list()
  qqplots <- list()
  
  # calculate the quantiles for each varibale in var
  for (c in 1 : length(var)) {
    
    ## calculation for observation quantiles ----
    obs_subset <- obs_data[[var[c]]]
    observed_quantiles <- quantile(as.vector(obs_subset), probs = quant, na.rm = TRUE)
    quant_obs <- cbind(as.data.frame(observed_quantiles), quant = quant)
    
    ## calculation for simulation ----
    sim_subset <- sim_data |>
      dplyr::select(var[c], simulation_nr)
    quant_sims <- list()
    
    ### calculate the quantiles statistics for each simulation run ----
    for (i in 1 : sim_nr){
      #### extract the ith simulation run ----
      marginal_data <- sim_subset |>
        dplyr::filter(simulation_nr == i)
      simulation_quantiles <- quantile(marginal_data[, var[c]], probs = quant, na.rm = TRUE)
      quant_sim <- cbind(as.data.frame(simulation_quantiles), quant = quant, simulation_nr = i)
      quant_sims[[i]] <- quant_sim
    }
    
    ## summary calculation of multiple simulations ----
    combi_quant <- do.call(rbind,quant_sims)
    sum_quant <- combi_quant |>
      dplyr::group_by(quant) |>
      dplyr::summarize(median_q = median(simulation_quantiles),
                       lower_q = quantile(simulation_quantiles, probs = a, na.rm = TRUE),
                       upper_q = quantile(simulation_quantiles, probs = b, na.rm = TRUE))
    
    ## plotting ----
    plot_dat <- merge(sum_quant, quant_obs, by = "quant")
    qqplot <- ggplot2::ggplot(plot_dat, aes(x = observed_quantiles))+
      ggplot2::geom_line(aes(y = median_q), color = "blue") +  # Line plot
      ggplot2::geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      # ggplot2::labs(x = "observed quantiles", y = "simulated quantiles",  title = var[c]) +  # Labels and title
      ggplot2::theme_bw() +
      ggplot2::theme(aspect.ratio = 1)
    
    if (type == "ribbon") {
      qqplot <- qqplot +
        ggplot2::geom_ribbon(data = plot_dat, aes(x = observed_quantiles, ymin = lower_q, ymax = upper_q), fill = qq_color, alpha = 0.5)   # Shaded area
    } else if (type == "point") {
      
      all_quant <- merge(quant_obs,combi_quant,by = "quant")
      plot_dat <- merge(all_quant, sum_quant, by = "quant")
      
      qqplot <- qqplot +
        ggplot2::geom_point(data = plot_dat, aes(x = observed_quantiles, y = simulation_quantiles), color = qq_color, size = 1, alpha = 0.01) +# 1/sim_nr
        ggplot2::theme(legend.position = "none")
    }
    
    if (title == FALSE) {
      qqplot <- qqplot +
        ggplot2::labs(x = "observed quantiles", y = "simulated quantiles")
    } else if (title == TRUE){
      qqplot <- qqplot +
        ggplot2::labs(x = "observed quantiles", y = "simulated quantiles",  title = var[c]) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       aspect.ratio = 1)
    }
    
    quant_sims_sum[[c]] <- sum_quant # not sure if it is necessary to keep
    qqplots[[var[c]]] <- qqplot
    
  }
  
  return(qqplots)
}

mVPC <- function(sim_data,
                 obs_data,
                 var = NULL,
                 sim_nr,
                 title = NULL,
                 colors_bands = c("#99E0DC", "#E498B4"),
                 caption = NULL,
                 percentiles = c(10, 50, 90),
                 conf_band = 95,
                 return_grob = FALSE,
                 verbose = TRUE) {
  
  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Variable names in var differ from names in obs_data")
  }
  
  # determine the variable pairs of interest ----
  pairs_matrix <- t(combinat::combn(var, 2))
  
  # generate subplots for qqplots
  qqplots <- get_qqplot(sim_data = sim_data,
                        obs_data = obs_data,
                        sim_nr = sim_nr,
                        conf_band = conf_band ,
                        var = var,
                        title = FALSE)
  
  # generate subplots for donutplots
  donutVPCs <- get_donutVPC(sim_data = sim_data,
                            obs_data = obs_data,
                            percentiles = percentiles,
                            sim_nr = sim_nr,
                            pairs_matrix = pairs_matrix,
                            conf_band = conf_band,
                            colors_bands = colors_bands,
                            verbose = verbose)
  
  # arrange the subplots
  n = length(var)
  nr_cross_plots <- choose(length(var), 2)
  layout_mat <- matrix(NA, nrow = n, ncol = n)
  layout_mat[lower.tri(layout_mat)] <- 1 : nr_cross_plots
  diag(layout_mat) <- nr_cross_plots + (1 : n)
  list_plots <- c(donutVPCs, qqplots)
  
  
  # determine the output
  if (return_grob != FALSE) {
    gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
  } else {
    mVPC_plot <- gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
    return(mVPC_plot)
  }
  
}

# Evaluate VPC of joint density
orgCovsEx$logMTTP <- log(orgCovsEx$MTTP)
myCovSimMICE$logMTTP <- log(myCovSimMICE$MTTP)

orgCovsEx <- orgCovsEx %>% select(-MTTP)
myCovSimMICE <- myCovSimMICE %>% select(-MTTP) %>% rename("simulation_nr" = "NSIM")


# donutVPC(sim_data = myCovSimMICE, obs_data = orgCovsEx,
#          percentiles = c(5, 50, 95), sim_nr = 10,
#          pairs_matrix = matrix(c("AGE", "AGE", "AGE", "AGE", "AGE",
#                                  "CACOR", "CACOR", "CACOR", "CACOR",
#                                  "K", "K", "K",
#                                  "logMTTP", "logMTTP",
#                                  "WT",
#                                  "CACOR", "K", "logMTTP", "WT", "ALB",
#                                  "K", "logMTTP", "WT", "ALB",
#                                  "logMTTP", "WT", "ALB",
#                                  "WT", "ALB",
#                                  "ALB"), 15, 2),
#          conf_band = 95, colors_bands = c("#99E0DC", "#E498B4"))

myCovSimMICECont <- myCovSimMICE %>% select("AGE","CACOR", "K", "logMTTP", "WT", "ALB", "simulation_nr")
orgCovsExCont <- orgCovsEx %>% select("AGE","CACOR", "K", "logMTTP", "WT", "ALB")

mVPC(sim_data = myCovSimMICECont, obs_data = orgCovsExCont, var = NULL, sim_nr = 15,
     percentiles = c(5, 50, 95), colors_bands = c("#99E0DC", "#E498B4"))
