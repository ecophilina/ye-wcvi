convert2utm <- function(df, coords = c("X", "Y"), out_crs = 3156) {
  x <- sf::st_as_sf(df,
    coords = coords, crs = 4326
  ) %>%
    sf::st_transform(crs = out_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  x$X <- x$X / 100000
  x$Y <- x$Y / 100000
  dplyr::bind_cols(x,
    df[, which(!names(df) %in% coords), drop = FALSE])
}

# to add sf geometry back onto dataframe
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

expand_prediction_grid <- function(grid, years) {
  nd <- do.call("rbind",
    replicate(length(years), grid, simplify = FALSE))
  nd[["year"]] <- rep(years, each = nrow(grid))
  nd
}

# leaves coast lines defined in lat lon unlike gfplot function
load_coastll <- function(xlim_ll, ylim_ll, utm_zone, buffer = 0) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
    xlim = xlim_ll + c(-buffer, buffer),
    ylim = ylim_ll + c(-buffer, buffer)
  )
}

load_boundaries <- function(utm_zone) {
  data("major", package = "PBSdata", envir = environment())
  gfplot:::ll2utm(major, utm_zone = utm_zone)
}

# TODO: we probably want to purge many of these...
get_diag <- function(m, response = "catch_count", variable = "depth_scaled", colour_var = "depth_m", start_year = 2007) {
  predictions <- predict(m)
  predictions$residuals <- residuals(m)
  
  predictions <- predictions %>% filter (year >= start_year)
  predictions$est_exp <- exp(predictions$est)
  
  print("R^2:")
  r2 <- cor(predictions$est, predictions[[response]])^2
  print(r2)
  
  print("")
  print("AIC:")
  print(AIC(m))
  
  print("")
  print("MSE:")
  print(mean(predictions$residuals^2))
  
  plot_map <- function(dat, column = "est") {
    ggplot(dat, aes_string("X", "Y", colour = column)) +
      geom_point(alpha = 0.5) +
      coord_fixed()+
      gfplot::theme_pbs() 
  }
  
  g <- plot_map(predictions, "est") +
    scale_colour_viridis_c() +
    ggtitle("Prediction (fixed effects + all random effects)")
  print(g)
  
  # g <- plot_map(predictions, "est_non_rf") +
  #   ggtitle("Prediction (fixed effects only)") +
  #   scale_colour_viridis_c()
  # print(g)
  # 
  #   g <- plot_map(predictions, "est_rf") +
  #     ggtitle("All random effects only") +
  #     scale_colour_gradient2()
  #   print(g)
  
  g <- plot_map(predictions, "omega_s") +
    ggtitle("Spatial random effects only") +
    scale_colour_gradient2()
  print(g)
  
  g <- plot_map(predictions, "epsilon_st") +
    ggtitle("Spatiotemporal random effects only") +
    facet_wrap(~year) +
    scale_colour_gradient2()
  print(g)
  
  g <- plot_map(predictions, "residuals") +
    ggtitle("Residuals") +
    facet_wrap(~year) +
    scale_colour_gradient2()
  print(g)
  
  g <- ggplot(predictions, aes_string("est_exp", response)) +
    geom_point(alpha = 0.2) +
    facet_wrap(~year) +
    coord_fixed() +
    geom_abline()+
    gfplot::theme_pbs() 
  print(g)
  
  g <- ggplot(predictions, aes(est, residuals)) +
    geom_point(alpha = 0.2) +
    geom_smooth()+
    gfplot::theme_pbs() 
  print(g)
  
  g <- ggplot(predictions, aes_string(variable, "residuals", colour = colour_var)) +
    geom_point(alpha = 0.4, size = 1.2) +
    geom_smooth(colour="grey", size = 1.2) +
    scale_colour_viridis_c(option = "B", direction = -1, begin = 0, end = 0.7, 
      limits= c(min(predictions[colour_var]), max(predictions[colour_var]))) + #, trans= sqrt
    # facet_wrap(~year, scales = "free_x")+
    gfplot::theme_pbs() + theme(
      axis.text.y = element_text(size = 14),
      axis.text.x = element_blank(), axis.ticks.x = element_blank())
  print(g)
  
  g <- ggplot(predictions, aes_string(variable, "residuals", colour = colour_var)) +
    geom_point(alpha = 0.4, size = 0.7) +
    geom_smooth(colour="grey", size = 0.7) +
    scale_colour_viridis_c(option = "B", direction = -1, begin = 0, end = 0.7, 
      limits= c(min(predictions[colour_var]), max(predictions[colour_var]))) + #, trans= sqrt
    facet_wrap(~year, scales = "free_x")+
    gfplot::theme_pbs() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  print(g)
  
}