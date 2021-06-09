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

utm2ll <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "UTM"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
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

# sim predictions and index function
get_all_sims <- function(fit_obj, newdata, sims = 200, level = 0.95,
  return_sims = TRUE, return_full_obj = TRUE,
  est_function = stats::median,
  agg_function = function(x) sum(exp(x))){

  pred_obj <- predict(fit_obj, newdata = newdata, sims = sims)
  # browser()
  i <- get_index_sims(pred_obj, return_sims = F, level = level,
    est_function = est_function, agg_function = agg_function)

  if(return_sims){
  i_sims <- get_index_sims(pred_obj, return_sims = T, level = level,
    est_function = est_function, agg_function = agg_function)
    return(list(index = i, sims = i_sims, grid = newdata, sim.predictions = pred_obj#, fit_obj = fit_obj
      ))
  } else {
    return(i)
  }
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

map_predictions <- function(
  pred_data = NULL,
  obs_data,
  fill_aes = exp(est),
  pred_min = min(exp(pred_data$est), na.rm = T),
  pred_max = max(exp(pred_data$est), na.rm = T),
  size_aes = (catch_count / hook_count) * 100,
  obs_col = "black",
  title = "",
  size_lab = "Observed fish\nper 100 hooks",
  fill_lab = "Predicted fish\nper 100 hooks",
  legend_position = c(0.99, 0.99),
  grey_waters = TRUE) {
  utm_zone9 <- 3156
  # download from:
  # https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/
  if (!file.exists("data-generated/coast_gshhg.rds")) {
    coast_gshhg <- sf::read_sf("data/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp") %>%
      st_crop(c(xmin = -133, ymin = 47, xmax = -121, ymax = 55))
    coast_gshhg_proj <- sf::st_transform(coast_gshhg, crs = utm_zone9)
    saveRDS(coast_gshhg_proj, file = "data-generated/coast_gshhg.rds")
  } else {
    coast_gshhg_proj <- readRDS("data-generated/coast_gshhg.rds")
  }

  focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
    layer = "taaqwiihak_areaVer2", quiet = TRUE)
  focal_area_proj <- sf::st_transform(focal_area, crs = utm_zone9)

  library(PBSmapping) # needs this for some reason
  majorbound <- load_boundaries(9)
  # attributes(majorbound)$PolyData
  # 5AB
  bound5Bnorth <- fortify(majorbound) %>%
    filter(PID %in% c(6)) %>%
    filter( # POS != 46 & # strange jog over HG
      X > 200 & X < 560 & Y > 5700
    )
  bound5Anorth <- fortify(majorbound) %>%
    filter(PID %in% c(5)) %>%
    filter(
      X > 200 & X < 600 & Y > 5650
    )
  # 3CD
  bound3Cnorth <- fortify(majorbound) %>%
    filter(PID %in% c(3)) %>%
    filter(X > 200 & X < 725 & Y > 5400) #%>%
  # gfplot:::utm2ll(utm_zone = 9)
  bound3Dnorth <- fortify(majorbound) %>%
    filter(PID %in% c(4)) %>%
    filter(X > 200 & X < 600 & Y > 5550)

  g <- ggplot(focal_area_proj)

  if (!is.null(pred_data)) {
    g <- g + geom_tile(
      data = pred_data,
      aes(X * 100000, Y * 100000, fill = {{fill_aes}}),
      width = 2000, height = 2000, colour = NA
    )
  }
  g <- g +
    geom_line( # add major management region boundaries
      data = bound3Cnorth,
      aes(X * 1e3, Y * 1e3), colour = "grey20", lty = 1,
      inherit.aes = F
    ) +
    geom_line( # add major management region boundaries
      data = bound3Dnorth,
      aes(X * 1e3, Y * 1e3), colour = "grey20", lty = 1,
      inherit.aes = F
    ) +
    geom_line( # add major management region boundaries
      data = bound5Anorth,
      aes(X * 1e3, Y * 1e3), colour = "grey20", lty = 1,
      inherit.aes = F
    ) +
    geom_sf(colour = "red", fill = NA, size = 0.70) + # add focal area behind coast
    geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey75", col = "grey55") +
    scale_fill_viridis_c(
      # trans = ggsidekick::fourth_root_power_trans(),
      trans = "sqrt",
      limits = c(pred_min, pred_max),
      na.value = "yellow",
      option = "D"
    ) +
    labs(fill = fill_lab, size = size_lab) +
    geom_point(
      data = obs_data,
      mapping = aes(
        X * 1e5, Y * 1e5,
        size = {{size_aes}}
      ), pch = 21,
      inherit.aes = FALSE, colour = obs_col, alpha = 0.9
    ) +
    scale_size_area(max_size = 3) +
    annotate("text",
      x = convert2utm9(-128.8, 51.1)[1],
      y = convert2utm9(-128.8, 51.1)[2], label = "5A") +
    annotate("text", x = convert2utm9(-128.8, 50.3)[1],
      y = convert2utm9(-128.8, 50.3)[2],
      label = "3D") +
    annotate("text", x = convert2utm9(-128.8, 48.8)[1],
      y = convert2utm9(-128.8, 48.8)[2],
      label = "3C")

  if (title != "") {
    g <- g + ggtitle(title)
  }

  if (grey_waters){
    g <- g +
      geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey99", col = "grey55") +
      geom_point(
        data = obs_data,
        mapping = aes(
          X * 1e5, Y * 1e5,
          size = {{size_aes}}
        ), pch = 21,
        inherit.aes = FALSE, colour = "white", alpha = 0.99
      ) + theme(
      legend.key = element_rect(colour = NA, fill = "grey75"),
      panel.background = element_rect(color = NA, size = 1, fill = "grey75"))
  }

  g <- g +
    theme(panel.grid.major = element_line(colour = "grey60", size = 0.3)) +
    coord_sf(
      xlim = c(230957.7 + 200000, 1157991 - 350000),
      ylim = c(5366427, 6353456 - 550000)
    ) +
    guides(
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 0)
    ) +
    theme(axis.title = element_blank(),
      legend.box.background = element_rect(color = NA, size = 1, fill = "#ffffff90"),
      legend.position = legend_position,
      legend.justification = c(1, 1)
    )
  g
}

convert2utm9 <- function(lon, lat) {
  temp <- data.frame(lon = lon, lat = lat)
  temp <- sf::st_as_sf(temp, coords = c("lon", "lat"), crs = 4326)
  temp <- sf::st_transform(temp, crs = 3156)
  temp
  as.numeric(temp$geometry[[1]])
}
