# set map boundaries
# min_map_lat <- 48.3
max_map_lat <- 51.4
min_map_lon <- -130.1
# max_map_lon <- -124.9
min_map_lat2 <- 48.6
max_map_lat2 <- 50.01
min_map_lon2 <- -127.9
max_map_lon2 <- -125.2


min_map_lat <- 48.3
max_map_lon <- -124.85
min_map_lat3 <- 48.4


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
  stopifnot(inherits(x,"sf") #&& inherits(sf::st_geometry(x),"sfc_POINT")
    )
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

# full_s_grid <- readRDS(file = "data-generated/full_filled_grid_paired.rds")
# m_halibut <- readRDS(file = "models/halibut-hybrid-model-rocky-muddy-400kn.rds")
# i_hal_combined <- get_all_sims(m_halibut, newdata = full_s_grid)

get_all_sims <- function(fit_obj = NULL, tmbstan_model = NULL,
                         newdata, # if contains a column names `area` it will be scaled by this
                         fit_obj_bin = NULL, fit_obj_pos = NULL,
                         sims = 500, level = 0.95,
  split_by_region = T,
  area_divisor = 10000, # 10000 for grid area in m2 to biomass/hectare,
  # could be 1000000 for m2 to km2, or 4000000 for m2 to # hooks 2x2 km grid cell
  return_sims = TRUE, return_full_obj = TRUE,
  est_function = stats::median,
  agg_function = function(x) sum(exp(x))){

# browser()
  if(!is.null(tmbstan_model)){
    pred_obj_unscaled <- predict(fit_obj, newdata = newdata, tmbstan_model = tmbstan_model)
  } else {
  if (!is.null(fit_obj)) {
    pred_obj_unscaled <- predict(fit_obj, newdata = newdata, sims = sims)
  }
  if (!is.null(fit_obj_bin) && !is.null(fit_obj_pos)) {
    pred_obj_unscaled_bin <- predict(fit_obj_bin, newdata = newdata, sims = sims)
    pred_obj_unscaled_pos <- predict(fit_obj_pos, newdata = newdata, sims = sims)
    pred_obj_unscaled <- log(plogis(pred_obj_unscaled_bin) * exp(pred_obj_unscaled_pos))
  }
  }

  if (any(names(newdata) == "area")) {
    # grid area is currently in m2 so need to convert to same units as the biomass variable
    newdata$area <- newdata$area / area_divisor
  #   p <- apply(pred_obj_unscaled, 2, function(x) x + log(newdata$area)) # also now `area` arg in get_index_sims()
  #   attr(p, "time") <- "year"
  }# else {
    p <- pred_obj_unscaled
  # }

  if (split_by_region) {

    by_region <- list()

    ind <- get_index_sims(p, return_sims = F, level = level, area = newdata$area,
      est_function = est_function, agg_function = agg_function)

    ind$region <- "all"

    i_sims <- get_index_sims(p, return_sims = T, level = level, area = newdata$area,
      est_function = est_function, agg_function = agg_function)

    i_sims$region <- "all"

    by_region[[1]] <- list(index = ind, sims = i_sims, grid = newdata
      , sim.predictions = p # this takes up a lot of space so if not using it...
    )

    setNames(by_region[1], "all")

    for (i in seq_along(unique(newdata$region))){

        pred <- p[newdata$region == unique(newdata$region)[i], ]
        attr(pred, "time") <- "year"

        ind2 <- get_index_sims(pred, return_sims = F, level = level,
                               area = newdata[newdata$region == unique(newdata$region)[i], ]$area,
          est_function = est_function, agg_function = agg_function)

        i_sims2 <- get_index_sims(pred, return_sims = T, level = level,
                                  area = newdata[newdata$region == unique(newdata$region)[i], ]$area,
          est_function = est_function, agg_function = agg_function)

        # setNames(ind2, unique(newdata$region)[i])
        ind2$region <- unique(newdata$region)[i]

        # setNames(i_sims2, unique(newdata$region)[i])
        i_sims2$region <- unique(newdata$region)[i]

        # setNames(pred, unique(newdata$region)[i])

        by_region[[i+1]] <- list(index = ind2, sims = i_sims2, grid = newdata
          , sim.predictions = pred # this takes up a lot of space so if not using it...
        )
        setNames(by_region[i+1], paste(unique(newdata$region)[i]))
    }

    # alternatively they could go in one df
    # ind <- do.call(rbind, ind)
    # i_sims <- do.call(rbind, i_sims)

    # browser()
    # return(list(index = ind, sims = i_sims, grid = newdata
    #   , sim.predictions = pred_obj # this takes up a lot of space so if not using it...
    #   #, fit_obj = fit_obj
    # ))
    return(by_region)

  } else {

    i <- get_index_sims(p, return_sims = F, level = level, area = newdata$area,
      est_function = est_function, agg_function = agg_function)

    if(return_sims){

      i_sims <- get_index_sims(p, return_sims = T, level = level, area = newdata$area,
        est_function = est_function, agg_function = agg_function)

      # list_of_one <- list()

      list_of_one <- list(index = i, sims = i_sims, grid = newdata
        , sim.predictions = p # this takes up a lot of space so if not using it...
        #, fit_obj = fit_obj
      )

      return(list_of_one)

    } else {
      return(i)
    }
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
get_diag <- function(m, response = "density",
  variable = "depth_scaled", colour_var = "depth_m", start_year = 2007) {

  # browser()
  set.seed(100)

  s <- simulate(m, nsim = 200)

  dharma_residuals(s, m)

  r <- dharma_residuals(s, m, plot = FALSE)
  browser()
  predictions <- predict(m)
  predictions$residuals <- residuals(m)

  predictions <- predictions %>% filter (year >= start_year)
  predictions$est_exp <- exp(predictions$est)

  print("R^2:")
  r2 <- cor(r$expected, r$observed)^2
  print(r2)

  print("")
  print("AIC:")
  print(AIC(m))

  print("")
  print("MSE:")
  print(mean(predictions$residuals^2))
  # qqnorm(predictions$residuals);qqline(predictions$residuals)
  #
  # hbll <- filter(predictions, survey == "HBLL")
  # qqnorm(hbll$residuals);qqline(hbll$residuals)


  plot_map <- function(dat, column = "est") {
    ggplot(dat, aes_string("X", "Y", colour = column)) +
      geom_point(alpha = 1, size = 0.2) +
      coord_fixed()+
      theme_void()
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
    ggtitle("Spatial random effects only", subtitle = " ") +
    scale_colour_gradient2()
  print(g)

  g <- plot_map(predictions, "epsilon_st") +
    ggtitle("Spatiotemporal random effects only", subtitle = " ") +
    facet_wrap(~year) +
    scale_colour_gradient2()
  print(g)

  g <- plot_map(predictions, "residuals") +
    ggtitle("Residuals", subtitle = " ") +
    facet_wrap(~year) +
    scale_colour_gradient2()
  print(g)

  g <- ggplot(predictions, aes_string(response,"est_exp")) +
    geom_point(alpha = 0.2) + geom_abline()+
    facet_wrap(~year) +
    scale_x_log10() + scale_y_log10() +
    coord_fixed() +
    xlab("Observed") + ylab("Predicted") +
    gfplot::theme_pbs() + theme(axis.text = element_blank())
  print(g)

  g <- ggplot(predictions, aes(est, residuals)) +
    geom_point(alpha = 0.2) +
    geom_smooth()+
    gfplot::theme_pbs()
  print(g)

  # g <- ggplot(predictions, aes_string(variable, "residuals", colour = colour_var)) +
  #   geom_point(alpha = 0.4, size = 1.2) +
  #   geom_smooth(colour="grey", size = 1.2) +
  #   scale_colour_viridis_c(option = "B", direction = -1, begin = 0, end = 0.7,
  #     limits= c(min(predictions[colour_var]), max(predictions[colour_var]))) + #, trans= sqrt
  #   # facet_wrap(~year, scales = "free_x")+
  #   gfplot::theme_pbs() + theme(
  #     axis.text.y = element_text(size = 14),
  #     axis.text.x = element_blank(), axis.ticks.x = element_blank())
  # print(g)

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
  obs_data = NULL,
  fill_aes = exp(est),
  col_aes = survey,
  pred_min = NULL,
  pred_max = NULL,
  viridis_dir = 1,
  viridis_option = "D",
  viridis_na = "yellow",
  size_aes = (catch_count / hook_count) * 100,
  max_size_obs = 4,
  obs_col = c("white", "#98FB98"), # "#FFDAB9"), # peach
  title = "",
  size_lab = "Observed fish\nper 100 hooks",
  fill_lab = "Predicted fish\nper 100 hooks",
  map_lat_limits = c(48.6, 51.3),
  map_lon_limits = c(-130.1,-125.1),
  legend_position = c(0.99, 0.99),
  legend_background = "#404788FF",#"#481567FF", #darkest "#440154FF", #"grey75",
  grey_waters = TRUE) {
  utm_zone9 <- 3156
  # download from:
  # https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/
  if (!file.exists(here::here("data-generated/coast_gshhg.rds"))) {
    coast_gshhg <- sf::read_sf(here::here("data/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")) %>%
      st_crop(c(xmin = -133, ymin = 47, xmax = -121, ymax = 55))
    coast_gshhg_proj <- sf::st_transform(coast_gshhg, crs = utm_zone9)
    saveRDS(coast_gshhg_proj, file = here::here("data-generated/coast_gshhg.rds"))
  } else {
    coast_gshhg_proj <- readRDS(here::here("data-generated/coast_gshhg.rds"))
  }

  focal_area <- sf::st_read(dsn = here::here("shape-files/taaqwiihak_areaVer2.shp"),
    layer = "taaqwiihak_areaVer2", quiet = TRUE)
  focal_area_proj <- sf::st_transform(focal_area, crs = utm_zone9)

  focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
  focal_area2_proj <- sf::st_transform(focal_area2, crs = utm_zone9)

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
  bound3Csouth <- fortify(majorbound) %>%
    filter(PID %in% c(3)) %>%
    filter(X > 200 & X < 900 & Y < 5420) #%>%
  # gfplot:::utm2ll(utm_zone = 9)
  bound3Dnorth <- fortify(majorbound) %>%
    filter(PID %in% c(4)) %>%
    filter(X > 200 & X < 600 & Y > 5550)

  g <- ggplot(focal_area_proj)

  if (!is.null(pred_data)) {
    if (is.null(pred_min)) {
      if (is.null(obs_data)) {
      g <- g + geom_tile(
        data = pred_data,
        aes(X * 100000, Y * 100000,
          colour = {{fill_aes}},
          fill = {{fill_aes}}), #colour = NA,
        width = 2000, height = 2000
      ) + scale_fill_viridis_c(
        # trans = ggsidekick::fourth_root_power_trans(),
        trans = "sqrt",
        direction = viridis_dir,
        na.value = viridis_na,
        option = viridis_option
      ) + scale_colour_viridis_c(
        # trans = ggsidekick::fourth_root_power_trans(),
        trans = "sqrt",
        direction = viridis_dir,
        na.value = viridis_na,
        option = viridis_option
      )} else{
        g <- g + geom_tile(
          data = pred_data,
          aes(X * 100000, Y * 100000,
            # colour = {{fill_aes}},
            fill = {{fill_aes}}), colour = NA,
          width = 2000, height = 2000
        ) + scale_fill_viridis_c(
          # trans = ggsidekick::fourth_root_power_trans(),
          trans = "sqrt",
          direction = viridis_dir,
          na.value = viridis_na,
          option = viridis_option
        )
      }
    } else {
      if (is.null(obs_data)) {
    g <- g + geom_tile(
      data = pred_data,
      aes(X * 100000, Y * 100000,
        colour = {{fill_aes}},
        fill = {{fill_aes}}), #colour = NA,
      width = 2000, height = 2000
    ) + scale_fill_viridis_c(
      # trans = ggsidekick::fourth_root_power_trans(),
      trans = "sqrt",
      limits = c(pred_min, pred_max),
      direction = viridis_dir,
      na.value = viridis_na,
      option = viridis_option
    ) + scale_colour_viridis_c(
      # trans = ggsidekick::fourth_root_power_trans(),
      trans = "sqrt",
      limits = c(pred_min, pred_max),
      direction = viridis_dir,
      na.value = viridis_na,
      option = viridis_option
    )
      } else {
        g <- g + geom_tile(
          data = pred_data,
          aes(X * 100000, Y * 100000,
            # colour = {{fill_aes}},
            fill = {{fill_aes}}), colour = NA,
          width = 2000, height = 2000
        ) + scale_fill_viridis_c(
          # trans = ggsidekick::fourth_root_power_trans(),
          trans = "sqrt",
          limits = c(pred_min, pred_max),
          direction = viridis_dir,
          na.value = viridis_na,
          option = viridis_option
        )
      }
    }
  }
  g <- g +
    geom_line( # add major management region boundaries
      data = bound3Csouth,
      aes(X * 1e3, Y * 1e3), colour = "grey20", lty = 1,
      inherit.aes = F
    ) +
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
    geom_sf(data = focal_area2_proj,
            colour = "darkorchid4",
            # colour = "deeppink4",
            fill = NA, size = 0.70) + # add focal area2 behind focal area 1
    geom_sf(colour = "red", fill = NA, size = 0.70) + # add focal area behind coast
    geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey75", col = "grey55") +
    labs(fill = fill_lab, colour = fill_lab, size = size_lab) +
    annotate("text",
      x = convert2utm9(-129.8, 50.7)[1],
      y = convert2utm9(-129.8, 50.7)[2], label = "5A") +
    annotate("text",
      x = convert2utm9(-129.8, 50.3)[1],
      y = convert2utm9(-129.8, 50.3)[2],
      label = "3D") +
    annotate("text",
      x = convert2utm9(-129.8, 48.8)[1],
      y = convert2utm9(-129.8, 48.8)[2],
      label = "3C")

  if (title != "") {
    g <- g + ggtitle(title)
  }


  if (grey_waters){
    g <- g +
      geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey99", col = "grey55") +
      theme(
      legend.key = element_rect(colour = NA, fill = legend_background),
      panel.background = element_rect(color = NA, size = 1, fill = "grey75"))

    # if (!is.null(obs_data)) {
    #   g <- g + geom_point(
    #     data = obs_data,
    #     mapping = aes(
    #       X * 1e5, Y * 1e5,
    #       size = {{size_aes}}
    #     ), pch = 21,
    #     inherit.aes = FALSE, colour = "white", alpha = 0.99
    #   ) + scale_size_area(max_size = 4, n.breaks = 4)
    # }
  }

  if (!is.null(obs_data)) {
    g <- g + geom_point(
      data = obs_data,
      mapping = aes(
        X * 1e5, Y * 1e5,
        colour = {{col_aes}},
        size = {{size_aes}}
      ), pch = 21,
      inherit.aes = FALSE, alpha = 0.99
    ) +
      scale_colour_manual(name = NULL, values = obs_col) +
      scale_size_area(max_size = max_size_obs, n.breaks = 5)+
      guides(
        fill = guide_colorbar(order = 1),
        size = guide_legend(order = 2, override.aes = list(colour="white")),
        colour = guide_legend(order = 3)
      )
  }

  g <- g +
    theme(panel.grid.major = element_line(colour = "grey60", size = 0.3)) +
    coord_sf(expand = F,
      # xlim = c(230957.7 + 205000, 1157991 - 385000),
      # ylim = c(5366427 + 25000, 5694150 - 8000) #6353456 - 590000)
      xlim = c(convert2utm9(map_lon_limits[1], 50.0)[1], convert2utm9(map_lon_limits[2], 50.0)[1]),
      ylim = c(convert2utm9(-130, map_lat_limits[1])[2], convert2utm9(-130, map_lat_limits[2])[2])
    ) +
    theme(axis.title = element_blank(),
      legend.box.background = element_rect(color = NA, size = 1, fill = "#ffffff90"),
      # legend.key.colour = "white",
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


### functions for editing scales on specific faceted plots
### lifted from: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }

  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }

  structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)

    if(is.null(params$scale_overrides)) return(scales)

    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)

    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale

      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }

    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)

  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) ||
      !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }

  facet_super$params$scale_overrides <- scale_overrides

  ggproto(NULL, CustomFacetWrap,
    shrink = facet_super$shrink,
    params = facet_super$params
  )
}




