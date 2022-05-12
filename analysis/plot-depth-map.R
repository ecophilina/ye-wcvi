library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
theme_set(ggsidekick::theme_sleek())

# load misc custom functions
source("analysis/functions.R")


# Explore depth variation across regions
f <- paste0("figs/depth-map.png")
if (!file.exists(f)) {
  coast_gshhg_proj <- readRDS("data-generated/coast_gshhg.rds")

  focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
                            layer = "taaqwiihak_areaVer2", quiet = TRUE)
  focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

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
  bound3Csouth <- fortify(majorbound) %>%
    filter(PID %in% c(3)) %>%
    filter(X > 200 & X < 900 & Y < 5420) #%>%


  full_s_grid <- readRDS("report-data/full_filled_grid_paired.rds")
  full_s_grid_trim <- full_s_grid %>% filter(depth <= 600)

  g <- ggplot(focal_area_proj) +
    geom_tile(data = full_s_grid_trim, aes(X * 100000, Y * 100000, fill = depth),
              width = 2000, height = 2000) +
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
    geom_sf(colour = "red", fill = NA, size = 0.70) + # add focal area behind coast
    geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey75", col = "grey55") +
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
             label = "3C") +

    scale_fill_viridis_c(trans = "sqrt", name = "Depth", direction = -1, guide = guide_colourbar(reverse = TRUE)) +
    # scale_fill_viridis_c(name = "Depth", direction = -1) +
    geom_sf(colour = "red", fill = NA, size = 0.70) + # add focal area behind coast
    geom_sf(data = coast_gshhg_proj, size = 0.07, fill = "grey99", col = "grey55") +
    coord_sf(
      # xlim = c(230957.7 + 205000, 1157991 - 385000),
      # ylim = c(5366427 + 25000, 6353456 - 590000)
      xlim = c(230957.7 + 205000, 1157991 - 370000),
      ylim = c(5366427 + 2000, 6353456 - 590000)
    ) +
    annotate("text",
             x = convert2utm9(-126.25, 50.1)[1],
             y = convert2utm9(-126.25, 50.1)[2],
             colour = "gray65",
             size = 3.5,
             label = "Vancouver\nIsland") +
    annotate("text",
             x = convert2utm9(-126.1, 51.4)[1],
             y = convert2utm9(-126.1, 51.4)[2],
             colour = "gray65",
             size = 5,
             label = "British\nColumbia") +
    ggsidekick::theme_sleek() + theme(
      panel.grid.major = element_line(colour = "grey60", size = 0.3),
      legend.key = element_rect(colour = NA, fill = "grey70"),
      axis.title = element_blank(),
      legend.box.background = element_rect(color = NA, size = 1, fill = "#ffffff90"),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1),
      panel.background = element_rect(color = NA, size = 1, fill = "grey70"))

  g

  ggsave("figs/depth-map.png",
         width = 5.5, height = 5.5, dpi = 400)
}
