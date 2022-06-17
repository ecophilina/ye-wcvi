### Make map of survey grids
# make map of grid areas and their overlap
# library(maptools)

# Make grids
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggsidekick) # for theme_sleek
# library(patchwork)
theme_set(ggsidekick::theme_sleek())

# load misc custom functions
source("analysis/functions.R")

f <- "figs/map-stitched-grid-overlap-expanded.png"

if (!file.exists(f)) {
  coast_gshhg_proj <- readRDS("data-generated/coast_gshhg.rds")

  focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
                            layer = "taaqwiihak_areaVer2", quiet = TRUE)
  focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

  focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
  focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)


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


  hbll_s_grid <- readRDS(here::here("grids/hbll_s_grid.rds"))
  hbll_s_grid <- hbll_s_grid$grid #%>% filter(latitude < 51.24999)

  hbll_sf <- st_as_sf(hbll_s_grid, coords = c("longitude", "latitude"), crs = 4326)
  hbll_sf <- st_transform(hbll_sf, crs = 3156)
  hbll_sf$source_data <- "HBLL"
  hbll_sf_grid <- sf::st_make_grid(x = hbll_sf,
                                   offset = st_bbox(hbll_sf)[c("xmin", "ymin")]+ c(-1000, -1000),
                                   cellsize=c(2000, 2000))

  # plot(st_geometry(hbll_sf_grid))
  # plot(st_geometry(hbll_sf), col= "red", add = T)

  keep <- st_intersects(hbll_sf, hbll_sf_grid)
  hbll_sf_grid <- hbll_sf_grid[unlist(keep)]
  hbll_sf2 <- st_as_sf(hbll_sf_grid)
  hbll_sf2$source_data <- "HBLL"


  load("grids/synoptic_grid.rda")
  trawl <- synoptic_grid %>%
    mutate(X = round(X), Y = round(Y),
           X = X*1000, Y = Y*1000,
           source_data = "TRAWL"
    )

  # to veiw the filled in trawl grid, but not illustrate actual survey cells
  # trawl <- readRDS(here::here("grids/nd_all_synoptic_new.rds")) %>%
  #   mutate(#X = round(X), Y = round(Y),
  #     X = X*1000, Y = Y*1000,
  #     source_data = "TRAWL"
  #   )

  trawl_sf <- st_as_sf(trawl, coords = c("X", "Y"), crs = 3156)
  trawl_sf_grid <- sf::st_make_grid(x = trawl_sf,
                                    offset = st_bbox(trawl_sf)[c("xmin", "ymin")]+ c(-1000, -1000),
                                    cellsize=c(2000, 2000))

  keep <- st_intersects(trawl_sf, trawl_sf_grid)
  trawl_sf_grid <- trawl_sf_grid[unlist(keep)]
  trawl_sf2 <- st_as_sf(trawl_sf_grid)
  trawl_sf2$source_data <- "TRAWL"

  # full_s_grid_utm <- grid_utm
  # full_s_grid_sf <- st_as_sf(full_s_grid_utm, coords = c("longitude", "latitude"))
  # st_crs(full_s_grid_sf) <- 4326 # set the coordinate reference system
  #
  # full_s_grid_sf2 <- st_transform(full_s_grid_sf, crs = 3156)
  # full_sf_grid <- sf::st_make_grid(x = full_s_grid_sf2,
  #   offset = st_bbox(hbll_sf)[c("xmin", "ymin")]+ c(-1000, -1000),
  #   cellsize=c(2000, 2000))
  # keep <- st_intersects(full_s_grid_sf2, full_sf_grid)
  # full_sf_grid <- full_sf_grid[unlist(keep)]
  # full_sf_grid2 <- st_as_sf(full_sf_grid)

  full_s_grid <- readRDS(file = "report-data/full_filled_grid_w_ext.rds")
  full_s_grid_trim <- full_s_grid %>% filter(depth <= 600)

  g <- ggplot(focal_area_proj) +
    # geom_sf(data=full_sf_grid2, fill = "white", colour = NA, alpha = 0.9) +
    geom_tile(data = full_s_grid_trim, aes(X * 100000, Y * 100000),
              # colour = "white", fill = "white", #colour = NA,
              colour = "grey85", fill = "grey85", #colour = NA,
              width = 2000, height = 2000) +
    # geom_sf(data=full_sf_grid2, fill = "grey90", colour = NA, alpha = 0.9) +
    geom_sf(data=trawl_sf2,  aes(fill = source_data), colour = NA,
            alpha = 0.7) +
    geom_sf(data=hbll_sf2,  aes(fill = source_data), colour = NA,
            alpha = 0.7) +
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
    # scale_fill_viridis_d(name = "Survey", begin = 0.2, end = 0.5, direction = -1) +
    scale_fill_manual(name = "Survey",
                      # values = c("aquamarine3","deepskyblue4")
                      values = c("aquamarine3","deepskyblue4")
                      ) +
    geom_sf(data = focal_area2_proj,
            # colour = "darkorchid4",
            colour = "deeppink4",
            lty = "twodash",
            fill = NA, size = 0.60) + # add focal area2 behind coast
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
  ggsave("figs/map-stitched-grid-overlap-expanded-dashed.png", width = 5.5, height = 5.5, dpi = 400)
}

