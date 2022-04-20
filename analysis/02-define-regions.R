# Make grids
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
# library(patchwork)
theme_set(ggsidekick::theme_sleek())

# load misc custom functions
source("analysis/functions.R")

# Scale covariates for models

f1 <- "data-generated/events_w_substrate_1km_buffer.rds"
if(!file.exists(f1)) {
  stop("Need to run part 1 of get-substrate.R script.")
} else {
  substrate <- readRDS(file = f1) %>%
    select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)
}

d_utm <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>%
  filter(year %in% years) %>% left_join(select(substrate, -X, -Y)) # looks like no missing data to worry about

if(sum(is.na(d_utm$muddy))) { stop("Check for missing substrate data before running models.")}

# check depth range and use as starting point for filtering grid
ggplot(d_utm) + geom_histogram(aes(depth_m)) + scale_x_log10(breaks=c(25, 50, 100, 200, 400, 600, 800, 1000))

f2 <- "grids/norca_grid_w_substrate_expanded.rds"

if(!file.exists(f2)) {
  stop("Need to run make-grid.Rmd")
} else {
  grid <- readRDS(file = f2) %>%
    filter(depth > 2 & depth < 600) # produces best match to combination of HBLL S and trawl grids
}
# grid <- grid[complete.cases(grid), ] # run this if only wanting complete 2x2 km cells

grid_utm <- expand_prediction_grid(grid, years = years) %>%
  # mutate(any_rock_scaled = (any_rock - mean(substrate$any_rock)/ sd(substrate$any_rock)) %>%
  mutate(depth_centred = log(depth) - mean(d_utm$depth_log)) %>%
  mutate(depth_scaled = depth_centred / sd(d_utm$depth_centred))

# grid_utm$source_data <- grid_utm$survey # can use this if you use original hybrid grid
# # if we want to predict for HBLL catch rates across full grid
grid_utm$survey <- "HBLL"



# Prediction grid that covers full outside S HBLL
full_s_grid_utm <- grid_utm %>% filter(longitude < -124.9)
full_s_grid_sf <- st_as_sf(full_s_grid_utm, coords = c("longitude", "latitude"))
st_crs(full_s_grid_sf) <- 4326 # set the coordinate reference system
full_s_grid1 <- sfc_as_cols(full_s_grid_sf, c("longitude", "latitude"))
st_geometry(full_s_grid1) <- NULL


# Trim prediction grid for area 5A
s_5A_grid_utm <- filter(grid_utm, latitude < 51.24999 & latitude > 50.5) # trims grid to be just area 5A
s_5A_grid_sf <- st_as_sf(s_5A_grid_utm, coords = c("longitude", "latitude"))
st_crs(s_5A_grid_sf) <- 4326 # set the coordinate reference system
s_5A_grid <- sfc_as_cols(s_5A_grid_sf, c("longitude", "latitude"))
st_geometry(s_5A_grid) <- NULL
s_5A_grid$region <- "5A"
saveRDS(s_5A_grid, file = "report-data/filled_5A_grid_paired.rds")


# Split area 3CD into areas inside and outside court defined area (CDA)
s_3CD_grid_utm <- filter(grid_utm, latitude < 50.5 & longitude < -124.9) # trims grid to be just area 3CD
s_3CD_grid_sf <- st_as_sf(s_3CD_grid_utm, coords = c("longitude", "latitude"))
st_crs(s_3CD_grid_sf) <- 4326 # set the coordinate reference system

focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp", layer = "taaqwiihak_areaVer2")
focal_area <- sf::st_transform(focal_area, crs = 4326)

sf_use_s2(FALSE)
intersected <- sf::st_intersects(s_3CD_grid_sf, focal_area)
sf_use_s2(TRUE)

cda_grid_sf <- s_3CD_grid_sf[which(lengths(intersected) > 0), ]
noncda_3CD_grid_sf <- s_3CD_grid_sf[which(lengths(intersected) == 0), ]


only_3CD_grid <- sfc_as_cols(s_3CD_grid_sf, c("longitude", "latitude"))
saveRDS(only_3CD_grid, file = "report-data/filled_3CD_grid_paired.rds")
cda_grid <- sfc_as_cols(cda_grid_sf, c("longitude", "latitude"))
noncda_3CD_grid <- sfc_as_cols(noncda_3CD_grid_sf, c("longitude", "latitude"))
st_geometry(cda_grid) <- NULL
st_geometry(noncda_3CD_grid) <- NULL
cda_grid$region <- "CDA"
noncda_3CD_grid$region <- "non-CDA 3CD"
saveRDS(cda_grid, file = "report-data/filled_cda_grid_paired.rds")
saveRDS(noncda_3CD_grid, file = "report-data/filled_noncda_3CD_grid_paired.rds")


# Area 3CD5A outside CDA
noncda_grid <- bind_rows(noncda_3CD_grid, s_5A_grid)
noncda_grid$region <- "non-CDA 3CD5A"
saveRDS(noncda_grid, file = "report-data/filled_noncda_grid_paired.rds")

noncda_gridN <- filter(noncda_grid, latitude > 50)
noncda_gridN$region <- "3CD5A N"
noncda_gridS <- filter(noncda_grid, latitude <= 50)
noncda_gridS$region <- "non-CDA 3CD5A S"


# Grids for different management regions
library(PBSmapping) # needs this for some reason
library(maptools)
majorbound <- load_boundaries(9)

ggplot() + geom_polygon(data = majorbound,  aes(X * 1000, Y * 1000,
                                                group = PID), colour = "grey", lty = 1, fill = NA)

# attributes(majorbound)$PolyData

bound3C <- fortify(majorbound) %>% mutate(X = X * 1000, Y = Y * 1000) %>%
  filter(PID %in% c(3)) %>% as.PolySet(., projection = "UTM", zone = 9)

bound3D <- fortify(majorbound) %>% mutate(X = X * 1000, Y = Y * 1000) %>%
  filter(PID %in% c(4)) %>% as.PolySet(., projection = "UTM", zone = 9)

bound5A <- fortify(majorbound) %>% mutate(X = X * 1000, Y = Y * 1000) %>%
  filter(PID %in% c(5)) %>% as.PolySet(., projection = "UTM", zone = 9)
bound5B <- fortify(majorbound) %>% mutate(X = X * 1000, Y = Y * 1000) %>%
  filter(PID %in% c(6)) %>% as.PolySet(., projection = "UTM", zone = 9)

poly3C <- PolySet2SpatialPolygons(bound3C, close_polys=TRUE)
poly3C_sf <-  st_as_sf(poly3C)
st_crs(poly3C_sf) <- 3156
poly3C_sf <- sf::st_transform(poly3C_sf, crs = 4326)
sf_use_s2(F)
intersected3C <- sf::st_intersects(full_s_grid_sf, poly3C_sf)
sf_use_s2(T)
poly3C_grid_sf <- full_s_grid_sf[which(lengths(intersected3C) > 0), ]
poly3C_grid <- sfc_as_cols(poly3C_grid_sf, c("longitude", "latitude"))
st_geometry(poly3C_grid) <- NULL
poly3C_grid$region <- "3C"
poly3D <- PolySet2SpatialPolygons(bound3D, close_polys=TRUE)
poly3D_sf <-  st_as_sf(poly3D)
st_crs(poly3D_sf) <- 3156
poly3D_sf <- sf::st_transform(poly3D_sf, crs = 4326)
sf_use_s2(F)
intersected3D <- sf::st_intersects(full_s_grid_sf, poly3D_sf)
sf_use_s2(T)
poly3D_grid_sf <- full_s_grid_sf[which(lengths(intersected3D) > 0), ]
poly3D_grid <- sfc_as_cols(poly3D_grid_sf, c("longitude", "latitude"))
st_geometry(poly3D_grid) <- NULL
poly3D_grid$region <- "3D"

poly5A <- PolySet2SpatialPolygons(bound5A, close_polys=TRUE)
poly5A_sf <-  st_as_sf(poly5A)
st_crs(poly5A_sf) <- 3156
poly5A_sf <- sf::st_transform(poly5A_sf, crs = 4326)
sf_use_s2(F)
intersected5A <- sf::st_intersects(full_s_grid_sf, poly5A_sf)
sf_use_s2(T)
poly5A_grid_sf <- full_s_grid_sf[which(lengths(intersected5A) > 0), ]
poly5A_grid <- sfc_as_cols(poly5A_grid_sf, c("longitude", "latitude"))
st_geometry(poly5A_grid) <- NULL
poly5A_grid$region <- "5A"

poly5B <- PolySet2SpatialPolygons(bound5B, close_polys=TRUE)
poly5B_sf <-  st_as_sf(poly5B)
st_crs(poly5B_sf) <- 3156
poly5B_sf <- sf::st_transform(poly5B_sf, crs = 4326)
sf_use_s2(F)
intersected5B <- sf::st_intersects(full_s_grid_sf, poly5B_sf)
sf_use_s2(T)
poly5B_grid_sf <- full_s_grid_sf[which(lengths(intersected5B) > 0), ]
poly5B_grid <- sfc_as_cols(poly5B_grid_sf, c("longitude", "latitude"))
st_geometry(poly5B_grid) <- NULL
poly5B_grid$region <- "5B"

ggplot() + geom_sf(data = poly3C_sf, colour = "blue", lty = 2, fill = NA) +
  geom_sf(data = poly3D_sf, colour = "green", lty = 2, fill = NA) +
  geom_sf(data = poly5A_sf, colour = "red", lty = 2, fill = NA) +
  geom_sf(data = poly5B_sf, colour = "yellow", lty = 2, fill = NA)


# full_s_grid_sf_utm <- st_transform(full_s_grid_sf, crs = 3156)
# intersected3C <- sf::st_intersects(full_s_grid_sf_utm, poly3C_sf)
# poly3C_grid_sf <- full_s_grid_sf_utm[which(lengths(intersected3C) > 0), ]

ggplot() + geom_sf(data = poly3C_sf, colour = "blue", lty = 2, fill = NA) +
  geom_sf(data = poly3D_sf, colour = "green", lty = 2, fill = NA) +
  geom_sf(data = poly5A_sf, colour = "red", lty = 2, fill = NA) +
  geom_sf(data = poly5B_sf, colour = "yellow", lty = 2, fill = NA) +
  geom_sf(data = poly3C_grid_sf, colour = "blue", lty = 2, fill = NA) +
  geom_sf(data = poly3D_grid_sf, colour = "green", lty = 2, fill = NA) +
  geom_sf(data = poly5A_grid_sf, colour = "red", lty = 2, fill = NA) +
  geom_sf(data = poly5B_grid_sf, colour = "yellow", lty = 2, fill = NA)


poly5A_grid <- poly5A_grid %>% filter(!(latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6))
regions_grid <- bind_rows(poly3C_grid, poly3D_grid, poly5A_grid, poly5B_grid)

saveRDS(regions_grid, file = "report-data/filled_regions_grid.rds")



# Just the HBLL S grid

hbll_s_grid <- readRDS("data-generated/full_hybrid_grid_w_substrate.rds") %>%
  filter(survey == "HBLL")
hbll_s_grid <- hbll_s_grid[complete.cases(hbll_s_grid), ]
hbll_s_grid <- expand_prediction_grid(hbll_s_grid, years = years) %>%
  mutate(depth_centred = log(depth) - mean(d_utm$depth_log)) %>%
  mutate(depth_scaled = depth_centred / sd(d_utm$depth_centred))



# Merge back into one grid with regions

noncda_gridS_named <- select(noncda_gridS, X, Y, region)
noncda_gridN_named <- select(noncda_gridN, X, Y, region)
cda_grid_named <- select(cda_grid, X, Y, region)

region_names <- bind_rows(noncda_gridS_named ,noncda_gridN_named, cda_grid_named) %>% distinct()
full_s_grid <- left_join(full_s_grid1, region_names)

# remove two partial cells within the Scott Islands RCA
full_s_grid <- full_s_grid %>% filter(!(latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6))

full_s_grid[is.na(full_s_grid$region), ]$region <- "other"
saveRDS(full_s_grid, file = "report-data/full_filled_grid_paired.rds")


# determine how to exclude partial cells within Scott Island RCA
full_s_grid2 <- filter(full_s_grid, area < 4000000)
full_s_grid3 <- filter(full_s_grid,
                       latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6)

full_s_grid3$area/1000000

ggplot() + geom_point(data = filter(full_s_grid2,
                                    latitude < 50.9 & latitude > 50.5 & longitude > -128.99 & longitude < -128.5
), aes(longitude, latitude),
colour = "grey85", fill = "grey85") +
  geom_point(data = filter(full_s_grid,
                           latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6
  ), aes(longitude, latitude, size = area))


f <- paste0("figs/region-colour-map-3x3-expanded.png")
if (!file.exists(f)) {
  full_s_grid2 <- full_s_grid

  full_s_grid2$region <- ordered(full_s_grid2$region,
                                 levels = c("CDA", "other", "3CD5A N", "non-CDA 3CD5A S"),
                                 labels = c("CDA", "non-CDA 3CD5A", "3CD5A N of 50º", "non-CDA 3CD5A S of 50º"))

  g1 <- ggplot(full_s_grid2, aes(X,Y, colour = region)) + geom_point() +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer("Sub-region", palette = "Set1")

  leg <- cowplot::get_legend(g1)

  min_map_lat <- 48.3
  # max_map_lat <- 51.4
  min_map_lon <- -130.1
  max_map_lon <- -124.85

  g <- map_predictions(
    pred_data = full_s_grid2,
    fill_aes = region,
    fill_lab = "Region",
    map_lat_limits = c(min_map_lat, 51.25),
    map_lon_limits = c(min_map_lon, max_map_lon)
  )+ theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1")
  g <- g + leg + patchwork::plot_layout(widths = c(1, 0.55))
  ggsave("figs/region-colour-map-3x3-expanded.png",
         width = 5, height = 3, dpi = 300)
}


### Make map of grids
# make map of grid areas and their overlap
f <- "figs/map-stitched-grid-overlap-expanded.png"

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
    scale_fill_viridis_d(name = "Survey", begin = 0.2, end = 0.5, direction = -1) +
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
  # g
  ggsave("figs/map-stitched-grid-overlap-expanded.png", width = 5.5, height = 5.5, dpi = 400)
}

