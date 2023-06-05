# Grids for different management regions
library(PBSmapping) # needs this for some reason
library(maptools)

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

# select which scale of grid to use
# grid_scale <- 2000
grid_scale <- 1000

# get management region boundaries
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

bound3CD <- fortify(majorbound) %>% mutate(X = X * 1000, Y = Y * 1000) %>%
  filter(PID %in% c(3, 4)) %>% as.PolySet(., projection = "UTM", zone = 9)


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
  left_join(select(substrate, -X, -Y)) # looks like no missing data to worry about

if(sum(is.na(d_utm$muddy))) { stop("Check for missing substrate data before running models.")}

# check depth range and use as starting point for filtering grid
ggplot(d_utm) + geom_histogram(aes(depth_m)) + scale_x_log10(breaks=c(25, 50, 100, 200, 400, 600, 800, 1000))

f2 <- paste0("grids/norca_grid_w_substrate_expanded", grid_scale, ".rds")

if(!file.exists(f2)) {
  stop("Need to run make-grid.Rmd")
} else {
  grid <- readRDS(file = f2) %>%
    filter(depth > 2 & depth < 600) # produces best match to combination of HBLL S and trawl grids
}
# grid <- grid[complete.cases(grid), ] # run this if only wanting complete 2x2 km cells
years <- sort(unique(d_utm$year))

grid_utm <- expand_prediction_grid(grid, years = years) %>%
  # mutate(any_rock_scaled = (any_rock - mean(substrate$any_rock)/ sd(substrate$any_rock)) %>%
  mutate(depth_centred = log(depth) - mean(d_utm$depth_log)) %>%
  mutate(depth_scaled = depth_centred / sd(d_utm$depth_centred))

# # if we want to predict for HBLL catch rates across full grid
grid_utm$survey <- "HBLL"



# Prediction grid that covers full outside S HBLL
full_s_grid_utm <- grid_utm %>% filter(longitude < -124.9)
full_s_grid_sf <- st_as_sf(full_s_grid_utm, coords = c("longitude", "latitude"))
st_crs(full_s_grid_sf) <- 4326 # set the coordinate reference system
full_s_grid1 <- sfc_as_cols(full_s_grid_sf, c("longitude", "latitude"))
st_geometry(full_s_grid1) <- NULL


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
saveRDS(poly3C_grid, file = paste0("data-generated/filled_3D_grid_paired_", grid_scale, ".rds"))

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
saveRDS(poly3D_grid, file = paste0("data-generated/filled_3D_grid_paired_", grid_scale, ".rds"))

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
# replace old one with one based on actual boundary
saveRDS(poly5A_grid, file = paste0("data-generated/filled_5A_grid_paired_", grid_scale, ".rds"))

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
saveRDS(poly5B_grid, file = paste0("data-generated/filled_5B_grid_paired_", grid_scale, ".rds"))


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

saveRDS(regions_grid, file = paste0("data-generated/filled_regions_grid_", grid_scale, ".rds"))

# add in CDA boundaries

focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp", layer = "taaqwiihak_areaVer2")
focal_area <- sf::st_transform(focal_area, crs = 4326)

focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
focal_area2 <- sf::st_transform(focal_area2, crs = 4326)
# plot(focal_area2)
# combine 3C and 3D

poly3CD <- PolySet2SpatialPolygons(bound3CD, close_polys=TRUE)
poly3CD_sf <-  st_as_sf(poly3CD)
st_crs(poly3CD_sf) <- 3156
poly3CD_sf <- sf::st_transform(poly3CD_sf, crs = 4326)
sf_use_s2(F)
intersected3CD <- sf::st_intersects(full_s_grid_sf, poly3CD_sf)
sf_use_s2(T)
poly3CD_grid_sf <- full_s_grid_sf[which(lengths(intersected3CD) > 0), ]
poly3CD_grid <- sfc_as_cols(poly3CD_grid_sf, c("longitude", "latitude"))
st_geometry(poly3CD_grid) <- NULL
poly3CD_grid$region <- "3CD"
saveRDS(poly3CD_grid, file = paste0("data-generated/filled_3CD_grid_paired_", grid_scale, ".rds"))


sf_use_s2(FALSE)
intersected <- sf::st_intersects(poly3CD_grid_sf, focal_area)
intersected2 <- sf::st_intersects(poly3CD_grid_sf, focal_area2)
sf_use_s2(TRUE)

cda_grid_sf <- poly3CD_grid_sf[which(lengths(intersected) > 0), ]
noncda_3CD_grid_sf <- poly3CD_grid_sf[which(lengths(intersected) == 0), ]
ext_grid_sf <- poly3CD_grid_sf[which(lengths(intersected2) > 0), ]
noncda_3CD_grid_sf <- poly3CD_grid_sf[which(lengths(intersected2) == 0), ]

cda_grid <- sfc_as_cols(cda_grid_sf, c("longitude", "latitude"))
ext_grid <- sfc_as_cols(ext_grid_sf, c("longitude", "latitude"))
noncda_3CD_grid <- sfc_as_cols(noncda_3CD_grid_sf, c("longitude", "latitude"))

st_geometry(cda_grid) <- NULL
st_geometry(ext_grid) <- NULL
st_geometry(noncda_3CD_grid) <- NULL


cda_grid$region <- "CDA"
ext_grid$region <- "CDA adjacent"
noncda_3CD_grid$region <- "non-CDA 3CD"
# saveRDS(cda_grid, file = "data-generated/filled_cda_grid_paired.rds")
# saveRDS(ext_grid, file = "data-generated/filled_cda_adj_grid_paired.rds")
# saveRDS(noncda_3CD_grid, file = "data-generated/filled_noncda_3CD_grid_paired.rds")

# Area 3CD5A outside CDA
# noncda_grid <- bind_rows(noncda_3CD_grid, poly5A_grid)
# noncda_grid$region <- "non-CDA 3CD5A"
# saveRDS(noncda_grid, file = "data-generated/filled_noncda_grid_paired.rds")


# make one grid with named sub regions
noncda_3CD_grid_named <- select(noncda_3CD_grid, X, Y, region)
poly5A_grid_named <- select(poly5A_grid, X, Y, region)
ext_grid_named <- select(ext_grid, X, Y, region)
cda_grid_named <- select(cda_grid, X, Y, region)

ext_grid_named2 <- anti_join(ext_grid_named, cda_grid_named, by=c("X","Y"))
noncda_3CD_grid_named2 <- anti_join(noncda_3CD_grid_named, ext_grid_named, by=c("X","Y"))

region_names <- bind_rows(cda_grid_named, ext_grid_named2, noncda_3CD_grid_named2, poly5A_grid_named) %>% distinct()
full_s_grid <- left_join(full_s_grid1, region_names)


# # # determine how to exclude partial cells within Scott Island RCA
# full_s_grid2 <- filter(full_s_grid, area < 4000000)
# full_s_grid3 <- filter(full_s_grid,
#                        latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6)
#
# full_s_grid3$area/1000000
#
# ggplot() + geom_point(data = filter(full_s_grid2,
#                                     latitude < 50.9 & latitude > 50.5 & longitude > -128.99 & longitude < -128.5
# ), aes(longitude, latitude),
# colour = "grey85", fill = "grey85") +
#   geom_point(data = filter(full_s_grid,
#                            latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6
#   ), aes(longitude, latitude, size = area))
#
#

# remove two partial cells within the Scott Islands RCA
full_s_grid <- full_s_grid %>% filter(!(latitude < 50.85 & latitude > 50.75 & longitude > -128.94 & longitude < -128.6))

full_s_grid[is.na(full_s_grid$region), ]$region <- "other"
saveRDS(full_s_grid, file = paste0("data-generated/full_filled_grid_w_ext_", grid_scale, ".rds"))


f <- paste0("figs/region-colour-map-3x3-expanded-",grid_scale,".png")
if (!file.exists(f)) {
  full_s_grid2 <- readRDS(file = paste0("data-generated/full_filled_grid_w_ext_", grid_scale, ".rds")) %>%
    filter(region != "other")
  unique(full_s_grid2$region)
  # full_s_grid2$region <- ordered(full_s_grid2$region,
  #                                levels = c("CDA", "other", "3CD5A N", "non-CDA 3CD5A S"),
  #                                labels = c("CDA", "non-CDA 3CD5A", "3CD5A N of 50º", "non-CDA 3CD5A S of 50º"))
  full_s_grid2$region <- ordered(full_s_grid2$region,
                                 levels = c("CDA", "non-CDA 3CD", #"other",
                                            "5A", "CDA adjacent"),
                                 labels = c("CDA", "non-CDA 3CD", #"other",
                                            "5A", "CDA adjacent"))

  (g1 <- ggplot(full_s_grid2, aes(X,Y, colour = region)) + geom_point() +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer("Sub-region", palette = "Set1"))

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
  ggsave(paste0("figs/region-colour-map-3x3-expanded-", grid_scale, ".png"),
         width = 5, height = 3, dpi = 300)
}

