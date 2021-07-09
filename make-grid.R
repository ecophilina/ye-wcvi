
library(rgdal)
# setwd("grids/HBLL-N-S/")
# shape <- rgdal::readOGR(
#   dsn = ".",
#   layer = "PHMA_N_GRID", verbose = FALSE)
# plot(shape)
# head(shape@data)

library(dplyr)
# hbll_grid <- select(shape@data, LONGITUDE, LATITUDE, DEPTH_M) %>%
#   rename(X = LONGITUDE, Y = LATITUDE, depth = DEPTH_M)
# hbll_grid <- mutate(hbll_grid, depth = -depth) %>%
#   filter(depth > 0)
# setwd("../../../")
# hbll_n_grid <- list(grid = hbll_grid, cell_area = 2.0)
# usethis::use_data(hbll_n_grid, internal = FALSE, overwrite = TRUE)

# South:
setwd("grids/HBLL-N-S/")
shape <- rgdal::readOGR(
  dsn = ".",
  layer = "PHMA_S_GRID", verbose = FALSE)
plot(shape)

proj1 =  sp::proj4string(shape)
proj2 <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs" # i believe this is 3156


hbll_sf <- st_as_sf(shape, coords = c("LONGITUDE", "LATITUDE"))

hbll_sf_utm <- st_transform(hbll_sf, crs = 3156)
hbll_sf_utm$geometry <- st_centroid(hbll_sf_utm$geometry) %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

hbll_sf_ll <- st_transform(hbll_sf, crs = 4326)
hbll_sf_ll$geometry <- st_centroid(hbll_sf_ll$geometry) %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

# hbll_sf3$geometry[1]

# plot(st_geometry(hbll_sf))
plot(st_geometry(hbll_sf_ll))
plot(st_geometry(hbll_sf_utm))
plot(st_geometry(hbll_sf_utm), cex = 0.2, add = T)

hbll_s_grid_ll <- sfc_as_cols(hbll_sf_ll, c("x", "y"))
st_geometry(hbll_s_grid_ll) <- NULL

hbll_s_grid_utm <- sfc_as_cols(hbll_sf_utm, c("x", "y"))
st_geometry(hbll_s_grid_utm) <- NULL

hbll_grid <- dplyr::select(hbll_s_grid_ll, x, y, LONGITUDE, LATITUDE, DEPTH_M) %>%
  rename(longitude = x, latitude = y, old_lon = LONGITUDE, old_lat = LATITUDE, depth = DEPTH_M)

hbll_grid_utm <- dplyr::select(hbll_s_grid_utm, x, y, LONGITUDE, LATITUDE, DEPTH_M) %>%
  rename(old_lon = LONGITUDE, old_lat = LATITUDE, depth = DEPTH_M) %>% mutate(X = x/ 100000, Y = y/ 100000)

hbll_grid <- left_join(hbll_grid, hbll_grid_utm)

hbll_grid <- mutate(hbll_grid, depth = -depth) %>%
  filter(depth > 0)

hbll_s_grid <- list(grid = hbll_grid, cell_area = 2.0)
saveRDS(hbll_s_grid, here::here("grids/hbll_s_grid.rds"))



