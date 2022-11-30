focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
                          layer = "taaqwiihak_areaVer2", quiet = TRUE)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

proj1 =  crs(focal_area_proj)
proj2 <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs" # I believe this is 3156

cda <- st_as_sf(focal_area_proj)


# bat <- readRDS(here::here("data/bath_res1_marmap.rds"))
bat <- readRDS(here::here("data/bath_res1_marmap2.rds"))
rast_bat <- marmap::as.raster(bat)

# change projection to match grid
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
rast_bat <- projectRaster(rast_bat, crs = proj)

rast_bat[rast_bat > 0] <- NA

plot(rast_bat)

rast_bat2 <- marmap::as.raster(bat)
rast_bat2 <- projectRaster(rast_bat2, crs = proj)
rast_water_area <- rast_bat2
rast_water_area[rast_bat2 <  0] <- 1
rast_water_area[rast_bat2 >=  0] <- 0

plot(rast_water_area)

grid.df <- as.data.frame(focal_area_proj, xy = TRUE)
grid.df$layer <- NULL
mean_depth <- raster::extract(x=rast_bat, y=focal_area_proj, fun = mean, na.rm = T)

median_depth <- raster::extract(x=rast_bat, y=focal_area_proj, fun = median, na.rm = T)
max_depth <- raster::extract(x=rast_bat, y=focal_area_proj, fun = min, na.rm = T)
min_depth <- raster::extract(x=rast_bat, y=focal_area_proj, fun = max, na.rm = T)


