library(sf)
library(terra)
library(raster)
library(tidyverse)

# load misc custom functions
source("analysis/functions.R")

grid_scale <- 1000

focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
                          layer = "taaqwiihak_areaVer2", quiet = TRUE)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

# proj1 <- crs(focal_area_proj)
# proj2 <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs" # I believe this is 3156

cda <- st_as_sf(focal_area_proj)

# bat <- readRDS(here::here("data/bath_res1_marmap.rds"))
bat <- readRDS(here::here("data/bath_res1_marmap2.rds"))
rast_bat <- marmap::as.raster(bat)

# change projection to match grid
# proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"

proj <- 3156
rast_bat <- projectRaster(rast_bat, crs = proj)

rast_bat[rast_bat > 0] <- NA

rast_bat[rast_bat < -1000] <- NA

plot(rast_bat)

rast_bat2 <- marmap::as.raster(bat)
rast_bat2 <- projectRaster(rast_bat2, crs = proj)
rast_water_area <- rast_bat2
rast_water_area[rast_bat2 < 0] <- 1
rast_water_area[rast_bat2 >= -2] <- NA

plot(rast_water_area)

water <- rasterToPolygons(rast_water_area, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
water <- st_as_sf(water)


rcas <- st_read(here::here("grids/RCA2019/RCA_2019.shp"))
rcas_utm <- st_transform(rcas, crs = 3156)

cells_in_rcas <- st_intersection(focal_area_proj, rcas_utm)

#not sure why, but st_union required otherwise df becomes huge!

focal_area_offshore <- st_intersection(focal_area_proj, st_union(water))
plot(focal_area_offshore)

focal_area_proj1 <- sf::st_difference(focal_area_offshore, st_union(cells_in_rcas))
plot(focal_area_proj1)

# for cda
mean_depth <- raster::extract(x=rast_bat, y=focal_area_proj1, fun = mean, na.rm = T)
median_depth <- raster::extract(x=rast_bat, y=focal_area_proj1, fun = median, na.rm = T)
max_depth <- raster::extract(x=rast_bat, y=focal_area_proj1, fun = min, na.rm = T)
min_depth <- raster::extract(x=rast_bat, y=focal_area_proj1, fun = max, na.rm = T)

write_tex(round(-mean_depth), "cdameandepth")
write_tex(round(-median_depth), "cdamediandepth")
write_tex(round(-max_depth), "cdamaxdepth")
write_tex(round(-min_depth), "cdamindepth")


# load previously saved raster
rocky <- raster("data/highres-rocky-raster.grd")
mixed <- raster("data/highres-mixed-raster.grd")
muddy <- raster("data/highres-muddy-raster.grd")

mean_rocky <- raster::extract(x=rocky, y=focal_area_proj1, fun = mean, na.rm = T)
write_tex(round(mean_rocky, 2)*100, "cdameanrocky")

mean_muddy <- raster::extract(x=muddy, y=focal_area_proj1, fun = mean, na.rm = T)
write_tex(round(mean_muddy, 3)*100, "cdameanmuddy")

mean_mixed <- raster::extract(x=mixed, y=focal_area_proj1, fun = mean, na.rm = T)
write_tex(round(mean_mixed, 3)*100, "cdameanmixed")

# for cda adjacent
focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")


focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)
focal_area2_proj <-  st_as_sf(focal_area2_proj)
focal_area_proj <-  st_as_sf(focal_area_proj)

rast_water_area2 <- rast_water_area
rast_water_area2[rast_bat2 >= -50] <- NA

plot(rast_water_area2)

water2 <- rasterToPolygons(rast_water_area2, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
water2 <- st_as_sf(water2)

focal_area2_offshore <- st_intersection(focal_area2_proj, st_union(water2))
plot(focal_area2_offshore)


focal_area2_proj2 <- sf::st_difference(focal_area2_offshore, focal_area_proj)
plot(focal_area2_proj2)

mean_depth2 <- raster::extract(x=rast_bat, y=focal_area2_proj2, fun = mean, na.rm = T)
median_depth2 <- raster::extract(x=rast_bat, y=focal_area2_proj2, fun = median, na.rm = T)
max_depth2 <- raster::extract(x=rast_bat, y=focal_area2_proj2, fun = min, na.rm = T)
min_depth2 <- raster::extract(x=rast_bat, y=focal_area2_proj2, fun = max, na.rm = T)


write_tex(round(-mean_depth2), "adjmeandepth")
write_tex(round(-median_depth2), "adjmediandepth")
write_tex(round(-max_depth2), "adjmaxdepth")
write_tex(round(-min_depth2), "adjmindepth")

mean_rocky2 <- raster::extract(x=rocky, y=focal_area2_proj2, fun = mean, na.rm = T)
write_tex(round(mean_rocky2, 3)*100, "adjmeanrocky")

mean_muddy2 <- raster::extract(x=muddy, y=focal_area2_proj2, fun = mean, na.rm = T)
write_tex(round(mean_muddy2, 2)*100, "adjmeanmuddy")

mean_mixed2 <- raster::extract(x=mixed, y=focal_area2_proj2, fun = mean, na.rm = T)
write_tex(round(mean_mixed2, 3)*100, "adjmeanmixed")



# for 3cd

focal_area3 <- sf::st_read("shape-files/Shapes/majorOutline.shp") %>% dplyr::filter(Name %in% c("3C", "3D"))
focal_area3_proj <- sf::st_transform(focal_area3, crs = 3156)
focal_area3_proj <-  st_as_sf(focal_area3_proj)
# plot(focal_area3_proj)



cells_in_rcas2 <- st_intersection(focal_area3_proj, rcas_utm)

#not sure why, but st_union required otherwise df becomes huge!

focal_area3_offshore <- st_intersection(st_union(focal_area3_proj), st_union(water))
plot(focal_area3_offshore)

focal_area3_proj2 <- sf::st_difference(focal_area3_offshore, st_union(cells_in_rcas2))
plot(focal_area3_proj2)

focal_area3_proj3 <- sf::st_difference(focal_area3_proj2, focal_area2_proj)
plot(focal_area3_proj3)

# focal_area3_proj3 <- Reduce(st_union, focal_area3_proj3)
# focal_area3_proj3 <- st_union(focal_area3_proj3)
# focal_area3_proj3 <- st_combine(focal_area3_proj3) %>% st_cast("POLYGON")
focal_area3_proj3 <- st_as_sf(focal_area3_proj3)


mean_depth3 <- raster::extract(x=rast_bat, y=focal_area3_proj3, fun = mean, na.rm = T)
median_depth3 <- raster::extract(x=rast_bat, y=focal_area3_proj3, fun = median, na.rm = T)
# max_depth3 <- raster::extract(x=rast_bat, y=focal_area3_proj3, fun = min, na.rm = T)
# min_depth3 <- raster::extract(x=rast_bat, y=focal_area3_proj3, fun = max, na.rm = T)

write_tex(round(-mean_depth3), "restmeandepth")
write_tex(round(-median_depth3), "restmediandepth")
# write_tex(round(-max_depth3), "3cdmaxdepth")
# write_tex(round(-min_depth3), "3cdmindepth")

mean_rocky3 <- raster::extract(x=rocky, y=focal_area3_proj3, fun = mean, na.rm = T)
mean_muddy3 <- raster::extract(x=muddy, y=focal_area3_proj3, fun = mean, na.rm = T)
mean_mixed3 <- raster::extract(x=mixed, y=focal_area3_proj3, fun = mean, na.rm = T)

write_tex(round(mean_mixed3, 2)*100, "restmeanmixed")
write_tex(round(mean_muddy3, 2)*100, "restmeanmuddy")
write_tex(round(mean_rocky3, 2)*100, "restmeanrocky")

grid <- readRDS(file = paste0("data-generated/full_filled_grid_w_ext_", grid_scale, ".rds"))

filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(total_area = sum(cell_area/1e+6))


## See if the grid match overall means?
# filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(mean_depth = mean(depth),
#                                                                     median_depth = median(depth))
# filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(mean_rocky = mean(rocky))
# filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(mean_muddy = mean(muddy))
# filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(mean_mixed = mean(mixed))
# filter(grid, year == 2008) %>% group_by(region, year) %>% summarise(mean_sandy = mean(sandy))
