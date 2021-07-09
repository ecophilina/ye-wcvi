library(tidyverse)

library(rgdal)
library(raster)

# library(sf)
# retrieve substate layers
rocky <- raster("data/substrate/rocky.tif")

# original projection
proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0"
projdefs <- "+datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
geoCRS <- paste( proj, projdefs, sep=" " )

# new projection
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"

new_extent <- raster::projectExtent(rocky, crs = proj)
raster::res(new_extent) <- 100

# # Project values to new raster
hres_substrate <-raster::projectRaster(rocky, new_extent) #, method = "ngb")
writeRaster(hres_substrate, file = "data/highres-rocky-raster.grd")
substrate <-raster("data/highres-rocky-raster.grd")

coords1 <- as.data.frame(raster::rasterToPoints(substrate))[, c("x", "y")]

coords1$rocky <- as.data.frame(raster::rasterToPoints(substrate))[, 3]

ggplot(coords1,  aes(x, y, col = rocky)) + geom_point()


# Add substrate columns to event data

# put event data in sf form
events <- readRDS("data-generated/halibut-hybrid-model-data-paired.rds")

# new projection
proj <- "+proj=utm +datum=NAD83 +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# make spatial points dataframe
pts <- cbind(events$X*100000, events$Y*100000)
xx <- SpatialPointsDataFrame(pts, events, proj = CRS(proj))
sp <- sp::SpatialPoints(pts)


events <- events %>% mutate (X2 = X*100000, Y2 = Y*100000)

# events_sf <- st_as_sf(events, coords = c("X2", "Y2"))

# st_crs(full_s_grid_sf) <- 4326 # set the coordinate reference system

# calculate proportion of each substrate within 1 km radius of each fishing event
events_w_substrate <- raster::extract(substrate, xx,
  buffer=1000, fun = mean, na.rm=TRUE,
  df=TRUE, sp = TRUE
  )

saveRDS(events_w_substrate, file = "data-generated/events-w-rocky.rds")

###################

# add substrate data to prediction grid

library(sf)

# put prediction data in raster form

substrate <-raster("data/highres-rocky-raster.grd")
nd_all <- readRDS(file = "data-generated/full_hybrid_grid_paired.rds")

nd_hbll <- nd_all %>% filter(year %in% c(2008) & survey == "HBLL") %>%
  mutate (x = round(X*100000), y = round(Y*100000), z = depth)# %>% dplyr::select(x, y)
  # mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms

nd_trawl <- nd_all %>% filter(year %in% c(2008) & survey == "TRAWL") %>%
  mutate (x = round(X*100000), y = round(Y*100000), z = depth)

nd_hbll_sf <- st_as_sf(nd_hbll, coords = c("x", "y"))
st_crs(nd_hbll_sf) <- CRS(proj)
nd_trawl_sf <- st_as_sf(nd_trawl, coords = c("x", "y"))
st_crs(nd_trawl_sf) <- CRS(proj)

# # for just centre points , could add buffer
# sets_sP <- SpatialPoints(nd1 %>% dplyr::select(x, y), proj4string = crs(substrate))
# extracted_env_lines <- raster::extract(x = substrate, y = sets_sP)
# extracted_env_lines <- cbind(nd1, extracted_env_lines)
#

grid_scale <- 1000 #scale of prediction grid in m
predict_grid <- raster(ext = extent(nd_trawl)#+c(-1000, 1000, -1000, 1000)
  , resolution = grid_scale, crs = crs(nd_trawl_sf))
predict_grid.df <- as.data.frame(predict_grid, xy = TRUE)
predict_grid.df$layer <- NULL

predict_grid_substrate <- raster::extract(x = substrate, y = predict_grid.df)
extracted_substrate <- cbind(predict_grid.df, predict_grid_substrate) %>% mutate()

# ggplot(extracted_substrate,  aes(x, y, col = predict_grid_substrate)) + geom_point() +
#   scale_colour_viridis_c(trans= "sqrt", na.value = "white") + ggsidekick::theme_sleek()

nd2 <- extracted_substrate %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)
nd_trawl2 <- left_join(nd_trawl, nd2) #%>% filter( year == 2008)


predict_grid <- raster(ext = extent(nd_hbll)#+c(-1001, 1001, -1001, 1001)
  , resolution = grid_scale, crs = crs(nd_hbll_sf))
predict_grid.df <- as.data.frame(predict_grid, xy = TRUE)
predict_grid.df$layer <- NULL

predict_grid_substrate <- raster::extract(x = substrate, y = predict_grid.df)
extracted_substrate2 <- cbind(predict_grid.df, predict_grid_substrate) %>% mutate()

nd3 <- extracted_substrate2 %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)
nd_hbll_even <- left_join(nd_hbll, nd3) %>% filter(!is.na(predict_grid_substrate))

range(nd1$X)
range(nd2$X)
range(nd3$X)
range(nd1$Y)
range(nd2$Y)
range(nd3$Y)

# Get predictions for odd number HBLL
predict_grid <- raster(ext = extent(nd_hbll)+c(-500, 500, -500, 500)
  , resolution = grid_scale, crs = crs(nd_hbll_sf))
predict_grid.df <- as.data.frame(predict_grid, xy = TRUE)
predict_grid.df$layer <- NULL

predict_grid_substrate <- raster::extract(x = substrate, y = predict_grid.df)
extracted_substrate3 <- cbind(predict_grid.df, predict_grid_substrate) %>% mutate()

nd4 <- extracted_substrate3 %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)
nd_hbll_odd <- left_join(nd_hbll, nd4) %>% filter(!is.na(predict_grid_substrate))

nd_new <- bind_rows(nd_hbll_odd,nd_hbll_even,nd_trawl2) %>% dplyr::select (-year) %>% distinct()

ggplot(nd_new,  aes(X, Y, fill = predict_grid_substrate)) + geom_tile(alpha = 0.5, width = 0.02, height = 0.02) +
  scale_fill_viridis_c(trans= "sqrt", na.value = "red") + ggsidekick::theme_sleek()

nd_new <- left_join(nd_all, nd_new) %>% distinct() %>% rename(rocky = predict_grid_substrate)

saveRDS(nd_new, file = "data-generated/full-hybrid-grid-w-rocky.rds")
