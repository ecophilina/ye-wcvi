library(tidyverse)

library(rgdal)
library(raster)

library(sf)

# library(sf)
# # retrieve substate layers if not done so before
# rocky <- raster("data/substrate/rocky.tif")
# mixed <- raster("data/substrate/mixed.tif")
# muddy <- raster("data/substrate/muddy.tif")
#
# # original projection
# proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0"
# projdefs <- "+datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# geoCRS <- paste( proj, projdefs, sep=" " )
#
# # new projection
# proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
#
# new_rocky <- raster::projectExtent(rocky, crs = proj)
# raster::res(new_rocky) <- 100
# new_mixed <- raster::projectExtent(mixed, crs = proj)
# raster::res(new_mixed) <- 100
# new_muddy <- raster::projectExtent(muddy, crs = proj)
# raster::res(new_muddy) <- 100
#
# # # Project values to new raster
# hres_rocky <- raster::projectRaster(rocky, new_rocky) #, method = "ngb")
# writeRaster(hres_rocky, file = "data/highres-rocky-raster.grd")
# hres_mixed <- raster::projectRaster(mixed, new_mixed) #, method = "ngb")
# writeRaster(hres_mixed, file = "data/highres-mixed-raster.grd")
# hres_muddy <- raster::projectRaster(muddy, new_muddy) #, method = "ngb")
# writeRaster(hres_muddy, file = "data/highres-muddy-raster.grd")
#


# Add substrate columns to event data
# put event data in sf form
events <- readRDS("data-generated/halibut-hybrid-model-data-paired.rds")

# load previously saved raster
rocky <- raster("data/highres-rocky-raster.grd")
mixed <- raster("data/highres-mixed-raster.grd")
muddy <- raster("data/highres-muddy-raster.grd")
## can be plotted but very very slow!
# coords1 <- as.data.frame(raster::rasterToPoints(rocky))[, c("x", "y")]
# coords1$rocky <- as.data.frame(raster::rasterToPoints(rocky))[, 3]
# ggplot(coords1,  aes(x, y, col = rocky)) + geom_point()
# coords1$mixed <- as.data.frame(raster::rasterToPoints(mixed))[, 3]
# ggplot(coords1,  aes(x, y, col = mixed)) + geom_point()

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
events_w_rocky <- raster::extract(rocky, xx,
  buffer=1000, fun = mean, na.rm=TRUE,
  df=TRUE, sp = TRUE
)

events_w_mixed <- raster::extract(mixed, xx,
  buffer=1000, fun = mean, na.rm=TRUE,
  df=TRUE, sp = TRUE
)

events_w_muddy <- raster::extract(muddy, xx,
  buffer=1000, fun = mean, na.rm=TRUE,
  df=TRUE, sp = TRUE
)

events_w_sub <- events_w_rocky@data
events_w_sub$mixed <- events_w_mixed@data$mixed
events_w_sub$any_rock <-  events_w_sub$rocky + events_w_sub$mixed
events_w_sub$muddy <- events_w_muddy@data$muddy

# saveRDS(events_w_sub, file = "data-generated/events_w_substrate_500m_buffer.rds")
saveRDS(events_w_sub, file = "data-generated/events_w_substrate_1km_buffer.rds")

ggplot(events_w_sub,  aes(X, Y, col = rocky)) + geom_point()
ggplot(events_w_sub,  aes(X, Y, col = mixed)) + geom_point()
ggplot(events_w_sub,  aes(X, Y, col = any_rock)) + geom_point()
ggplot(events_w_sub,  aes(X, Y, col = muddy)) + geom_point()


###################

# add substrate data to prediction grid

# new projection
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"

# put prediction data in raster form
nd_all <- readRDS(file = "data-generated/hybrid_grid.rds") #%>% filter(year %in% c(2008)) %>% dplyr::select(-depth_scaled, -depth_centred)
nd <- nd_all %>%
  mutate (x = round(X*100000), y = round(Y*100000), z = depth)# %>% dplyr::select(x, y)
# mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms

# ## checking none-overall of trawl and hbll grid portions
# nd_hbll <- nd_all %>% filter(year %in% c(2008) & survey == "HBLL") %>%
#   mutate (x = round(X*100000), y = round(Y*100000), z = depth)# %>% dplyr::select(x, y)
#   # mutate(X=X*1000, Y=Y*1000) # change utms to meters from Kms
#
# nd_trawl <- nd_all %>% filter(year %in% c(2008) & survey == "TRAWL") %>%
#   mutate (x = round(X*100000), y = round(Y*100000), z = depth)
#
# nd_hbll_sf <- st_as_sf(nd_hbll, coords = c("x", "y"))
# st_crs(nd_hbll_sf) <- CRS(proj)
# nd_trawl_sf <- st_as_sf(nd_trawl, coords = c("x", "y"))
# st_crs(nd_trawl_sf) <- CRS(proj)
#
# plot(st_geometry(nd_trawl_sf), cex =0.5)
# plot(st_geometry(nd_hbll_sf), col= "red", cex =0.5, add = T)
#

## grid making code used to discover error in hbll grid alignment
# cs <- c(2000, 2000)
#
# nd_trawl_grid <- sf::st_make_grid(x = nd_trawl_sf,
#   offset = st_bbox(nd_trawl_sf)[c("xmin", "ymin")]+ c(-1000, -1000),
#   cellsize=cs)
# keep <- st_intersects(nd_trawl_sf, nd_trawl_grid)
# nd_trawl_grid <- nd_trawl_grid[unlist(keep)]

# plot(st_geometry(nd_trawl_grid), col= "red")
# plot(st_geometry(nd_trawl_sf), col= "black", cex =0.5, add = T)

# nd_hbll_grid1 <- sf::st_make_grid(x = nd_hbll_sf,
#   offset = st_bbox(nd_hbll_sf)[c("xmin", "ymin")]+ c(-1000, -1000),
#   cellsize=cs)
# keep <- st_intersects(nd_hbll_sf, nd_hbll_grid1)
# nd_hbll_grid <- nd_hbll_grid1[unlist(keep)]
# # plot(st_geometry(nd_hbll_sf), cex =0.2)
# # plot(st_geometry(nd_trawl_grid1), add = T)
# plot(st_geometry(nd_hbll_grid), col= "red")
# plot(st_geometry(nd_hbll_sf), col= "black", cex =0.5, add = T)
#

grid_scale <- 2000 #scale of prediction grid in m

predict_grid <- raster(
  ext = extent(nd)+c(-1000, 1000, -1000, 1000), # need to add 1km in all directions to allow coordinates to be the centroids of 2x2km cells
  resolution = grid_scale, crs = CRS(proj))
predict_grid.df <- as.data.frame(predict_grid, xy = TRUE)
predict_grid.df$layer <- NULL


rocky <-raster("data/highres-rocky-raster.grd")
predict_grid_rocky <- raster::extract(x = rocky, y = predict_grid.df)
extracted_rocky <- cbind(predict_grid.df, predict_grid_rocky)
nd_rocky <- extracted_rocky %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)

mixed <-raster("data/highres-mixed-raster.grd")
predict_grid_mixed <- raster::extract(x = mixed, y = predict_grid.df)
extracted_mixed <- cbind(predict_grid.df, predict_grid_mixed)
nd_mixed <- extracted_mixed %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)

muddy <-raster("data/highres-muddy-raster.grd")
predict_grid_muddy <- raster::extract(x = muddy, y = predict_grid.df)
extracted_muddy <- cbind(predict_grid.df, predict_grid_muddy)
nd_muddy <- extracted_muddy %>% mutate(X = x/1000, Y = y/1000) %>% mutate(X = round(X)/100, Y = round(Y)/100)%>% dplyr::select(-x, -y)


nd_new <- left_join(nd_all, nd_rocky) %>% left_join(nd_mixed) %>% left_join(nd_muddy) %>%
  distinct() %>%
  rename(rocky = predict_grid_rocky, mixed = predict_grid_mixed, muddy = predict_grid_muddy) %>%
  mutate(any_rock = rocky + mixed)


ggplot(nd_new,  aes(X, Y, fill = any_rock)) + geom_tile(alpha = 0.75, width = 0.02, height = 0.02) +
  scale_fill_viridis_c(trans= "sqrt", na.value = "red") + ggsidekick::theme_sleek()

ggplot(nd_new,  aes(X, Y, fill = muddy)) + geom_tile(alpha = 0.75, width = 0.02, height = 0.02) +
  scale_fill_viridis_c(trans= "sqrt", na.value = "red") + ggsidekick::theme_sleek()

saveRDS(nd_new, file = "data-generated/full_hybrid_grid_w_substrate.rds")
