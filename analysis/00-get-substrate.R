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
events <- readRDS("data-generated/halibut-model-data-keepable-weight.rds")

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

