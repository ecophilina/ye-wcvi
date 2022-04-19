# this is a script to get proportion water and mean depths for a predictions grid
# it must be converted to a shape file if not already one

if(!require(marmap))install.packages('marmap');library(marmap)
if(!require(sf))install.packages('sf');library(sf)
if(!require(sp))install.packages('sp');library(sp) #coverting to UTMS
if(!require(rgdal))install.packages('rgdal');library(rgdal) #coverting to UTMS
if(!require(rgeos))install.packages('rgeos');library(rgeos)
if(!require(tidyverse))install.packages('tidyverse');library(tidyverse)
if(!require(raster))install.packages('raster');library(raster)

# set the UTM projection used for grids
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"

# Download bathymetric data and save on disk ####

### if your grid is a dataframe with lat lons ####
grid_df <- readRDS("hbll_s_grid.rds")[[1]]

# you will need to make a geometry in UTMS ####
sf_ll <- st_as_sf(grid_df, coords = c("longitude", "latitude"), crs = 4326)
sf_utm <- st_transform(sf_ll, crs = proj)

# convert this to a polygon grid ####
sf_grid <- sf::st_make_grid(x = sf_utm,
  offset = st_bbox(sf_utm)[c("xmin", "ymin")]+ c(-1000, -1000),
  cellsize=c(2000, 2000))
keep <- st_intersects(sf_utm, sf_grid)
sf_grid <- sf_grid[unlist(keep)]
grid <- st_as_sf(sf_grid)
plot(grid)


### if your grid is already a series of polygons ####
## will need to be in correct projection
# grid <- st_as_sf(grid, crs = proj)
# grid <- st_transform(grid, crs = proj)

### convert your grid to lat lons before getting NOAA bathymetry ####
# sf_ll <- st_transform(grid, crs = 4326) # to lat lons

# resolution of the grid, in minutes (default is 4 or > 4 miles) and I've add a 1 degree buffer to the grid
bat <- getNOAA.bathy(extent(sf_ll)[1]-1, extent(sf_ll)[2]+1, extent(sf_ll)[3]-1, extent(sf_ll)[4]+1, res = 1, keep = TRUE)

## if your data has lat lons this works too
# bat <- getNOAA.bathy(min(grid_df$longitude)-1, max(grid_df$longitude)+1,
#     min(grid_df$latitude)-1, max(grid_df$latitude)+1, res = 1, keep = TRUE)

# MAKE RASTERS ####
rast_bat <- marmap::as.raster(bat)

# change projection to match grid
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
rast_bat <- projectRaster(rast_bat, crs = proj)

# make all land NA
rast_bat[rast_bat > 0] <- NA

# check result
plot(rast_bat)

# load another version to create raster of water vs. not water
new_rast <- marmap::as.raster(bat)
new_rast <- projectRaster(new_rast, crs = proj)
rast_water_area <- new_rast
rast_water_area[new_rast < 0] <- 1
rast_water_area[new_rast >= 0] <- 0

# check result
plot(rast_water_area)

# GET DEPTH ####
# these are weigthed averages based on proportion of each pixel overlapping each grid cell
# exact = TRUE the fraction of each cell that is (partly) covered by the polygon is extracted
# weights = TRUE means the fraction is saved as a column default is that it is normalized to add to 1
# na.rm = T is required because we have assigned land as NA
get_depth <- raster::extract(x=rast_bat, y=grid, na.rm = T, exact = T, weights = T)
mean_depth <- sapply(get_depth, function(x) if (!is.null(x)) {sum(apply(x, 1, prod), na.rm = T) / sum(x[,2], na.rm = T)} else NA)

# GET PROPORTION WATER ####
# calculates the product of the value of each pixel (water is 1 and land is 0) times the proportion of the cell it covers
get_prop_water <- raster::extract(x=rast_water_area, y=grid, na.rm = T, weights=T, exact=T)
prop_water <- sapply(get_prop_water, function(x) if (!is.null(x)) {sum(apply(x, 1, prod))} else NA) # should be no NAs


# if your grid started out as a dataframe, bind to it
grid_w_depth <- bind_cols(grid_df, mean_depth = mean_depth, prop_water = prop_water)

# remove cells entirely over land
grid_w_depth <- grid_w_depth %>% filter(prop_water != 0)

ggplot(grid_w_depth) +
  geom_tile(aes(x,y, fill = prop_water), width = 2000, height = 2000) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.8))

ggplot(grid_w_depth) +
  geom_point(aes(X,Y, size = prop_water, colour = depth)) +
  scale_size_continuous(range = c(0.2,1.2)) +
  scale_colour_viridis_c(trans = "log10", direction = -1) +
  guides(colour = guide_colorbar(reverse=T)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.8))

# saveRDS(grid_w_depth, "hbll_grid_w_depth.rds")
