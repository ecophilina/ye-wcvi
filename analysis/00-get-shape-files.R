# get mapping data and shape files

dir.create("data", showWarnings = FALSE)
# download gshhg-shp-2.3.7 from https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/ add place in the "data" folder

dir.create("grids", showWarnings = FALSE)
# synoptic_grid.rda (from gfplot package data) and a shape file for outside HBLL survey are also needed in a "grids" folder
# an up to date hbll_s_grid.rda is now also available in gfplot, but 00-make-grid.Rmd needs modifying to work with it

dir.create("shape-files", showWarnings = FALSE)
# this folder need to contain "taaqwiihak_areaVer2.shp", "ExtendCDA1/Extend_1.shp", and "Shapes/majorOutline.shp" shape files
# used for various mapping and data grouping purposes
