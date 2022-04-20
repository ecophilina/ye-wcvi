
# Retrieve all survey data using gfdata

dir.create("data", showWarnings = FALSE)

halsetdata <- gfdata::get_survey_sets("Pacific Halibut")
saveRDS(halsetdata, "data/halibut-surv-sets-all.rds")

yesetdata <- gfdata::get_survey_sets("Yelloweye Rockfish")
saveRDS(yesetdata, "data/yelloweye-surv-sets-all.rds")

# get detailed biological data, I have retrieved data from all surveys in case comparisons between surveys are needed later
yesampledata <- gfdata::get_survey_samples("Yelloweye Rockfish")
saveRDS(yesampledata, "data/yelloweye-surv-samples-all.rds")

halsampledata <- gfdata::get_survey_samples("Pacific Halibut")
saveRDS(halsampledata, "data/halibut-surv-samples-all.rds")

# I also copied ####_grid.rda from gfplot into "data" folder
# and will need a folder called "shape-files" containing "taaqwiihak_areaVer2.shp"
# download gshhg-shp-2.3.7 from https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/ add place in data folder
