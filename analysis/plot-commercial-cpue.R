# explore commercial catch
library(tidyverse)
library(lubridate)
library(sf)

### Check commercial catch ratios

# # only works on network
# cye <- get_cpue_spatial_ll("yelloweye rockfish")
# chal <- get_cpue_spatial_ll("pacific halibut")
#
# cye2 <- cye %>% rename(ye_cpue = cpue,
#                        ye_kg = landed_round_kg,
#                        ye_realeased = total_released_pcs) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
# chal2 <- chal %>% rename(hal_cpue = cpue,
#                          hal_kg = landed_round_kg,
#                          hal_realeased = total_released_pcs) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
# both <- full_join(chal2, cye2)
# cc <- both %>% select(-ye_realeased, -vessel_registration_number, - trip_id, -fishing_event_id)
# saveRDS(cc, "hal-ye-ll-cpue-anon.rds")

f <- "data-generated/commercialcpue.rds"

if(!file.exists(f)) {
cc <- readRDS("data/hal-ye-ll-cpue-anon.rds") %>%
  filter(fishery_sector %in% c("halibut", "halibut and sablefish")) %>%
  # filter(fishery_sector %in% c("halibut")) %>%
  filter(major_stat_area_code %in% c("03","04","05")) %>%
  # season based on summer shallow period versus winter deep period Loher 2011
  mutate(month = month(best_date), season = ifelse(month>4 & month<9, "Summer", "Winter"))

# add utms

cc <- sdmTMB::add_utm_columns(cc, ll_names = c("lon", "lat"),
                      ll_crs = 4326,
                      utm_names = c("x", "y"),
                      utm_crs = 3156,
                      units = "m"
)


# library(sp)
# library(raster)
# library(gfplot)
# library(PBSmapping)
# data("bctopo", package = "PBSdata", envir = environment())
# ggplot(filter(bctopo, z > 0), aes(x, y, colour = z)) + geom_point()
# bath <- rename(bctopo, lon = x, lat = y, depth = z)

## this bath option only has 1km resolution
## make depth raster
bat <- readRDS(here::here("data/bath_res1_marmap2.rds"))
rast_bat <- marmap::as.raster(bat)
#
## change projection to match grid
## I believe this is 3156
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
rast_bat <- raster::projectRaster(rast_bat, crs = proj)
depth2 <- raster::extract(x=rast_bat, y=cc[,15:16], buffer=1000, fun = median, na.rm = T, exact=F)
cc2 <- cbind(cc, depth2) %>% mutate(X=x/1000, Y=y/1000, depth = -depth2)
ggplot(cc2, aes(x, y, colour = depth)) + geom_point()
proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
rast_bat <- raster::projectRaster(bath, crs = proj)


# tried doing everything this way but too slow!
# cc$depth <- NA_real_
# instead just fill in missing data from above function
cc3<-gfplot:::interp_survey_bathymetry(cc2)
# ggplot(cc3$data, aes(x, y, colour = akima_depth)) + geom_point()

hist(cc3$data$depth)

cc <- cc3$data

cc <- cc %>% filter(depth > 0)
cc_akima <- cc3$data %>% filter(akima_depth > 0) %>% mutate(depth = akima_depth)
cc <- bind_rows(cc, cc_akima)

hist(cc$depth)
hist(cc$akima_depth)
ggplot(cc, aes(x, y, colour = depth)) + geom_point()


cc$hal_cpue[is.na(cc$hal_cpue)] <- 0
cc$hal_kg[is.na(cc$hal_kg)] <- 0
cc$ye_cpue[is.na(cc$ye_cpue)] <- 0
cc$ye_kg[is.na(cc$ye_kg)] <- 0

unique(cc$fishery_sector)

ccN <- cc %>% filter(lat >= 50.5)
ccS <- cc %>% filter(lat < 50.5)
ccN$region <- "3CD5A N of 50º"
ccS$region <- "non-CDA 3CD"

focal_area <- sf::st_read(dsn = "shape-files/taaqwiihak_areaVer2.shp",
                          layer = "taaqwiihak_areaVer2", quiet = TRUE)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

ccS_sf <- st_as_sf(ccS, coords = c("lon","lat"), crs = 4326)
ccS_sf <- st_transform(ccS_sf, crs = 3156)
keep <- st_intersects(focal_area_proj, ccS_sf)
ccS_cda <- ccS_sf[unlist(keep), ]
st_geometry(ccS_cda) <- NULL
ccS_cda$region <- "CDA"

focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)

keep2 <- st_intersects(focal_area2_proj, ccS_sf)
ccS_ext <- ccS_sf[unlist(keep2), ]
st_geometry(ccS_ext) <- NULL
ccS_ext$region <- "CDA adjacent"

ccS_ext2 <- anti_join(ccS_ext, ccS_cda, by = c("x", "y", "year"))
ccS2 <- anti_join(ccS, ccS_ext, by = c("x", "y", "year"))
cc <- bind_rows(ccN, ccS2) %>% dplyr::select(-lat, -lon)
cc <- bind_rows(cc,ccS_cda) %>% bind_rows(., ccS_ext2)

saveRDS(cc, f)
} else {
cc <- readRDS(f)
}

cols <- c("red", "darkorchid4", # #4DAF4A", "#984EA3",
          "deepskyblue4")

cc %>% filter(depth < 600) %>% filter(region != "3CD5A N of 50º") %>% #filter(hal_cpue > 0) %>%
  ggplot(., aes(depth, log10(ye_cpue + 1),
                colour= region, fill= region
  )
  ) + geom_point(alpha=0.1) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F, ylim = c(0, 2.8)) +
  facet_grid(
    # cols=vars(region),
    rows=vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

# ggsave("figs/comm-cpue-YE-by-depth.pdf", width = 6, height = 6)
ggsave("figs/comm-cpue-YE-by-depth.png", width = 6, height = 6)

cc %>% filter(depth < 600) %>% filter(region != "3CD5A N of 50º") %>% #filter(hal_cpue > 0) %>%
  ggplot(., aes(depth, log10(hal_cpue + 1),
                colour= region, fill= region
  )
  ) + geom_point(alpha=0.1) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F, ylim = c(0, 2.8)) +
  facet_grid(
    # cols=vars(region),
    rows=vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

# ggsave("figs/comm-cpue-hal-by-depth.pdf", width = 6, height = 6)
ggsave("figs/comm-cpue-hal-by-depth.png", width = 6, height = 6)


cc %>% filter(depth < 600) %>% filter(region != "3CD5A N of 50º") %>% filter(hal_cpue > 0) %>%
  ggplot(., aes(depth, log10(ye_cpue/hal_cpue),
                colour= region, fill= region
  )
  ) + geom_point(alpha=0.1) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F) +
  facet_grid(
    # cols=vars(region),
    rows=vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/comm-cpue-ratio-by-depth.png", width = 6, height = 6)

cc %>% filter(depth < 400) %>% filter(region != "3CD5A N of 50º") %>% filter(hal_cpue > 0) %>%
  filter(season == "Summer" & year <= 2015) %>%
  ggplot(., aes(depth, log10(ye_cpue/hal_cpue),
                colour= region, fill= region
  )
  ) + geom_point(alpha=0.4) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/comm-cpue-ratio-by-depth-summer.png", width = 6, height = 3)





# # distribution of depths for fishing events in different regions
# cc %>% filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º")%>%
#   filter(year > 2015) %>%
#   ggplot(.,
#          aes(
#            depth,
#            colour= region, fill= region)
#   ) + geom_density(alpha=0.1, lty = "dashed") +
#   geom_density(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0 & year > 2015), alpha=0.1) +
#   scale_fill_manual(values = cols) +
#   scale_colour_manual(values = cols) +
#   coord_cartesian(expand = F) +
#   facet_grid(
#     # cols=vars(fishery_sector),
#     rows=vars(season), scales = "free_y"
#   ) + ggsidekick::theme_sleek()
#
# # ggsave("figs/comm-cpue-density-by-season-region-sector-post2015.png", width = 7, height = 5)
# ggsave("figs/comm-cpue-density-by-season-region.pdf", width = 6, height = 6)

# density plots?
cc %>% filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º")%>%
  filter(year > 2015) %>%
  ggplot(., aes(depth)) +
  geom_density(alpha=0.4, fill = "darkgreen", colour = "darkgreen") +
  geom_density(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & hal_cpue > 0
                             & year <= 2015
  ), fill = "darkgreen", colour = "darkgreen", alpha=0.1, lty = "dashed") +
  geom_density(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0
                             & year > 2015
                             ), fill = "orange", colour = "orange", alpha=0.4) +
  geom_density(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0
                             & year <= 2015
                             ), fill = "orange", colour = "orange", alpha=0.1, lty = "dashed") +
  scale_y_sqrt()+
  facet_grid(
    cols=vars(region),
    rows=vars(season), scales = "free_y"
  ) + ggsidekick::theme_sleek()
ggsave("figs/comm-spp-encounter-density-by-depth-pre-vs-post-2016.png", width = 7, height = 4)




# correlations/tradeoffs between species by depth and season
cc %>% filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º")%>%
  # filter(year > 2012) %>%
  ggplot(.,
         aes(
           log(hal_cpue+1), log(ye_cpue+1),
           colour = depth, fill= depth)
           # colour= region, fill= region)
  ) + geom_jitter(alpha=0.35, size = 2) +
  geom_smooth(method = "lm", colour = "black", fill = "black") +
  scale_fill_viridis_c(direction = -1, name = "Depth", guide = guide_colourbar(reverse = TRUE)) +
  scale_colour_viridis_c(direction = -1, name = "Depth", guide = guide_colourbar(reverse = TRUE)) +
  # scale_fill_manual(values = cols) +
  # scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )")+
  coord_cartesian(expand = F) +
  facet_grid(
    # rows=vars(region),cols=vars(season)
    cols=vars(region),rows=vars(season)
  ) + ggsidekick::theme_sleek()

ggsave("figs/comm-cpue-cor-by-season-region.png", width = 7, height = 6)

# correlations/tradeoffs between species pre and post 2016 introduction of YE quota
cc %>% filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º")%>%
  filter(year <= 2015) %>%
  ggplot(.,
         aes(
           log(hal_cpue+1), log(ye_cpue+1),
         colour= region, fill= region)
  ) +
  geom_smooth(data=filter(cc, depth < 600 & region != "3CD5A N of 50º" & year <= 2015), method = "lm", lty = "dashed") +
  geom_smooth(data=filter(cc, depth < 600 & region != "3CD5A N of 50º" & year > 2015), method = "lm") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )")+
  coord_cartesian(expand = F) +
  facet_grid(
    cols=vars(region),
    rows=vars(season)
  ) + ggsidekick::theme_sleek()

ggsave("figs/comm-cpue-cor-by-pre-vs-post-2016.png", width = 7, height = 4)


# check if license type important?
# filtered for recent years to help account for change in frequencies of license types through time
ggplot(cc) + geom_histogram(aes(year, fill = fishery_sector))

cc %>% filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º")%>%
  filter(year > 2015) %>%
  ggplot(.,
         aes(
           depth)) +
  geom_density(alpha=0.1, fill = "darkgreen", colour = "darkgreen") +
  geom_density(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0
                             # & year > 2015
  ),
  fill = "orange", colour = "orange", alpha=0.1
  ) +
  facet_grid(
    cols=vars(fishery_sector),
    rows=vars(season)
  ) + ggsidekick::theme_sleek()

cc %>% filter(depth < 600) %>%   filter(year > 2015) %>%
  filter(region != "3CD5A N of 50º")%>%
  ggplot(.,
         aes(
           log(hal_cpue+1), log(ye_cpue+1),
         colour= fishery_sector, fill= fishery_sector)
  ) + geom_jitter(alpha=0.35, size = 2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )")+
  coord_cartesian(expand = F) +
  facet_grid(
    # rows=vars(region),cols=vars(season)
    cols=vars(region),rows=vars(season)
  ) + ggsidekick::theme_sleek()

ggsave("figs/comm-cpue-cor-by-season-region-sector.png", width = 7, height = 6)


#
# ggplot(cc,
#        aes(
#          # hal_cpue, ye_cpue,
#          log(hal_cpue+1), log(ye_cpue+1),
#          colour= as.factor(year), fill= as.factor(year))
# ) +
#   geom_smooth(method = "lm", se = T, alpha =0.15) +
#   scale_fill_viridis_d("Year")+
#   scale_color_viridis_d("Year")+
#   xlab("Landable halibut ( log(CPUE + 1) )") +
#   ylab("YE ( log(CPUE + 1) )")+
#   coord_cartesian(expand = F, ylim = c(0,3.5), xlim = c(0,5)) +
#   facet_grid(cols=vars(region)
#              ,rows=vars(fishery_sector)
#   ) + ggsidekick::theme_sleek() #+ theme(legend.position = "none")
