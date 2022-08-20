# explore commercial catch
library(tidyverse)
library(lubridate)
library(sf)

### Check commercial catch ratios

# # only works on network
# cpue <- get_cpue_index_hl()
# # saveRDS(cpue, "all-hl-cpue.rds")
# # cpue <- readRDS( "all-hl-cpue.rds")
# fe <- cpue %>% select(year, month, fishing_event_id, best_depth) %>%
#   mutate(fishing_event_id = as.integer(fishing_event_id)) %>%
#   distinct()
#
# cye <- get_cpue_spatial_ll("yelloweye rockfish")
# chal <- get_cpue_spatial_ll("pacific halibut")
#
# cye2 <- cye %>% rename(ye_cpue = cpue,
#                        ye_kg = landed_round_kg,
#                        ye_realeased = total_released_pcs) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
#
# chal2 <- chal %>% rename(hal_cpue = cpue,
#                          hal_kg = landed_round_kg,
#                          hal_realeased = total_released_pcs) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
#
# both <- full_join(chal2, cye2)
# both2 <- left_join(both, fe)
# cc <- both2 %>% select( -vessel_registration_number, - trip_id, -fishing_event_id)
# saveRDS(cc, "hal-ye-ll-cpue-anon.rds")


f <- "data-generated/commercialcpue.rds"

if (!file.exists(f)) {
  cc <- readRDS("data/hal-ye-ll-cpue-w-effort.rds")
  # add utms

  cc <- sdmTMB::add_utm_columns(cc,
    ll_names = c("lon", "lat"),
    ll_crs = 4326,
    utm_names = c("x", "y"),
    utm_crs = 3156,
    units = "m"
  )

  ggplot(cc, aes(x, y, colour = -log(best_depth))) +
    geom_point()


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
  # library(raster)
  proj <- "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs"
  rast_bat <- raster::projectRaster(rast_bat, crs = proj)
  depth2 <- raster::extract(x = rast_bat, y = cc[, (ncol(cc)-1):ncol(cc)], buffer = 1000, fun = median, na.rm = T, exact = F)
  cc2 <- cbind(cc, depth2) %>% mutate(X = x / 1000, Y = y / 1000, bath_depth = -depth2, depth = best_depth)

  ggplot(cc2, aes(log(best_depth),log(bath_depth))) + geom_point() +
    geom_abline(slope = 1, intercept = 0)

  # cc3 <- gfplot:::interp_survey_bathymetry(cc2)

  # I think best_depth is much better!

  # instead just fill in missing data from above function
  # cc2 <- cc %>% mutate(X = x / 1000, Y = y / 1000, depth = best_depth)
  cc3 <- gfplot:::interp_survey_bathymetry(cc2)
  # ggplot(cc3$data, aes(x, y, colour = akima_depth)) + geom_point()

  hist(cc3$data$depth, breaks = 40)

  cc1 <- cc3$data

  cc1 <- cc1 %>% filter(depth > 0)

  cc_akima <- cc3$data %>%
    filter(akima_depth > 0) %>%
    mutate(depth = akima_depth)

  cc <- bind_rows(cc1, cc_akima)

  hist(cc$depth, breaks = 40)
  hist(cc$akima_depth, breaks = 40)

  cc$hal_cpue[is.na(cc$hal_cpue)] <- 0
  cc$hal_kg[is.na(cc$hal_kg)] <- 0
  cc$ye_cpue[is.na(cc$ye_cpue)] <- 0
  cc$ye_kg[is.na(cc$ye_kg)] <- 0

  unique(cc$fishery_sector)

  ccN <- cc %>% filter(lat >= 50.5)
  ccS <- cc %>% filter(lat < 50.5)
  ccN$region <- "3CD5A N of 50º"
  ccS$region <- "non-CDA 3CD"

  focal_area <- sf::st_read(
    dsn = "shape-files/taaqwiihak_areaVer2.shp",
    layer = "taaqwiihak_areaVer2", quiet = TRUE
  )
  focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

  ccS_sf <- st_as_sf(ccS, coords = c("lon", "lat"), crs = 4326)
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
  cc <- bind_rows(cc, ccS_cda) %>% bind_rows(., ccS_ext2)

  cc <- cc %>%
    mutate(discrepency = depth - bath_depth,
           ye_count = ye_cpue,
           ye_cpue = ye_kg/ha_fished,
           hal_count = hal_cpue,
           hal_cpue = hal_kg/ha_fished,
           ye_cpue2 = ifelse(ye_cpue < 0.1, 0.1, ye_cpue),
           hal_cpue2 = ifelse(hal_cpue < 0.1, 0.1, hal_cpue),
           ye_cpue3 = ifelse(ye_cpue < 0.01, 0.01, ye_cpue),
           hal_cpue3 = ifelse(hal_cpue < 0.01, 0.01, hal_cpue)
    ) %>%
    filter(dist_km_fished < 5 & dist_km_fished > 0.1) %>%
    filter(depth > 20 & depth < 1000) %>%
    filter(discrepency < 400 & discrepency > -500)

  saveRDS(cc, f)
} else {
  cc <- readRDS(f)
}


cc <- cc %>%
  filter(!(region == "CDA" & depth > 130)) %>% # removes single depth outlier in CDA
  filter(dist_km_fished < 3 & dist_km_fished > 0.2)

hist(cc$discrepency)

cc %>%
  # filter(depth < 600) %>%
  ggplot() + geom_point(aes(X, Y, colour = discrepency)) +
  scale_color_gradient2() + facet_wrap(~region, scales = "free")

cc %>%
  # filter(depth < 600) %>%
  ggplot() + geom_point(aes(X, Y, colour = -log(depth)), alpha = 0.5) +
  facet_wrap(~region, scales = "free")


# check that a could extreme outliers from CDA were removed
cc %>%
  # filter(depth < 600) %>%
  ggplot() + geom_point(aes(depth, bath_depth, colour = discrepency)) +
  scale_color_gradient2() + facet_wrap(~region)

cc %>%
  # filter(depth < 600) %>%
  ggplot() + geom_point(aes(depth, X, colour = discrepency)) +
  scale_color_gradient2() + facet_wrap(~region)


ggplot(cc, aes(bath_depth, depth)) + geom_point(aes(colour = discrepency)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_gradient2()


ggplot(cc, aes(X, Y, colour = depth)) +
  geom_point() #+ scale_color_continuous(trans = "log10")

cc %>% filter (depth < 800 & ye_cpue > 0) %>%
  ggplot(aes(X, Y, colour =  discrepency)) +
  geom_point() + scale_color_gradient2()

hist(cc$ye_cpue)

cc %>% filter (depth < 1000 & ye_cpue > 0) %>%
  ggplot(aes(X, Y, colour =  -log(ye_cpue))) +
  geom_point()

# load map custom functions
source("analysis/functions.R")
map_predictions(obs_data = cc %>% mutate(X = X/100, Y = Y/100) %>% filter (depth < 1000 & ye_cpue > 0),
                size_aes = ye_cpue,
                obs_col = c("blue","red"),
                col_aes = fishery_sector)





cols <- c(
  "red",
  "deeppink4",
  # "darkorchid4", # #4DAF4A", "#984EA3",
  "deepskyblue4"
)

cc %>%
  filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º") %>%
  filter(hal_cpue > 0) %>%
  ggplot(., aes(depth, log10(ye_cpue + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(
    ylim = c(0, 2),
    expand = F) +
  facet_grid(
    # cols=vars(region),
    rows = vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

# ggsave("figs/comm-cpue-YE-by-depth.pdf", width = 6, height = 6)
ggsave("figs/comm-cpue-YE-by-depth.png", width = 6, height = 4)

cc %>%
  filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º") %>% # filter(hal_cpue > 0) %>%
  ggplot(., aes(depth, log10(hal_cpue + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(
    ylim = c(0, 3),
    expand = F) +
  facet_grid(
    # cols=vars(region),
    rows = vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

# ggsave("figs/comm-cpue-hal-by-depth.pdf", width = 6, height = 6)
ggsave("figs/comm-cpue-hal-by-depth.png", width = 6, height = 4)


cc %>%
  filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º") %>%
  filter(hal_cpue > 0) %>%
  # ggplot(., aes(depth, log10(ye_cpue / hal_cpue),
  ggplot(., aes(depth, log10(ye_cpue / hal_cpue),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.1) +
  geom_vline(xintercept = 175, lty = "dashed") +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F,
                  xlim = c(0, 500)) +
  facet_grid(
    # cols=vars(region),
    rows = vars(season)
  ) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/comm-true-cpue-ratio-by-depth.png", width = 6, height = 6)

cc %>%
  # filter(depth < 400) %>%
  filter(region != "3CD5A N of 50º") %>%
  # filter(hal_cpue > 0) %>%
  filter(season == "Summer" ) %>%
  filter(year <= 2015)%>%
  ggplot(., aes(depth,
                # log10(ye_cpue / hal_cpue),
                log10(ye_cpue2 / hal_cpue2),
                # log10(ye_cpue3 / hal_cpue3),
                # log10((ye_cpue +0.1)/ (hal_cpue+0.1)),
                # log10((ye_cpue +0.1)/ (hal_cpue+0.1)),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, lty = "dashed" ) +
  geom_vline(xintercept = 175, lty = "dashed") +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F,
                  xlim = c(0, 500),
                  # ylim = c(-2.4, 2.4)) +
                  ylim = c(-3.4, 3.4)) +
  ylab("log10(YE/halibut) truncated CPUE at 0.1") +
  ggtitle(paste0("A. Commercial data from 2006 to 2015")) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/comm-true-cpue-ratio-by-depth-summer-w-zeros-pre2016.png", width = 7, height = 4)

cc %>%
  # filter(depth < 400) %>%
  filter(region != "3CD5A N of 50º") %>%
  # filter(hal_cpue > 0) %>%
  filter(season == "Summer" ) %>%
  filter(year > 2015)%>%
  ggplot(., aes(depth,
                # log10(ye_cpue / hal_cpue),
                log10(ye_cpue2 / hal_cpue2),
                # log10(ye_cpue3 / hal_cpue3),
                # log10((ye_cpue +0.1)/ ((hal_cpue)+0.1)),
                # log10((ye_cpue +0.1)/ (hal_cpue+0.1)),
                colour = region, fill = region
  )) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_vline(xintercept = 175, lty = "dashed") +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F,
                  xlim = c(0, 500),
                  ylim = c(-3.4, 3.4)) +
                  # ylim = c(-2.4, 2.4)) +
  ylab("log10(YE/halibut) truncated CPUE at 0.1") +
  ggtitle(paste0("B. Commercial data from 2016 to 2022")) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/comm-true-cpue-ratio-by-depth-summer-w-zeros-post2015.png", width = 7, height = 4)
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
cc %>%
  filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º") %>%
  filter(year > 2015) %>%
  ggplot(., aes(depth)) +
  geom_density(alpha = 0.4, fill = "darkgreen", colour = "darkgreen") +
  geom_density(
    data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & hal_cpue > 0 & year <= 2015),
    fill = "darkgreen", colour = "darkgreen", alpha = 0.1, lty = "dashed") +
  geom_density(
    data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0 & year > 2015),
    fill = "orange", colour = "orange", alpha = 0.4) +
  geom_density(
    data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0 & year <= 2015),
    fill = "orange", colour = "orange", alpha = 0.1, lty = "dashed") +
  scale_y_sqrt() +
  facet_grid(
    cols = vars(region),
    rows = vars(season), scales = "free_y"
  ) +
  ggsidekick::theme_sleek()
ggsave("figs/comm-spp-encounter-density-by-depth-pre-vs-post-2016.png", width = 7, height = 4)


# correlations/tradeoffs between species by depth and season
cc %>%
  filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º") %>%
  # filter(year < 2016) %>%
  mutate(period = ifelse(year<2016, "pre-2016", "post-2015")) %>%
  ggplot(
    .,
    aes(
      # log(hal_cpue + 1), log(ye_cpue + 1),
      log(hal_cpue + 1), log(ye_cpue + 1),
      colour = period, fill = period
    )
    # colour= region, fill= region)
  ) +
  geom_jitter(alpha = 0.35, size = 2) +
  geom_smooth(method = "lm"#, colour = "black", fill = "black"
              ) +
  # scale_fill_viridis_c(direction = -1, name = "Depth", guide = guide_colourbar(reverse = TRUE)) +
  # scale_colour_viridis_c(direction = -1, name = "Depth", guide = guide_colourbar(reverse = TRUE)) +
  # scale_fill_manual(values = cols) +
  # scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )") +
  coord_cartesian(expand = F) +
  facet_grid(
    # rows=vars(region),cols=vars(season)
    cols = vars(region), rows = vars(season)
  ) +
  ggsidekick::theme_sleek()

ggsave("figs/comm-true-cpue-cor-by-season-region.png", width = 7, height = 6)

# correlations/tradeoffs between species pre and post 2016 introduction of YE quota
cc %>%
  filter(depth < 600) %>%
  filter(region != "3CD5A N of 50º") %>%
  filter(year <= 2015) %>%
  ggplot(
    .,
    aes(
      # log(hal_cpue + 1), log(ye_cpue + 1),
      log(hal_cpue + 1), log(ye_cpue + 1),
      colour = region, fill = region
    )
  ) +
  geom_smooth(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & year <= 2015), method = "lm", lty = "dashed") +
  geom_smooth(data = filter(cc, depth < 600 & region != "3CD5A N of 50º" & year > 2015), method = "lm") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )") +
  coord_cartesian(expand = F) +
  facet_grid(
    cols = vars(region),
    rows = vars(season)
  ) +
  ggsidekick::theme_sleek()

ggsave("figs/comm-true-cpue-cor-by-pre-vs-post-2016.png", width = 7, height = 4)


# check if license type important?
# filtered for recent years to help account for change in frequencies of license types through time
ggplot(cc) +
  geom_histogram(aes(year, fill = fishery_sector))

cc %>%
  filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º") %>%
  filter(year > 2015) %>%
  ggplot(
    .,
    aes(
      depth
    )
  ) +
  geom_density(alpha = 0.1, fill = "darkgreen", colour = "darkgreen") +
  geom_density(
    data = filter(
      cc, depth < 600 & region != "3CD5A N of 50º" & ye_cpue > 0
      # & year > 2015
    ),
    fill = "orange", colour = "orange", alpha = 0.1
  ) +
  facet_grid(
    cols = vars(fishery_sector),
    rows = vars(season)
  ) +
  ggsidekick::theme_sleek()

cc %>%
  filter(depth < 600) %>%
  filter(year > 2015) %>%
  filter(region != "3CD5A N of 50º") %>%
  ggplot(
    .,
    aes(
      log(hal_cpue + 1), log(ye_cpue + 1),
      colour = fishery_sector, fill = fishery_sector
    )
  ) +
  geom_jitter(alpha = 0.35, size = 2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  xlab("Landable halibut ( log(CPUE + 1) )") +
  ylab("YE ( log(CPUE + 1) )") +
  coord_cartesian(expand = F) +
  facet_grid(
    # rows=vars(region),cols=vars(season)
    cols = vars(region), rows = vars(season)
  ) +
  ggsidekick::theme_sleek()

ggsave("figs/comm-true-cpue-cor-by-season-region-sector.png", width = 7, height = 6)


library(gfplot)
library(ggsidekick)
# cc %>%
#   filter(depth < 600 & hal_cpue > 0 & region != "3CD5A N of 50º") %>%
  # filter(year > 2015) %>%

c <- cc %>% filter (depth < 1000) %>%
  mutate(
    # cpue = ye_cpue,
    cpue = ifelse(ye_cpue == 0, 0.00001, ye_cpue),
    # cpue = ifelse(ye_cpue>15, 15, ye_cpue),
    # cpue = log10(ye_cpue),
    lon = longitude,
    lat = latitude) %>%
  select(-X, -Y)

# hist(c$cpue)

p1 <- plot_cpue_spatial(c, start_year = 2007, bin_width = 5, n_minimum_vessels = 3,
                  xlim = c(450, 800),
                  ylim = c(5370, 5640)) +
  # scale_colour_viridis_c(trans = fourth_root_power_trans(), option = "D") +
  # scale_fill_viridis_c(trans = fourth_root_power_trans(), option = "D")
  scale_colour_viridis_c(trans = "log10", option = "D",
                         breaks = c(0, 0.001, 0.1, 10),
                         labels = c("0" ,"0.001","0.1", "10"),
                         name = "CPUE(kg/ha)") +
  scale_fill_viridis_c(trans = "log10", option = "D",
                       breaks = c(0, 0.001, 0.1, 10),
                       labels = c("0" ,"0.001","0.1", "10"),
                       name = "CPUE(kg/ha)")+
  theme(legend.position = c(0.95,0.95)) +
  # ggtitle("A. Yelloweye Rockfish commercial CPUE (kg/ha)")
  ggtitle("(a) Yelloweye Rockfish commercial CPUE (kg/ha)")
  # scale_colour_viridis_c(option = "D") +
  # scale_fill_viridis_c(option = "D")

ggsave("figs/spatial-cpue-yelloweye.png", width = 4.5, height = 3.5)

c2 <- cc %>% filter (depth < 1000) %>%
  mutate(
    # cpue = ye_cpue,
    cpue = ifelse(hal_cpue == 0, 0.00001, hal_cpue),
    # cpue = ifelse(ye_cpue>15, 15, ye_cpue),
    # cpue = log10(ye_cpue),
    lon = longitude,
    lat = latitude) %>%
  select(-X, -Y)

# hist(c$cpue)

p2 <- plot_cpue_spatial(c2, start_year = 2007, bin_width = 5, n_minimum_vessels = 3,
                  xlim = c(450, 800),
                  ylim = c(5370, 5640)) +
  # scale_colour_viridis_c(trans = fourth_root_power_trans(), option = "D") +
  # scale_fill_viridis_c(trans = fourth_root_power_trans(), option = "D")
  # scale_colour_viridis_c(trans = "log10", option = "D",
  #                        breaks = c(0, 0.001, 0.1, 10),
  #                        labels = c("0" ,"0.001","0.1", "10"),
  #                        name = "CPUE(kg/ha)") +
  # scale_fill_viridis_c(trans = "log10", option = "D",
  #                      breaks = c(0, 0.001, 0.1, 10),
  #                      labels = c("0" ,"0.001","0.1", "10"),
  #                      name = "CPUE(kg/ha)")
  scale_colour_viridis_c(trans = "sqrt", option = "D",name = "CPUE(kg/ha)") +
  scale_fill_viridis_c(trans = "sqrt", option = "D",name = "CPUE(kg/ha)") +
  theme(legend.position = c(0.95,0.95)) +
  # ggtitle("B. Pacific Halibut commercial CPUE (kg/ha)")
  ggtitle("(b) Pacific Halibut commercial CPUE (kg/ha)")
#   scale_colour_viridis_c(option = "D") +
#   scale_fill_viridis_c(option = "D")

ggsave("figs/spatial-cpue-halibut.png", width = 4.5, height = 3.5)

p1 + p2 + patchwork::plot_layout() & theme(axis.ticks = element_blank(),
                                           axis.title = element_blank(),
                                           axis.text = element_blank())

ggsave("figs/spatial-cpue.png", width = 8, height = 3.5)
