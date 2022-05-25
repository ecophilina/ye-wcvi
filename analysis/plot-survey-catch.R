### Merge survey catches
library(tidyverse)
library(lubridate)
library(sf)

d_hal_plotsS <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 50.5) %>% filter(survey != "NON-SURVEY")

d_ye_plotsS <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 50.5) %>% filter(survey != "NON-SURVEY")

ye_obs <- d_ye_plotsS %>%
  # mutate(catch_count_ye = (catch_count/hook_count)*100, catch_equiv_ye = catch_equiv) %>%
  select(X, Y,
    depth = depth_m, year_pair, year_true, survey, # catch_count_ye, catch_equiv_ye,
    density_ye = density, fishing_event_id
  )
hal_obs <- d_hal_plotsS %>%
  # mutate(catch_count_hal = (catch_count/hook_count)*100, catch_equiv_hal = catch_equiv) %>%
  select(fishing_event_id, # catch_count_hal, catch_equiv_hal,
    density_hal = density
  )
surv_dat <- left_join(ye_obs, hal_obs) # %>% filter(depth < 200)


focal_area <- sf::st_read(
  dsn = "shape-files/taaqwiihak_areaVer2.shp",
  layer = "taaqwiihak_areaVer2", quiet = TRUE
)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

surv_dat <- surv_dat %>% mutate(X2 = X * 100000, Y2 = Y * 100000)
surv_dat_sf <- st_as_sf(surv_dat, coords = c("X2", "Y2"), crs = 3156)
keep <- st_intersects(focal_area_proj, surv_dat_sf)
surv_dat_cda <- surv_dat_sf[unlist(keep), ]
st_geometry(surv_dat_cda) <- NULL
surv_dat_cda$region <- "CDA"

focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)

keep2 <- st_intersects(focal_area2_proj, surv_dat_sf)
surv_dat_ext <- surv_dat_sf[unlist(keep2), ]
st_geometry(surv_dat_ext) <- NULL
surv_dat_ext$region <- "CDA adjacent"

surv_dat_ext2 <- anti_join(surv_dat_ext, surv_dat_cda, by = c("X", "Y", "fishing_event_id"))
surv_dat2 <- anti_join(surv_dat, surv_dat_ext, by = c("X", "Y", "fishing_event_id"))
surv_dat2$region <- "non-CDA 3CD"
surv_dat <- bind_rows(surv_dat2, surv_dat_cda) %>% bind_rows(., surv_dat_ext2)

cols <- c(
  "red",
  "deeppink4",
  # "darkorchid4", # #4DAF4A", "#984EA3",
  "deepskyblue4"
)

ggplot(surv_dat, aes(log(density_hal + 1), log(density_ye + 1), fill = region, colour = region)) +
  geom_smooth(method = "lm") +
  geom_jitter(alpha = 0.5) +
  # coord_cartesian(expand = F, xlim = c(0, log(max(catch_compN$catch_count_hal, na.rm = T)+1))) +
  xlim(0, log(max(surv_dat$density_hal, na.rm = T) + 1)) +
  # ylab("log( YE catch count + 1)") +
  # xlab("log( Halibut catch count + 1)") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian() +
  # ggtitle("A. HBLL survey north of 50º latitude")+
  ggsidekick::theme_sleek()

surv_dat %>%
  filter(depth < 550) %>%
  ggplot(., aes(depth, log10(density_ye + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F, ylim = c(0, 0.8)) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/surv-YE-by-depth.png", width = 6, height = 6)

surv_dat %>%
  filter(depth < 550) %>%
  ggplot(., aes(depth, log10(density_hal + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F, ylim = c(0, 0.8)) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/surv-hal-by-depth.png", width = 6, height = 6)

hist(surv_dat$density_hal, breaks = 30)

surv_dat %>%
  filter(depth < 400) %>%
  # filter(year_true > 2006) %>%
  # filter(density_hal > 0) %>%
  # filter(region != "non-CDA 3CD") %>%
  ggplot(., aes(depth,
                # log10(density_ye/density_hal),
                log10((density_ye+0.01)/(density_hal+0.01)),
    colour = region, fill = region, shape = survey, group = region
  )) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 170) +
  geom_smooth() +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F,
                  xlim = c(0, 400),
                  ylim = c(-3.4, 3.4)) +
  ggtitle(paste0("Survey data from 2004 to 2020")) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/surv-ratio-by-depth-w-zeros.png", width = 6, height = 4)

