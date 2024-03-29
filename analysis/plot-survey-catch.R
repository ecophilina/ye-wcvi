### Merge survey catches
library(tidyverse)
library(lubridate)
library(sf)

d_hal_plotsS <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 50.5) %>% filter(survey != "NON-SURVEY")

d_ye_plotsS <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 50.5) %>% filter(survey != "NON-SURVEY")

ye_obs <- d_ye_plotsS %>%
  select(X, Y,
    depth = depth_m, year_pair, year_true, survey,
    density_ye = density, fishing_event_id
  )
hal_obs <- d_hal_plotsS %>%
  select(fishing_event_id,
    density_hal = density
  )
surv_dat <- left_join(ye_obs, hal_obs)


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
  xlim(0, log(max(surv_dat$density_hal, na.rm = T) + 1)) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian() +
  ggsidekick::theme_sleek()

surv_dat %>%
  filter(depth < 550) %>%
  ggplot(., aes(depth, log10(density_ye + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F, ylim = c(0, 2.6)) +
  ggsidekick::theme_sleek()

ggsave("figs/surv-hal-by-depth.png", width = 6, height = 3)

surv_dat %>%
  filter(depth < 550) %>%
  ggplot(., aes(depth, log10(density_hal + 1),
    colour = region, fill = region
  )) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(
    ylim = c(0, 2.6),
    expand = F) +
  ggsidekick::theme_sleek()

ggsave("figs/surv-hal-by-depth.png", width = 6, height = 3)

hist(surv_dat$density_hal, breaks = 30)

surv_dat %>%
  mutate(
    density_ye2 = ifelse(density_ye < 0.1, 0.1, density_ye),
    density_hal2 = ifelse(density_hal < 0.1, 0.1, density_hal)
  )%>%
  ggplot(., aes(depth,
                # log10(density_ye/density_hal),
                log10(density_ye2/density_hal2),
    colour = region, fill = region, shape = survey, group = region
  )) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_vline(xintercept = 175, lty = "dashed") +
  geom_smooth(method = "gam") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  coord_cartesian(expand = F,
                  xlim = c(0, 500),
                  ylim = c(-3.5, 3.5)) +
  ylab("log10(YE/halibut) truncated at 0.1") +
  ggtitle(paste0("Survey data from 2004 to 2020")) +
  ggsidekick::theme_sleek()

ggsave("figs/surv-ratio-by-depth-w-zeros.png", width = 6, height = 4)

