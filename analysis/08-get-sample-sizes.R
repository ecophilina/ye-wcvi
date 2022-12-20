# model data stats

library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)

library(gfplot)
library(ggsidekick)

hal_model <- "w-deeper-500kn-delta-AR1-aniso-dec22"
m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))

# ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-dec22"
# m_ye <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))

focal_area <- sf::st_read(
  dsn = "shape-files/taaqwiihak_areaVer2.shp",
  layer = "taaqwiihak_areaVer2", quiet = TRUE
)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

m_dat <- m_hal$data %>% mutate(X2 = X * 100000, Y2 = Y * 100000)
# m_dat <- m_ye$data %>% mutate(X2 = X * 100000, Y2 = Y * 100000)

m_dat_sf <- st_as_sf(m_dat, coords = c("X2", "Y2"), crs = 3156)
keep <- st_intersects(focal_area_proj, m_dat_sf)
m_dat_cda <- m_dat_sf[unlist(keep), ]
st_geometry(m_dat_cda) <- NULL
m_dat_cda$region <- "CDA"

focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)

keep2 <- st_intersects(focal_area2_proj, m_dat_sf)
m_dat_ext <- m_dat_sf[unlist(keep2), ]
st_geometry(m_dat_ext) <- NULL
m_dat_ext$region <- "CDA adjacent"

m_dat_ext2 <- anti_join(m_dat_ext, m_dat_cda, by = c("X", "Y", "fishing_event_id"))
m_dat2 <- anti_join(m_dat, m_dat_ext, by = c("X", "Y", "fishing_event_id"))
m_dat2$region <- "non-CDA 3CD"
m_dat <- bind_rows(m_dat2, m_dat_cda) %>% bind_rows(., m_dat_ext2)

m_dat %>% filter(region == "CDA adjacent") %>%
ggplot(aes(X,Y)) + geom_point(colour = "purple") +
  geom_point(data = filter(m_dat, region == "CDA"), colour = "red")

m_dat %>% group_by(survey, region) %>% summarise(n = n())
m_dat %>% group_by(region) %>% summarise(n = n())
m_dat %>% group_by(survey) %>% summarise(n = n())

m_dat %>% summarise(n = n())
