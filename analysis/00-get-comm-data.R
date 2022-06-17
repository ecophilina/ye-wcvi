library(dplyr)
library(gfdata)

# # only works on network
# cpue <- get_cpue_index_hl()
# saveRDS(cpue, "data/all-hl-cpue.rds")
#
# hks <- get_commercial_hooks_per_fe(min_cpue_year = 2006)
# saveRDS(hks, "data/all-hl-hooks.rds")
#
# cpue <- readRDS("data/all-hl-cpue.rds")
# hks <- readRDS("data/all-hl-hooks.rds")
#
# fe <- cpue %>%
#   select(year, month, fishing_event_id) %>%
#   mutate(fishing_event_id = as.integer(fishing_event_id)) %>%
#   distinct()
#
# cye <- get_cpue_spatial_ll("yelloweye rockfish")
# chal <- get_cpue_spatial_ll("pacific halibut")
#
# cye2 <- cye %>%
#   rename(
#     ye_cpue = cpue,
#     ye_kg = landed_round_kg,
#     ye_realeased = total_released_pcs
#   ) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
#
# chal2 <- chal %>%
#   rename(
#     hal_cpue = cpue,
#     hal_kg = landed_round_kg,
#     hal_realeased = total_released_pcs
#   ) %>%
#   select(-species_code, -species_common_name, -species_scientific_name)
#
# both <- full_join(chal2, cye2)
# both2 <- left_join(both, fe)
# both3 <- left_join(both2, hks)
#
# cc <- both3 # %>% select( -vessel_registration_number, - trip_id, -fishing_event_id)
# saveRDS(cc, "data/hal-ye-ll-cpue-all.rds")


# clean up effort data

library(lubridate)
library(tidyverse)
library(sp)

cc <- readRDS("data/hal-ye-ll-cpue-all.rds") %>%
  filter(fishery_sector %in% c("halibut", "halibut and sablefish"))


cc <- cc %>%
  mutate(
    end_longitude = ifelse((-end_longitude) < (max(cc$lon) + 2) & (-end_longitude) > (min(cc$lon) - 2), -end_longitude, NA),
    end_latitude = ifelse(end_latitude < (max(cc$lat) + 2) & end_latitude > (min(cc$lat) - 2), end_latitude, NA)
  )

# cc <- sdmTMB::add_utm_columns(cc, ll_names = c("lon", "lat"), units = "m", ll_crs = 4326, utm_crs = 3156)

cc <- cc %>% filter(end_latitude > 0 & end_longitude < 0 & lat > 0 & lon < 0)

# cc <- sdmTMB::add_utm_columns(cc2, ll_names = c("end_longitude", "end_latitude"), utm_names = c("X_end", "Y_end"), units = "m", ll_crs = 4326, utm_crs = 3156)

startxy <- cc %>% select(x = lon, y = lat)
startxy <- as.matrix(startxy)

endxy <- cc %>% select(x = end_longitude, y = end_latitude)
endxy <- as.matrix(endxy)
cc$dist_km <- spDists(startxy, y = endxy, longlat = TRUE, diagonal = TRUE)
cc <- cc %>% mutate(dist_km = ifelse(dist_km > 20, NA, dist_km))


cc$number_gear_lost[is.na(cc$number_gear_lost)] <- 0
cc$hal_cpue[is.na(cc$hal_cpue)] <- 0
cc$hal_kg[is.na(cc$hal_kg)] <- 0
cc$ye_cpue[is.na(cc$ye_cpue)] <- 0
cc$ye_kg[is.na(cc$ye_kg)] <- 0


# investigate vessel means
cc <- cc %>%
  group_by(vessel_registration_number) %>%
  mutate(
    med_gear_set = median(number_gear_set, na.rm = T),
    med_hooks_per_skate = median(hooks_traps_per_skate, na.rm = T),
    mean_dist_km = mean(dist_km, na.rm = T),
    med_dist_km = median(dist_km, na.rm = T)
  ) %>%
  ungroup()

hist(cc$dist_km, breaks = 1000)
hist(cc$med_gear_set, xlim = c(0, 20), breaks = 1000)
hist(cc$med_hooks_per_skate, xlim = c(0, 1000), breaks = 200)
hist(cc$med_dist_km, xlim = c(0, 20), breaks = 200)

# make sure units appear the same for gear set and lost
cc %>%
  filter(number_gear_lost > 0) %>%
  filter(number_gear_set < 50) %>%
  ggplot() +
  geom_point(aes(number_gear_set, number_gear_lost))

cc %>%
  filter(number_gear_set < 30) %>%
  ggplot() +
  geom_point(aes(number_gear_set, hooks_traps_per_skate))

cc %>%
  filter(number_gear_set > 30) %>%
  ggplot() +
  geom_point(aes(number_gear_set, hooks_traps_per_skate))

# compute new effort vars
cc <- cc %>%
  mutate(
    # assume that gear is the same unit for both set and lost
    prop_fished = ifelse(number_gear_lost == 0, 1, (number_gear_set - number_gear_lost) / number_gear_set),
    number_skates_set = ifelse(number_gear_set < 30, number_gear_set, NA),
    # hooks_miss_recorded = ifelse(hooks_traps_per_skate > 400, hooks_traps_per_skate, NA),
    hooks_per_skate = ifelse(hooks_traps_per_skate < 300 & hooks_traps_per_skate > 50, hooks_traps_per_skate, NA),
    hook_count = number_skates_set * prop_fished * hooks_per_skate,
    gear_km_fished = ifelse(skate_ft < 3000, number_skates_set * prop_fished * skate_ft * 0.0003048, NA), # convert from ft to km
    dist_km_fished = ifelse(dist_km < 10, dist_km * prop_fished, NA),
    # convert to hectares for both hook count and distance
    ha_swept = hook_count * 0.0024384 * 0.009144 * 100 * 2,
    ha_fished = dist_km_fished * 0.009144 * 100 * 2 # more of this
  )

# filter to study area
cc <- cc %>%
  filter(major_stat_area_code %in% c("03", "04", "05")) %>%
  # season based on summer shallow period versus winter deep period Loher 2011
  mutate(
    # fishing_event_id = row_number(),
    month = month(best_date),
    season = ifelse(month > 4 & month < 9, "Summer", "Winter"),
    vessel_id = as.character(vessel_registration_number)
  ) %>%
  filter(lon > -130.2 & lon < -124.75 & lat < 51.5 & lat > 48.3)

saveRDS(cc, "data/hal-ye-ll-cpue-w-effort.rds")


# data visualization

hist(cc$hooks_traps_per_skate, xlim = c(0, 1000), breaks = 200)
hist(cc$hooks_per_skate, xlim = c(0, 400), breaks = 50)
hist(cc$hook_count, xlim = c(0, 3000), breaks = 100)
hist(cc$skate_ft, breaks = 100)
hist(cc$gear_km_fished, breaks = 50)
hist(cc$ft_per_hook, xlim= c(0,100), breaks = 200)
hist(cc$number_gear_set, xlim = c(0, 100), breaks = 10000) # greater than 30 seem to be errors
ggplot(cc) + geom_histogram(aes(number_skates_set))
ggplot(cc) + geom_histogram(aes(ha_fished))



ggplot(cc) + geom_point(aes(number_skates_set, hooks_per_skate, colour = dist_km)) +
  scale_colour_viridis_c(trans = "log10")

cc %>%
  filter(med_gear_set < 100) %>%
  ggplot() +
  geom_point(aes(med_dist_km, med_gear_set))

cc %>%
  filter(number_gear_set < 100) %>%
  ggplot() +
  geom_point(aes(number_gear_set, skate_ft))

cc %>%
  filter(number_gear_set < 200) %>%
  filter(hooks_traps_per_skate < 200) %>%
  ggplot() +
  geom_point(aes(number_gear_set, hooks_traps_per_skate))

cc %>%
  # filter(gear_km_fished < 5) %>%
  ggplot() +
  geom_point(aes(dist_km_fished, gear_km_fished,
    colour = hook_count
  ), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_viridis_c(trans = "log10") +
  facet_wrap(~year) +
  ggsidekick::theme_sleek()



cc %>%
  # filter(gear_km_fished < 3) %>%
  filter(dist_km_fished > 0.2 & dist_km_fished < 3) %>%
  ggplot() +
  geom_point(aes(ha_fished, ha_swept,
    colour = gear_km_fished
  ), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_viridis_c() +
  facet_wrap(~year) +
  ggsidekick::theme_sleek()


cc %>%
  filter(dist_km_fished < 3) %>%
  ggplot() +
  geom_point(aes(ha_swept, hal_kg,
    colour = ha_fished
  ), alpha = 0.5) +
  scale_colour_viridis_c() +
  facet_wrap(~year) +
  ggsidekick::theme_sleek()

cc %>%
  filter(dist_km_fished < 3) %>%
  ggplot() +
  geom_point(aes(ha_fished, hal_kg,
    colour = ha_swept
  ), alpha = 0.5) +
  scale_colour_viridis_c() +
  facet_wrap(~year) +
  ggsidekick::theme_sleek()


# cc %>%
#   ggplot() +
#   geom_point(aes(dist_km_fished, hook_count,
#                  colour = hooks_per_skate), alpha = 0.5) +
#   scale_colour_viridis_c(trans = "log10") +
#   facet_wrap(~vessel_name) +
#   ggsidekick::theme_sleek() + theme(legend.position = "none")
#

