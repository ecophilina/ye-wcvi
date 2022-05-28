# Stitch together trawl and HBLL survey data
library(dplyr)
library(tidyr)
library(ggplot2)


dir.create("data-generated", showWarnings = FALSE)

# load misc custom functions
source("analysis/functions.R")

# Load custom functions including one that takes all the set data and hook data retrieved for each species and produces a dataframe for use in sdmTMB model.

# These models will be of estimated biomass density, so HBLL catch counts are converted to biomass using the mean weight of all fish sampled on the trawl surveys.
# The HBLL biomass estimate is further divided by the "area swept" calculation I found in a hook competition script (total_hooks * 0.0024384 * 0.009144 * 1000) except that I am using density estimates in kg/ha from the trawl instead of kg/km2, so I am multiplying by 10000 instead of 1000.
# It also appears that 0.009144 is the radius not the diameter; therefore, should all be multiplied by 2.
# This results in similar, but not identical, value ranges for both surveys, as would be expected given the different habitats they sample.



model_data_prep <- function(trawldata, hblldata, cc, mean_weight) {
  d <- hblldata

  d <- d %>%
    mutate(
      year_pair = case_when(
        year %in% c(2007, 2008) ~ 2008,
        year %in% c(2009, 2010) ~ 2010,
        year %in% c(2011, 2012) ~ 2012,
        year %in% c(2013, 2014) ~ 2014,
        year %in% c(2015, 2016) ~ 2016,
        year %in% c(2017, 2018) ~ 2018,
        year %in% c(2019, 2020) ~ 2020  # no WCVI
      ),
      year_true = year,
      density = catch_count * mean_weight / (hook_count * 0.0024384 * 0.009144 * 10000 * 2)
      # hook spacing = 0.0024384 km (8 feet?)
      # assumed catch radius = 0.009144 km (30 feet)
      # this area swept calc was for km2, so * 10000 gives it in hectares
    ) %>%
    select(
      survey_abbrev, year_pair, year_true,
      longitude, latitude, fishing_event_id, depth_m, density,
      catch_count, hook_count
    ) %>%
    mutate(lon = longitude, lat = latitude) %>%
    mutate(survey = "HBLL", vessel_id = as.character(paste0(year_true, "survey")))

  dt <- trawldata %>%
    mutate(
      year_pair = case_when(
        year %in% c(2007, 2008) ~ 2008,
        year %in% c(2009, 2010) ~ 2010,
        year %in% c(2011, 2012) ~ 2012,
        year %in% c(2013, 2014) ~ 2014,
        year %in% c(2015, 2016) ~ 2016,
        year %in% c(2017, 2018) ~ 2018,
        year %in% c(2019, 2020) ~ 2020
      ),
      year_true = year,
      density = density_kgpm2 * 10000 # making density of trawl kg per hectare
    ) %>%
    select(
      survey_abbrev, year_pair, year_true,
      longitude, latitude, fishing_event_id, depth_m, density
    ) %>%
    mutate(lon = longitude, lat = latitude) %>%
    mutate(survey = "TRAWL", vessel_id = as.character(paste0(year_true, "survey")))
# browser()
  cc <- cc %>%
    filter(year>2006 & year < 2021) %>%
    mutate(
      year_pair = case_when(
        year %in% c(2007, 2008) ~ 2008,
        year %in% c(2009, 2010) ~ 2010,
        year %in% c(2011, 2012) ~ 2012,
        year %in% c(2013, 2014) ~ 2014,
        year %in% c(2015, 2016) ~ 2016,
        year %in% c(2017, 2018) ~ 2018,
        year %in% c(2019, 2020) ~ 2020
      ),
      year_true = year,
      depth_m = best_depth,
      density = cpue
    ) %>%
    select(year_pair, year_true,
           lon, lat,
           fishing_event_id,
           vessel_id, #vessel_registration_number,
           depth_m, density, dist_km_fished, hook_count
    ) %>%
    mutate(longitude = lon, latitude = lat) %>%
    mutate(survey = "NON-SURVEY")
    # mutate(survey = ifelse(year > 2015, paste0("NON-SURVEY post-2015"), "NON-SURVEY"))



  d <- bind_rows(d, dt, cc) %>%
    filter(depth_m > 1)

  # convert to utms for sdmTMB model
  d_utm <- convert2utm(d, coords = c("lon", "lat"))

  d_utm$depth_log <- log(d_utm$depth_m)
  d_utm$depth_centred <- d_utm$depth_log - mean(d_utm$depth_log)
  d_utm$depth_mean <- mean(d_utm$depth_log)
  d_utm$depth_sd <- sd(d_utm$depth_centred)
  d_utm$depth_scaled <- d_utm$depth_centred / sd(d_utm$depth_centred)
  d_utm$Y_cent <- d_utm$Y - mean(d_utm$Y)
  d_utm$X_cent <- d_utm$X - mean(d_utm$X)

  d_utm
}


# Estimate commercial-sized halibut biomass in both trawl and HBLL data

# trawl
halsampledata <- readRDS("data/halibut-surv-samples-all.rds") %>% filter(survey_abbrev == c("SYN WCVI", "SYN QCS"))

ggplot(halsampledata, aes(length)) + geom_histogram() + geom_vline(xintercept = 81.3) + geom_text(x = 84, y = 300, label = "threshold to keep", angle = 90)

halsampledata2 <- halsampledata %>% select(fishing_event_id, sex, length, weight) %>% mutate(
  log_length = log(length),
  log_weight = -11.94 + (3.14 * log_length) , # sex is missing for many fish, so will use F coefficients from synopsis report for all
  est_weight = exp(log_weight)*1000, # samples are in grams, but total catch and densities are in kg
  new_weight = if_else(!is.na(weight), as.numeric(weight)/1000,  as.numeric(exp(log_weight)))
)

# ggplot(halsampledata2, aes(weight,est_weight)) + geom_point() + geom_abline(b=1, a=1)

# proportion of trawl halibut biomass that belongs to halibut that were large enough to keep
small <- filter(halsampledata2, length < 81.3)
sum_small <- sum(small$est_weight/1000, na.rm = T)
N_small <- nrow(small)

keepers <- filter(halsampledata2, length >= 81.3)
sum_keepers <- sum(keepers$est_weight/1000, na.rm = T)
N_keepers <- nrow(keepers)

trawl_mean_ratio <- sum_keepers/(sum_keepers + sum_small) # almost 38% of biomass is from fish large enough to keep
N_keepers/(N_keepers + N_small) # almost 20% of individuals are large enough to keep

# fishing event specific ratios of small to keeper biomass...
halsampledata3 <- halsampledata2 %>% group_by(fishing_event_id) %>% summarise(
  sample_N = n(),
  sample_mass = sum(est_weight, na.rm = T),
  small_mass = sum(ifelse(length < 81.3, est_weight, 0), na.rm = T),
  keeper_mass = sum(ifelse(length >= 81.3, est_weight, 0), na.rm = T),
  small_N = sum(ifelse(length < 81.3, 1, 0), na.rm = T),
  keeper_N = sum(ifelse(length >= 81.3, 1, 0), na.rm = T)
)

# estimated weight of fish at min length threshold for keeping
min_weight <- exp(-11.94 + (3.14 * log(81.3)))

# calculate biomass density of sufficiently large halibut to be kept...
# use sample-specific biomass ratio if 10 or more fish were measured OR more than 70% of biomass was sampled
# otherwise use global mean ratio from all trawl samples of 38% of halibut biomass belonging to keepers
haltrawldata <- readRDS("data/halibut-surv-sets-all.rds") %>% filter(survey_abbrev %in% c("SYN WCVI", "SYN QCS"))

haltrawldata <- left_join(haltrawldata, halsampledata3) %>% mutate(
  density_allsizes = density_kgpm2,
  sample_N = if_else(sample_N > 0, as.double(sample_N), 0, 0),
  prop_sampled = if_else(catch_weight == 0, 1, if_else((sample_mass/1000)/catch_weight >1, 1, (sample_mass/1000)/catch_weight, 0), 0),
  ratio_keepers = if_else(sample_mass > 0, keeper_mass/sample_mass, trawl_mean_ratio, missing = trawl_mean_ratio),
  density_kgpm2 = ifelse(catch_weight < min_weight, 0, ifelse(prop_sampled > 0.7 | sample_N > 9, ratio_keepers * density_kgpm2, trawl_mean_ratio * density_kgpm2))
)

saveRDS(haltrawldata, "data/halibut-surv-sets-trawl-keepers.rds")

# see offload_counts.R script for calculation of average kg of commercially retainable halibut per fish caught in HBLL



# Check relative sizes of yelloweye caught in both surveys

# print("Mean Yelloweye weight from trawl")
yesampledataSYN <- readRDS("data/yelloweye-surv-samples-all.rds") %>% filter(survey_abbrev == c("SYN WCVI", "SYN QCS"))
yemeanweightSYN <- mean(yesampledataSYN$weight / 1000, na.rm = T)
round(yemeanweightSYN, 2)

# print("Mean Yelloweye weight from HBLL")
yesampledata <- readRDS("data/yelloweye-surv-samples-all.rds") %>% filter(survey_abbrev == c("HBLL OUT S"))
yemeanweightHBLL <- mean(yesampledata$weight / 1000, na.rm = T)
round(yemeanweightHBLL, 2)

# print("Mean Yelloweye weight in commerical data")
# 3.2 # 7 lbs =3.18 kg


# get commercial ll cpue

cc <- readRDS("data/hal-ye-ll-cpue-w-effort.rds") %>% filter(dist_km_fished < 5 & dist_km_fished > 0.1)

# cc$hal_realeased[is.na(cc$hal_realeased)] <- 0
# cc$ye_realeased[is.na(cc$ye_realeased)] <- 0
cc_ye <- cc %>% filter(season == "Summer") %>% mutate(cpue = ye_kg/ha_fished)
cc_hal <- cc %>% filter(season == "Summer") %>% mutate(cpue = hal_kg/ha_fished)

# cc_hal %>%
#   mutate(pre2016 = ifelse(year < 2016, "yes", "no")) %>%
#   ggplot() + geom_violin(aes(pre2016, log(cpue+1))) +
#   facet_wrap(~vessel_registration_number)

# hist(d_hal$density, breaks = 30, xlim = c(0,5))
# hist(cc$hal_kg/1000, breaks = 30, xlim = c(0,5))
#
# hist(d_ye$density, breaks = 30, xlim = c(0,5))
# hist(cc$ye_kg/1000, breaks = 30, xlim = c(0,5), ylim = c(0,15000))


# Prep data

# prep halibut
haltrawldata <- readRDS("data/halibut-surv-sets-trawl-keepers.rds")
halhblldata <- readRDS("data/halibut-surv-sets-all.rds") %>% filter(survey_abbrev == "HBLL OUT S")

halmeanweight <- 4.23 # average kg of commercially retainable halibut per fish caught based on HBLL offload_round_weights/catch_counts

d_hal <- model_data_prep(haltrawldata, halhblldata, cc_hal, mean_weight = halmeanweight) %>%
  mutate(year = year_pair)
# mutate(year = year_true)
saveRDS(d_hal, "data-generated/halibut-model-data-keepable-weight.rds")

# prep yelloweye
yetrawldata <- readRDS("data/yelloweye-surv-sets-all.rds") %>% filter(survey_abbrev %in% c("SYN WCVI", "SYN QCS"))
yehblldata <- readRDS("data/yelloweye-surv-sets-all.rds") %>% filter(survey_abbrev == "HBLL OUT S")

yemeanweight <- round(yemeanweightHBLL, 2)

d_ye <- model_data_prep(yetrawldata, yehblldata, cc_ye, mean_weight = yemeanweight)%>%
  mutate(year = year_pair)
# mutate(year = year_true)
saveRDS(d_ye, "data-generated/yelloweye-model-data-hbll-weights.rds")

years <- sort(unique(d_ye$year))
