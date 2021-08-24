library(readr)

# just_catch_counts <- read_csv("data/just_catch_counts.csv")
# glimpse(just_catch_counts)
# something seems to be wrong with this data frame... possibly sets duplicated across FOS IDS?

halhblldata <- readRDS("data/halibut-surv-sets-all.rds") %>% filter(survey_abbrev == "HBLL OUT S")

total_catch_counts <- halhblldata %>%
  group_by(year) %>% summarise(catch = sum(catch_count, na.rm = T)) %>% select(year, catch) #%>% distinct()

just_offloads <- read_csv("data/just_offloads.csv") %>% filter(OFFLOAD_COUNT != 'NULL') %>% mutate(OFFLOAD_COUNT = as.double(OFFLOAD_COUNT))
glimpse(just_offloads)

# just_catch_counts <- just_catch_counts %>%
#   rename(survey = `HBLL SURVEY`, year = YEAR)

total_catch_counts <- just_catch_counts %>%
  group_by(survey, year, FOS_TRIP_ID) %>% summarise(catch = sum(CATCH_COUNT, na.rm = T)) %>% select(survey, year, catch) %>% distinct()

ids <- just_catch_counts %>% select(survey, year, FOS_TRIP_ID) %>% distinct()

offloads <- left_join(just_offloads, ids) %>% group_by(survey, year) %>% summarise(kept_count = sum(OFFLOAD_COUNT, na.rm = T),
  landed_mass = sum(LANDED_ROUND_WEIGHT_KG, na.rm = T)
  ) %>% filter(survey == "SOUTH") # only considering the southern survey area

offloads <- left_join(total_catch_counts, offloads) %>% mutate(
  landed_mass_per_fish_caught = landed_mass/catch, prop_kept =  kept_count/catch)

# %>% mutate(
#   landed_mass_per_fish_caught = (LANDED_ROUND_WEIGHT_KG)/catch,
#   count_released = catch - (OFFLOAD_COUNT),
#   prop_released = count_released/catch,
#   prop_kept =  (OFFLOAD_COUNT)/catch
#   ) #




ggplot(offloads, aes(prop_kept)) + geom_histogram() + geom_vline(xintercept = mean(offloads$prop_kept)) +
  facet_wrap(~year)


halsampledata_allsizes <- readRDS("data/halibut-surv-samples-all.rds") %>% filter(survey_abbrev == c("SYN WCVI", "SYN QCS"))
ggplot(halsampledata_allsizes, aes(length)) + geom_histogram() + geom_vline(xintercept = 81.3)


halsampledata2 <- halsampledata_allsizes %>% select(fishing_event_id, sex, length, weight) %>% mutate(
  log_length = log(length),
  log_weight = -11.94 + (3.14 * log_length) , # sex is missing for many fish, so will use F coefficients from synopsis report for all
  est_weight = exp(log_weight)*1000,
  new_weight = if_else(!is.na(weight), as.numeric(weight)/1000,  as.numeric( exp(log_weight)))
)

ggplot(halsampledata2, aes(weight,est_weight)) + geom_point() + geom_abline(b=1, a=1)


# estimated weight of fish at min length threshold for keeping
exp(-11.94 + (3.14 * log(81.3)))


# proportion of trawl halibut biomass that belongs to halibut that were large enough to keep
small <- filter(halsampledata2, length < 81.3)
sum_small <- sum(small$est_weight/1000, na.rm = T)
N_small <- nrow(small)

keepers <- filter(halsampledata2, length >= 81.3)
sum_keepers <- sum(keepers$est_weight/1000, na.rm = T)
N_keepers <- nrow(keepers)

sum_keepers/(sum_keepers + sum_small) # almost 38% of biomass is from fish large enough to keep
N_keepers/(N_keepers + N_small) # almost 20% of individuals are large enough to keep

# the proportion of individuals kept on the HBLL appears to be 34%
mean(offloads$prop_kept, na.rm = T)

mean(offloads$landed_mass_per_fish_caught, na.rm = T) # 4.25 kg per fish caught


# fishing event specific ratios of small to large biomass...
halsampledata3 <- halsampledata2 %>% group_by(fishing_event_id) %>% summarise(
  sample_N = n(),
  sample_mass = sum(est_weight, na.rm = T),
  small_mass = sum(ifelse(length < 81.3, est_weight, 0), na.rm = T),
  keeper_mass = sum(ifelse(length >= 81.3, est_weight, 0), na.rm = T),
  small_N = sum(ifelse(length < 81.3, 1, 0), na.rm = T),
  keeper_N = sum(ifelse(length >= 81.3, 1, 0), na.rm = T)
)

haltrawldata2 <- left_join(haltrawldata, halsampledata3) %>% mutate(
  ratio_keepers = keeper_mass/sample_mass,
  mass_keepers = ifelse(catch_weight < 6, 0, ratio_keepers * catch_weight),
  prop_sampled = ifelse(catch_weight == 0, 1, ifelse((sample_mass/1000)/catch_weight >1, 1, (sample_mass/1000)/catch_weight))
)

ggplot(filter(haltrawldata2, prop_sampled > 0.25), aes( sample_mass, keeper_N, colour = prop_sampled)) + geom_point()


# IPHC
exp(-11.94 + (3.14 * log(81.3)))

