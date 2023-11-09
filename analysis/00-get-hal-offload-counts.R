# calculation of average kg of commercially retainable halibut per fish caught in HBLL
# this script requires just_offloads.csv and just_catch_counts.csv files to be in the data folder
# these data are privacy protected and were retrieved from the databases for us by the Groundfish Data Unit

library(readr)
library(tidyverse)


halhblldata <- readRDS("data/halibut-surv-sets-all.rds") %>% filter(survey_abbrev == "HBLL OUT S")

total_catch_counts <- halhblldata %>% group_by(year) %>% summarise(catch = sum(catch_count, na.rm = T)) %>% select(year, catch) #%>% distinct()

just_offloads <- read.csv("data/just_offloads.csv") %>%
  filter(OFFLOAD_COUNT != 'NULL') %>%
  mutate(OFFLOAD_COUNT = as.double(OFFLOAD_COUNT))

glimpse(just_offloads)

ids <- read.csv("data/just_catch_counts.csv") %>% rename(survey = HBLL.SURVEY, year = YEAR) %>% select(survey, year, FOS_TRIP_ID) %>% distinct()

offloads <- left_join(just_offloads, ids) %>% group_by(survey, year) %>%
  summarise(offload_count = sum(OFFLOAD_COUNT, na.rm = T),
  landed_round_weight = sum(LANDED_ROUND_WEIGHT_KG, na.rm = T)
  ) %>% filter(survey == "SOUTH") # only considering the southern survey area

offloads <- left_join(total_catch_counts, offloads) %>% mutate(
  landed_weight_per_fish_caught = landed_round_weight/catch, prop_kept = offload_count/catch)

ggplot(offloads, aes(year, prop_kept)) + geom_point() + ylim(0,1)

ggplot(offloads, aes(year, landed_weight_per_fish_caught)) +
  geom_point(aes(size = catch), colour = "black") +
  # geom_point(aes(size = offload_count), colour = "red") +
  geom_hline(yintercept = mean(offloads$landed_weight_per_fish_caught, na.rm = TRUE)) +
  ylim(0,6)+
  ylab("Weight of landed halibut per halibut caught") + xlab("Survey year") +
  scale_size_continuous(limits = c(1500, 3000), name = "Total halibut count") +
  theme(legend.position = c(0.2, 0.3))
ggsave("figs/hbll-landable-weight-by-year.png", width = 5, height = 4)

# the proportion of individuals kept on the HBLL appears to be 34%
mean(offloads$prop_kept, na.rm = T)

mean_kg_per_fish <- mean(offloads$landed_weight_per_fish_caught, na.rm = T) # 4.23 kg per fish caught
min_kg_per_fish <- min(offloads$landed_weight_per_fish_caught, na.rm = T) # 2.62 kg per fish caught
max_kg_per_fish <- max(offloads$landed_weight_per_fish_caught, na.rm = T) # 5.31 kg per fish caught

# overall mean is lower than the mean and annual means...
sum(offloads$landed_round_weight, na.rm = T)/sum(offloads$catch, na.rm = T)


# low_multiplier
min_kg_per_fish/mean_kg_per_fish

# high_multiplier
max_kg_per_fish/mean_kg_per_fish


# mean weight of offloaded fish only
  sum(offloads$landed_mass, na.rm = T)/
  sum(offloads$kept_count, na.rm = T) # 12.448
