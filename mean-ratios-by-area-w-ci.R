# attempt at uncertainty...
library(dplyr)
library(ggplot2)
library(tidyr)
# Do we need instructions for installing other dependencies like "pbsmapping"?
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
theme_set(ggsidekick::theme_sleek())
# i_hal2 <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-all-S-sim-500.rds")[[3]]
g_cda <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-cda-sim-500.rds")[[3]] %>% select(X, Y, year)
g_noncda <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-3CD-outside-cda-sim-500.rds")[[3]] %>% select(X, Y, year)
g_5A <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-5A-sim-500.rds")[[3]] %>% select(X, Y, year)

cda_2020 <- filter(g_cda, year ==2020)

# i_hal4 <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-all-S-sim-500.rds")[[4]]
s_hal_cda4 <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-cda-sim-500.rds")[[4]]
s_hal_noncda4 <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-3CD-outside-cda-sim-500.rds")[[4]]
s_hal_5A4 <- readRDS("data-generated/hybrid-halibut-trawlinhbllyrs-paired-index-5A-sim-500.rds")[[4]]


# i_ye4 <- readRDS("data-generated/hybrid-yelloweye-trawlinhbllyrs-paired-index-all-S-sim-500.rds")[[4]]
s_ye_cda4 <- readRDS("data-generated/hybrid-yelloweye-trawlinhbllyrs-paired-index-cda-sim-500.rds")[[4]]
s_ye_noncda4 <- readRDS("data-generated/hybrid-yelloweye-trawlinhbllyrs-paired-index-3CD-outside-cda-sim-500.rds")[[4]]
s_ye_5A4 <- readRDS("data-generated/hybrid-yelloweye-trawlinhbllyrs-paired-index-5A-sim-500.rds")[[4]]

s_hal_cda4 <- bind_cols(g_cda, as.data.frame(s_hal_cda4))
s_hal_noncda4 <- bind_cols(g_noncda, as.data.frame(s_hal_noncda4))
s_hal_5A4 <- bind_cols(g_5A, as.data.frame(s_hal_5A4))
s_ye_cda4 <- bind_cols(g_cda, as.data.frame(s_ye_cda4))
s_ye_noncda4 <- bind_cols(g_noncda, as.data.frame(s_ye_noncda4))
s_ye_5A4 <- bind_cols(g_5A, as.data.frame(s_ye_5A4))

.s_ye <- bind_rows(
  # mutate(i_ye2, Area = "Full combined", num_cells = nrow(full_s_grid)),
  mutate(s_ye_5A4, Area = "5A"),
  mutate(s_ye_noncda4, Area = "non-CDA 3CD"),
  mutate(s_ye_cda4, Area = "CDA")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "ye_est")
.s_ye$Area <- ordered(.s_ye$Area, levels = c("CDA", "non-CDA 3CD", "5A", "Full combined"))

.s_hal <- bind_rows(
  # mutate(i_hal2, Area = "Full combined", num_cells = nrow(full_s_grid)),
  mutate(s_hal_5A4, Area = "5A"),
  mutate(s_hal_noncda4, Area = "non-CDA 3CD"),
  mutate(s_hal_cda4, Area = "CDA")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "hal_est")

.s_hal$Area <- ordered(.s_hal$Area, levels = c("CDA", "non-CDA 3CD", "5A", "Full combined"))

.dat <- left_join(.s_ye, .s_hal) %>% mutate(
  halibut = exp(hal_est) * (100 * 0.0024384 * 0.009144 * 10000)*1.33 , # convert kg/ha to kg/100 hooks and apply adams high hal mass
  yelloweye = exp(ye_est) * (100 * 0.0024384 * 0.009144 * 10000), # convert kg/ha to kg/100 hooks
  ye_per_hal = yelloweye / (halibut), # not correcting for <1
  hal_per_ye = (halibut) / yelloweye # not correcting for <1
)

avoiding_ye <- .dat[order(.dat$yelloweye, decreasing = F), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    hal_cumsum2 = ifelse(cumsum(halibut) < 0.01, 0.01, cumsum(halibut)),
    ye_cumsum = cumsum(yelloweye),
    ye_cumsum2 = ifelse(cumsum(yelloweye) < 0.01, 0.01, cumsum(yelloweye)),
    cumsum_ye_hal = (ye_cumsum*100)/(hal_cumsum2*100),
    cumsum_hal_ye = (hal_cumsum*100)/(ye_cumsum2*100),
    hal_mean2 = ifelse((hal_cumsum*100)/ordered < 1, 1, (hal_cumsum*100)/ordered),
    mean_ratio = (ye_cumsum*100/ordered)/hal_mean2,
    ye_mean2 = ifelse((ye_cumsum*100)/ordered < 1, 1, (ye_cumsum*100)/ordered),
    mean_hal_ratio = (hal_cumsum*100/ordered)/ye_mean2,
    proportion = ordered/nrow(cda_2020),
    pair_name = case_when(
      year %in% c(2007, 2008) ~ "2007-2008",
      year %in% c(2009, 2010) ~ "2009-2010",
      year %in% c(2011, 2012) ~ "2011-2012",
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020"
    )) %>% ungroup()

# check that denominator was successfully truncated to 1
# before
range(avoiding_ye$ye_cumsum*100/avoiding_ye$ordered)
# after
range(avoiding_ye$ye_mean2)
# before
range(avoiding_ye$hal_cumsum*100/avoiding_ye$ordered)
# after
range(avoiding_ye$hal_mean2)

avoiding_ye_sum <- avoiding_ye %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    # lwr_hal_cumsum = quantile(hal_cumsum, 0.025),
    # upr_hal_cumsum = quantile(hal_cumsum, 0.975),
    # lwr_ye_cumsum = quantile(ye_cumsum, 0.025),
    # upr_ye_cumsum = quantile(ye_cumsum, 0.975),
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye = ifelse(quantile(mean_hal_ratio, 0.025)<1, 1, quantile(mean_hal_ratio, 0.025)),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    halibut = mean(halibut),
    yelloweye = mean(yelloweye),
    mean_hal_per_ye = mean(mean_hal_ratio)
  ) %>% ungroup()


#### maximizing halibut strategy ###
maximize_hal <- .dat[order(.dat$halibut, decreasing = T), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    hal_cumsum2 = ifelse(cumsum(halibut) < 0.01, 0.01, cumsum(halibut)),
    ye_cumsum = cumsum(yelloweye),
    ye_cumsum2 = ifelse(cumsum(yelloweye) < 0.01, 0.01, cumsum(yelloweye)),
    cumsum_ye_hal = (ye_cumsum*100)/(hal_cumsum2*100),
    cumsum_hal_ye = (hal_cumsum*100)/(ye_cumsum2*100),
    hal_mean2 = ifelse((hal_cumsum*100)/ordered < 1, 1, (hal_cumsum*100)/ordered),
    mean_ratio = (ye_cumsum*100/ordered)/hal_mean2,
    ye_mean2 = ifelse((ye_cumsum*100)/ordered < 1, 1, (ye_cumsum*100)/ordered),
    mean_hal_ratio = (hal_cumsum*100/ordered)/ye_mean2,
    proportion = ordered/nrow(cda_2020),
    pair_name = case_when(
      year %in% c(2007, 2008) ~ "2007-2008",
      year %in% c(2009, 2010) ~ "2009-2010",
      year %in% c(2011, 2012) ~ "2011-2012",
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020"
    )) %>% ungroup()
# check that denominator was successfully truncated to 1
# before
range(maximize_hal$ye_cumsum*100/maximize_hal$ordered)
# after
range(maximize_hal$ye_mean2)
# before -- not needed
range(maximize_hal$hal_cumsum*100/maximize_hal$ordered)

maximize_hal_sum <- maximize_hal %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    # lwr_hal_cumsum = quantile(hal_cumsum, 0.025),
    # upr_hal_cumsum = quantile(hal_cumsum, 0.975),
    # lwr_ye_cumsum = quantile(ye_cumsum, 0.025),
    # upr_ye_cumsum = quantile(ye_cumsum, 0.975),
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye = ifelse(quantile(mean_hal_ratio, 0.025)<1, 1, quantile(mean_hal_ratio, 0.025)),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    halibut = mean(halibut),
    yelloweye = mean(yelloweye),
    mean_hal_per_ye = mean(mean_hal_ratio)
  ) %>% ungroup()

range(maximize_hal_sum$lwr_mean_hal_ye)

maximize_hal_sum_cda <- maximize_hal_sum %>% filter(Area == "CDA")
range(maximize_hal_sum_cda$mean_ye_per_hal)


avoiding_ye_sum$Area <- ordered(avoiding_ye_sum$Area, levels = c("5A", "non-CDA 3CD", "CDA", "Full combined"))
maximize_hal_sum$Area <- ordered(maximize_hal_sum$Area, levels = c("5A", "non-CDA 3CD", "CDA", "Full combined"))

saveRDS(maximize_hal_sum, "data-generated/maximize_hal_sum.rds")
saveRDS(avoiding_ye_sum, "data-generated/avoiding_ye_sum.rds")

#### make plots ####

# mean_ye_hal
ggplot(avoiding_ye_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02),
      round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      , round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      # , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_x_continuous(breaks = c(0, 500, 1000)) +
  coord_cartesian(# expand = F, # xlim = c(0, nrow(cda_2020)/2),
    ylim = c(0, 0.03)) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  ylab("Mean ratio of YE to halibut") +
  xlab("Total area of cells selected to minimize yelloweye (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_YE_when_avoiding_YE_CI.png", width = 4.5, height = 6)
ggsave("figs/expected_YE_when_avoiding_YE_CI.png", width = 6, height = 3.5)

# flipped ratio
ggplot(avoiding_ye_sum %>%
    filter(ordered %in% c(
      # round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02),
      round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      , round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3)
      , round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      , round((nrow(cda_2020))*.75)
      , round((nrow(cda_2020))*1)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_hal_per_ye, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_hal_ye,
    ymax = upr_mean_hal_ye,
    fill = Area), alpha=0.2) +
  # coord_cartesian(
  #   # expand = F,
  #   # xlim = c(0, nrow(cda_2020)/2),
  #   ylim = c(0,2000)
  # ) +
  #
  scale_y_log10() +
  scale_x_continuous(breaks = c(0, 500, 1000, 2000)) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  ylab("Mean ratio of halibut to YE (ratio + 1)") +
  xlab("Total area of cells selected to minimize yelloweye (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_hal_when_avoiding_YE_CI.png", width = 4.5, height = 6)
# ggsave("figs/expected_hal_when_avoiding_YE_CI.png", width = 6, height = 3.5)
ggsave("figs/expected_hal_when_avoiding_YE_CI2.png", width = 6, height = 3.5)

ggplot(maximize_hal_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1), round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3)
      , round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      # , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_x_continuous(breaks = c(0, 500, 1000)) +
  coord_cartesian(# expand = F,
    ylim = c(0, 0.5)
  ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  ylab("Mean ratio of YE to halibut") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_YE_when_maximize_hal_CI.png", width = 4.5, height = 6)
ggsave("figs/expected_YE_when_maximize_hal_CI.png", width = 6, height = 3.5)



# mean_ye_hal
ggplot(maximize_hal_sum %>%
    filter(ordered %in% c(
      # round((nrow(cda_2020))*.001), # 1 cell
      # round((nrow(cda_2020))*.005), # 3 cells
      round((nrow(cda_2020))*.01), # 6 cells
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1), round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3)
      , round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      # , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_hal_per_ye, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_hal_ye,
    ymax = upr_mean_hal_ye,
    fill = Area), alpha=0.2) +
  # coord_cartesian(# expand = F, #
  #   xlim = c(0, round(nrow(cda_2020)*.2)*4),
  #   ylim = c(0, 1000)
  # ) +
  scale_x_continuous(breaks = c(0, 500, 1000)) +
  scale_y_log10() +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  ylab("Mean ratio of halibut to YE") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_hal_when_maximize_hal_CI.png", width = 4.5, height = 6)
ggsave("figs/expected_hal_when_maximize_hal_CI.png", width = 6, height = 3.5)




