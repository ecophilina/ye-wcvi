# attempt at uncertainty...

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
  halibut = exp(hal_est) * (100 * 0.0024384 * 0.009144 * 10000)*1.33 , # convert kg/ha to kg/100 hooks
  yelloweye = exp(ye_est) * (100 * 0.0024384 * 0.009144 * 10000), # convert kg/ha to kg/100 hooks
  ye_per_hal = yelloweye / (halibut),
  hal_per_ye = (halibut) / yelloweye
)

avoiding_ye <- .dat[order(.dat$yelloweye, decreasing = F), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    ye_cumsum = cumsum(yelloweye),
    cumsum_ye_hal = ye_cumsum/(hal_cumsum),
    cumsum_hal_ye = hal_cumsum/(ye_cumsum+1),
    mean_ratio = (ye_cumsum/ordered)/((hal_cumsum)/ordered),
    mean_hal_ratio = (hal_cumsum/ordered)/((ye_cumsum+1)/ordered),
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


avoiding_ye_sum <- avoiding_ye %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    # lwr_hal_cumsum = quantile(hal_cumsum, 0.025),
    # upr_hal_cumsum = quantile(hal_cumsum, 0.975),
    # lwr_ye_cumsum = quantile(ye_cumsum, 0.025),
    # upr_ye_cumsum = quantile(ye_cumsum, 0.975),
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye = quantile(mean_hal_ratio, 0.025),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    halibut = mean(halibut),
    yelloweye = mean(yelloweye),
    mean_hal_per_ye = mean(mean_hal_ratio)
  ) %>% ungroup()

# mean_ye_hal
ggplot(avoiding_ye_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02),
      round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      # , round((nrow(cda_2020))*.2)
      # , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4), round((nrow(cda_2020))*.5)
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
  coord_cartesian(
    # expand = F,
    # xlim = c(0, nrow(cda_2020)/2),
    ylim = c(0, 0.003)
  ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1") + scale_colour_brewer(palette = "Set1") +
  facet_wrap(~pair_name, ncol = 2)+
  ylab("Mean ratio of YE to halibut") +
  xlab("Total area of cells selected to minimize yelloweye (km2)") +
  theme(legend.position = c(0.65, 0.1))

ggsave("figs/expected_YE_when_avoiding_YE_CI.png", width = 7, height = 6)


# flipped ratio
ggplot(avoiding_ye_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02),
      round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      # , round((nrow(cda_2020))*.2)
      # , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4)
      # , round((nrow(cda_2020))*.5)
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
  coord_cartesian(
    # expand = F,
    # xlim = c(0, nrow(cda_2020)/2),
    ylim = c(0, 50000)
  ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1") + scale_colour_brewer(palette = "Set1") +
  facet_wrap(~pair_name, ncol = 2)+
  ylab("Mean ratio of halibut to YE") +
  xlab("Total area of cells selected to minimize yelloweye (km2)") +
  theme(legend.position = c(0.65, 0.1))


ggsave("figs/expected_hal_when_avoiding_YE_CI.png", width = 7, height = 6)

#### maximizing halibut strategy ###
maximize_hal <- .dat[order(.dat$halibut, decreasing = T), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    ye_cumsum = cumsum(yelloweye),
    cumsum_ye_hal = ye_cumsum/hal_cumsum,
    cumsum_hal_ye = hal_cumsum/(ye_cumsum+1),
    mean_ratio = (ye_cumsum/ordered)/(hal_cumsum/ordered),
    mean_hal_ratio = (hal_cumsum/ordered)/((ye_cumsum+1)/ordered),
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


maximize_hal_sum <- maximize_hal %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    # lwr_hal_cumsum = quantile(hal_cumsum, 0.025),
    # upr_hal_cumsum = quantile(hal_cumsum, 0.975),
    # lwr_ye_cumsum = quantile(ye_cumsum, 0.025),
    # upr_ye_cumsum = quantile(ye_cumsum, 0.975),
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye = quantile(mean_hal_ratio, 0.025),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    halibut = mean(halibut),
    yelloweye = mean(yelloweye),
    mean_hal_per_ye = mean(mean_hal_ratio)
  ) %>% ungroup()


ggplot(maximize_hal_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      # , round((nrow(cda_2020))*.2)
      # , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4), round((nrow(cda_2020))*.5)
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
  coord_cartesian(# expand = F,
    ylim = c(0, 0.5)
  ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1") + scale_colour_brewer(palette = "Set1") +
  facet_wrap(~pair_name, ncol = 2, scales = "free_y")+
  ylab("Mean ratio of YE to halibut") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  theme(legend.position = c(0.65, 0.1))


ggsave("figs/expected_YE_when_maximize_hal_CI.png", width = 7, height = 6)


# mean_ye_hal
ggplot(maximize_hal_sum %>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      # , round((nrow(cda_2020))*.2)
      # , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4), round((nrow(cda_2020))*.5)
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
  coord_cartesian(# expand = F, # xlim = c(0, nrow(cda_2020)/2),
    ylim = c(0, 1000)
  ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1") + scale_colour_brewer(palette = "Set1") +
  facet_wrap(~pair_name, ncol = 2)+
  ylab("Mean ratio of halibut to YE") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  theme(legend.position = c(0.65, 0.1))

ggsave("figs/expected_hal_when_maximize_hal_CI.png", width = 7, height = 6)




