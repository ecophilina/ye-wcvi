# attempt at uncertainty...
library(dplyr)
library(ggplot2)
library(tidyr)
# Do we need instructions for installing other dependencies like "pbsmapping"?
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
theme_set(ggsidekick::theme_sleek())


# # model_type <- "-trawlinhbllyrs-paired"
# model_type <- "-paired" # no substrate, independent spatiotemporal fields and year effect
# model_type <- "-RW" # no substrate, no year effect and fixed spatial field, random walk instead
#
# # add substrate
# model_type <- "-RW-w-mudd-rock"
# model_type <- "-rocky-muddy"
#
#
# # add substrate + and est for hbll
# model_type <- "-RW-hbll-ests-w-mud-rock"
# model_type <- "-hbll-mw"

model_type <- "-keepable"
model_vars <- "-delta-est-rock-mud"

# model_vars <- "-new-RW-hbll-ests-w-mud-rock"

# i_hal2 <- readRDS(paste0("data-generated/filled-halibut", model_vars, "index-all-S-sim-500.rds"))[[3]]
g_cda <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-cda-sim-500.rds"))[[3]] %>% filter(region== "CDA"
  ) %>% select(X, Y, year, area)

# g_noncda <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars,
#   "-index-cda-sim-500.rds"))[[3]] %>%
#   filter(region %in% c("3CD5A N", "non-CDA 3CD5A S")& area == 400
#   ) %>% select(X, Y, year, area)

g_noncdaN <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars,
  "-index-5A3CDS-outside-cda-sim-500.rds"))[[3]] %>% filter(region %in% c("3CD5A N")
  ) %>% select(X, Y, year, area)

g_noncdaS <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars,
  "-index-5A3CDS-outside-cda-sim-500.rds"))[[3]] %>%
  filter(region %in% c("non-CDA 3CD5A S")
  ) %>% select(X, Y, year, area)

.file <- paste0(model_type, "-100.rds")

if (!file.exists(paste0("data-generated/ye_sims_for_ratios_by_area", model_type, "-50.rds"))) {

# i_hal4 <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "index-all-S-sim-500.rds"))[[4]]
s_hal_cda4 <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-cda-sim-500.rds"))[[4]][,1:100]
s_hal_noncda4N <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-5A3CDN-outside-cda-sim-500.rds"))[[4]][,1:100]
s_hal_noncda4S <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-5A3CDS-outside-cda-sim-500.rds"))[[4]][,1:100] #116571 vs. 116620 for newdata saved with it?
# s_hal_5A4 <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-5A-sim-500.rds"))[[4]]


model_vars <- "-est-rock-mud" # YE

# i_ye4 <- readRDS(paste0("data-generated/filled", model_type, "-yelloweye", model_vars, "index-all-S-sim-500.rds"))[[4]]
s_ye_cda4 <- readRDS(paste0("data-generated/filled", model_type, "-yelloweye", model_vars, "-index-cda-sim-500.rds"))[[4]][,1:100]
s_ye_noncda4N <- readRDS(paste0("data-generated/filled", model_type, "-yelloweye", model_vars, "-index-5A3CDN-outside-cda-sim-500.rds"))[[4]][,1:100]
s_ye_noncda4S <- readRDS(paste0("data-generated/filled", model_type, "-yelloweye", model_vars, "-index-5A3CDS-outside-cda-sim-500.rds"))[[4]][,1:100]
# s_ye_5A4 <- readRDS(paste0("data-generated/filled", model_type, "-yelloweye", model_vars, "-index-5A-sim-500.rds"))[[4]]

model_vars <- "-delta-est-rock-mud" # change back to halibut/ version

s_hal_cda4 <- cbind(g_cda, as.data.frame(s_hal_cda4)) %>% filter(area == 400) %>% select(-area)
s_hal_noncda4N <- bind_cols(g_noncdaN, as.data.frame(s_hal_noncda4N))%>% filter(area == 400) %>% select(-area)
s_hal_noncda4S <- bind_cols(g_noncdaS, as.data.frame(s_hal_noncda4S))%>% filter(area == 400) %>% select(-area)
s_hal_noncda4 <- bind_rows(s_hal_noncda4N, s_hal_noncda4S)

s_ye_cda4 <- bind_cols(g_cda, as.data.frame(s_ye_cda4))%>% filter(area == 400) %>% select(-area)
s_ye_noncda4N <- bind_cols(g_noncdaN, as.data.frame(s_ye_noncda4N))%>% filter(area == 400) %>% select(-area)
s_ye_noncda4S <- bind_cols(g_noncdaS, as.data.frame(s_ye_noncda4S))%>% filter(area == 400) %>% select(-area)
s_ye_noncda4 <- bind_rows(s_ye_noncda4N, s_ye_noncda4S)

.s_ye <- bind_rows(
  # mutate(i_ye2, Area = "Full combined", num_cells = nrow(full_s_grid)),
  # mutate(s_ye_5A4, Area = "5A"),
  mutate(s_ye_noncda4N, Area = "5A3CD N of 50º"),
  mutate(s_ye_noncda4S, Area = "non-CDA 5A3CD S of 50º"),
  mutate(s_ye_noncda4, Area = "non-CDA 5A3CD"),
  mutate(s_ye_cda4, Area = "CDA")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "ye_est")

.s_ye$Area <- ordered(.s_ye$Area, levels = c("CDA"
  , "non-CDA 5A3CD"
  , "5A3CD N of 50º", "non-CDA 5A3CD S of 50º"
  # , "5A", "Full combined"
  ))

.s_hal <- bind_rows(
  # mutate(i_hal2, Area = "Full combined", num_cells = nrow(full_s_grid)),
  # mutate(s_hal_5A4, Area = "5A"),
  mutate(s_hal_noncda4N, Area = "5A3CD N of 50º"),
  mutate(s_hal_noncda4S, Area = "non-CDA 5A3CD S of 50º"),
  mutate(s_hal_noncda4, Area = "non-CDA 5A3CD"),
  mutate(s_hal_cda4, Area = "CDA")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "hal_est")

.s_hal$Area <- ordered(.s_hal$Area, levels = c("CDA"
  , "non-CDA 5A3CD"
  , "5A3CD N of 50º", "non-CDA 5A3CD S of 50º"
  # , "5A", "Full combined"
  ))

saveRDS(.s_hal, paste0("data-generated/hal_sims_for_ratios_by_area", .file))
saveRDS(.s_ye, paste0("data-generated/ye_sims_for_ratios_by_area", .file))
}


# g_5A <- readRDS(paste0("data-generated/filled", model_type, "-halibut", model_vars, "-index-5A-sim-500.rds"))[[3]] %>% select(X, Y, year)
g_cda <- g_cda %>% filter(area == 400) %>% select(-area)
g_noncdaS <- g_noncdaS %>% filter(area == 400) %>% select(-area)
g_noncdaN <- g_noncdaN %>% filter(area == 400) %>% select(-area)
g_noncda <- rbind(g_noncdaN, g_noncdaS)
noncda_2020 <- filter(g_noncda, year == 2020)
cda_2020 <- filter(g_cda, year == 2020)


if (!file.exists(paste0("data-generated/avoiding_ye_sum", .file))) {

.s_hal <- readRDS( paste0("data-generated/hal_sims_for_ratios_by_area", model_type, "-50.rds"))
.s_ye <- readRDS( paste0("data-generated/ye_sims_for_ratios_by_area", model_type, "-50.rds"))

glimpse(.s_hal)
glimpse(.s_ye)

# .dat <- left_join(.s_ye, .s_hal)
# left_join was crashing R so trying this
.s_ye <- .s_ye %>% select(ye_est)
.dat <- cbind(.s_hal, .s_ye)

.dat <- .dat %>% mutate(
  halibut = exp(hal_est) * (100 * 0.0024384 * 0.009144 * 10000), # convert kg/ha to kg/100 hooks
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



saveRDS(maximize_hal_sum, paste0("report-data/maximize_hal_sum", .file))
saveRDS(avoiding_ye_sum, paste0("report-data/avoiding_ye_sum", .file))
}
#

# chose which areas to plot

areas_to_plot <- c(
  # "Full combined", "5A"
  "CDA",
  "non-CDA 5A3CD"
  #, "5A3CD N of 50º", "non-CDA 5A3CD S of 50º"
  )


# #### make plots ####
maximize_hal_sum <- readRDS( paste0("data-generated/maximize_hal_sum", .file))
avoiding_ye_sum <- readRDS( paste0("data-generated/avoiding_ye_sum", .file))


# mean_ye_hal
ggplot(avoiding_ye_sum %>%
    filter(Area %in% areas_to_plot)%>%
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
      , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1)
      , round((nrow(cda_2020))*1.5)
      , round((nrow(cda_2020))*3)
      , round((nrow(cda_2020))*6)
      , round((nrow(cda_2020))*10)
      , round((nrow(cda_2020))*100)
      , round((nrow(noncda_2020)))
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
  # scale_x_continuous(breaks = c(0, 500, 1000)) +
  # coord_cartesian(ylim = c(0.00000001, 0.15)) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "5A3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_colour_brewer(palette = "Set1", direction = 1) +
  ylab("Mean ratio of YE to halibut") +
  xlab(expression("Total area of cells selected to minimize YE ("~km^2~")")) +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

ggsave(paste0("figs/expected_YE_when_avoiding_YE_CI", model_type, "_allcda_filled.png"), width = 6.5, height = 3.5)

# flipped ratio
ggplot(avoiding_ye_sum %>%
    filter(Area %in% areas_to_plot)%>%
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
      , round((nrow(cda_2020))*1.5)
      , round((nrow(cda_2020))*3)
      , round((nrow(cda_2020))*6)
      , round((nrow(cda_2020))*10)
      , round((nrow(cda_2020))*100)
      , round((nrow(noncda_2020)))
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
  #   ylim = c(1,(10001))
  # ) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  ylab("Mean ratio of halibut to YE (ratio + 1)") +
  xlab(expression("Total area of cells selected to minimize YE ("~km^2~")")) +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_hal_when_avoiding_YE_CI.png", width = 4.5, height = 6)
# ggsave("figs/expected_hal_when_avoiding_YE_CI.png", width = 6, height = 3.5)
ggsave(paste0("figs/expected_hal_when_avoiding_YE_CI", model_type, "_allcda_filled.png"), width = 6.5, height = 3.5)

ggplot(maximize_hal_sum %>%
    filter(Area %in% areas_to_plot)%>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1), round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3)
      , round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1), round((nrow(cda_2020))*1.5)
      , round((nrow(cda_2020))*3)
      , round((nrow(cda_2020))*6)
      , round((nrow(cda_2020))*10)
      , round((nrow(cda_2020))*100)
      , round((nrow(noncda_2020)))
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
  # coord_cartesian(# expand = F,
  #   ylim = c(0, 0.5)
  # ) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "5A3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_colour_brewer(palette = "Set1", direction = 1) +
  ylab("Mean ratio of YE to halibut") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_YE_when_maximize_hal_CI.png", width = 4.5, height = 6)
ggsave(paste0("figs/expected_YE_when_maximize_hal_CI", model_type, "_allcda_filled.png"), width = 6.5, height = 3.5)



# mean_ye_hal
ggplot(maximize_hal_sum %>%
    filter(Area %in% areas_to_plot)%>%
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
      , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1), round((nrow(cda_2020))*1.5), round((nrow(cda_2020))*3)
      , round((nrow(cda_2020))*6)
      , round((nrow(cda_2020))*10)
      , round((nrow(cda_2020))*100)
      , round((nrow(noncda_2020)))
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
  # scale_x_continuous(breaks = c(0, 500, 1000)) +
  # scale_y_log10() +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
  # scale_color_identity(name = "Area",
  #   breaks = c("darkgreen","darkblue", "red"),
  #   labels = c("5A", "5A3CD (non-CDA)", "CDA"),
  #   guide = "legend") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_colour_brewer(palette = "Set1", direction = 1) +
  ylab("Mean ratio of halibut to YE") +
  xlab("Total area of cells selected to maximize halibut (km2)") +
  # facet_wrap(~pair_name, ncol = 2)+
  # theme(legend.position = c(0.65, 0.1))
  facet_wrap(~pair_name, ncol = 4)+
  theme(legend.position = c(0.85, 0.2))

# ggsave("figs/expected_hal_when_maximize_hal_CI.png", width = 4.5, height = 6)
ggsave(paste0("figs/expected_hal_when_maximize_hal_CI", model_type, "_allcda_filled.png"), width = 6.5, height = 3.5)


# chose which areas to plot

areas_to_plot <- c(
  # "Full combined", "5A",
  "non-CDA 5A3CD",
 # "non-CDA 5A3CD S of 50º",  "5A3CD N of 50º",
  "CDA")


avoiding_ye_sum$Area <- ordered(avoiding_ye_sum$Area, levels =  c(
  "CDA", "non-CDA 5A3CD", "5A3CD N of 50º", "non-CDA 5A3CD S of 50º"
))

maximize_hal_sum$Area <- ordered(maximize_hal_sum$Area, levels =  c(
  "CDA", "non-CDA 5A3CD", "5A3CD N of 50º", "non-CDA 5A3CD S of 50º"))




p1 <- ggplot(avoiding_ye_sum %>%
    filter(Area %in% areas_to_plot)%>%
    filter(pair_name =="2019-2020")%>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.0005), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02),
      round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
      , round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3), round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1)
      , round((nrow(cda_2020))*1.5), round((nrow(cda_2020))*1.5)
      , round((nrow(cda_2020))*2), round((nrow(cda_2020))*10)
      , round((nrow(cda_2020))*100)
      , round((nrow(cda_2020))*1000)
      , round((nrow(cda_2020))*5000)
      , round((nrow(noncda_2020)))
      # , round((nrow(cda_2020))*100000)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = c(10, 100, 1000, "10000", "100000")) +
  coord_cartesian(ylim = c(0.00000001, 0.5)) +
  # labs(tag="A")+
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_colour_brewer(palette = "Set1", direction = 1) +
  ylab("Mean ratio of YE to halibut") +
  xlab(expression("Total area of cells ("~km^2~")")) +
  ggtitle("B. Minimizing YE ") +
  theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    legend.position = "none"
    )

p2 <- ggplot(maximize_hal_sum %>%
    filter(Area %in% areas_to_plot)%>%
    filter(pair_name =="2019-2020")%>%
    filter(ordered %in% c(
      round((nrow(cda_2020))*.0005), round((nrow(cda_2020))*.005),
      round((nrow(cda_2020))*.01),
      round((nrow(cda_2020))*.02), round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
      round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1), round((nrow(cda_2020))*.15)
      , round((nrow(cda_2020))*.2)
      , round((nrow(cda_2020))*.3)
      , round((nrow(cda_2020))*.4)
      , round((nrow(cda_2020))*.5)
      , round((nrow(cda_2020))*.75), round((nrow(cda_2020))*1), round((nrow(cda_2020))*1.5)
      , round((nrow(cda_2020))*2), round((nrow(cda_2020))*10), round((nrow(cda_2020))*100),
      round((nrow(cda_2020))*500),
      round((nrow(cda_2020))*1000), round((nrow(cda_2020))*5000), round((nrow(noncda_2020)))
      # , round((nrow(cda_2020))*100000)
    ))) +
  geom_line(
    aes(x = ordered*4,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Area), size = 1) +
  geom_ribbon(aes(x = ordered*4,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Area), alpha=0.2) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
  coord_cartesian(# expand = F,
    ylim = c(0.00000001, 0.5)) +
  # labs(tag="B")+
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_colour_brewer(palette = "Set1", direction = 1) +
  ylab("Mean ratio of YE to halibut") +
  xlab(expression("Total area of cells ("~km^2~")")) +
  ggtitle("A. Maximizing halibut") +
  theme(
    # axis.title.y = element_blank(), axis.text.y = element_blank(),
    legend.position = c(0.4,0.2))

p3 <- ggplot(data.frame(l = "lab", x = 1, y = 1)) +
  geom_text(aes(x, y), label = expression("Total area of cells ("~km^2~")"), colour="grey30") +
  theme_void() +
  coord_cartesian(clip = "off")

# xlab <- p1$labels$x
p1$labels$x <- p2$labels$x <- " "
# ylab <- p1$labels$y
# p1$labels$y <- " "
(p2 | p1)/p3 + patchwork::plot_layout(heights = c(15,0.25))
# grid::grid.draw(grid::textGrob(xlab, y = 0.03))
# grid::grid.draw(grid::textGrob(ylab, x = 0.02,rot = 90))

ggsave(paste0("figs/expected_ye_to_hal", model_type, "_both_scenarios.png"), width = 5.5, height = 3)
# ggsave(paste0("figs/expected_ye_to_hal", model_type, "_both_scenarios_NS.png"), width = 7, height = 3.5)


# # flipped ratio
# ggplot(avoiding_ye_sum %>%
#     filter(Area %in% areas_to_plot)%>%
#     filter(pair_name =="2019-2020")%>%
#     filter(ordered %in% c(
#       # round((nrow(cda_2020))*.001), round((nrow(cda_2020))*.005),
#       round((nrow(cda_2020))*.01),
#       round((nrow(cda_2020))*.02),
#       round((nrow(cda_2020))*.03), round((nrow(cda_2020))*.04),
#       round((nrow(cda_2020))*.05), round((nrow(cda_2020))*.1)
#       , round((nrow(cda_2020))*.15)
#       , round((nrow(cda_2020))*.2)
#       , round((nrow(cda_2020))*.3)
#       , round((nrow(cda_2020))*.4)
#       , round((nrow(cda_2020))*.5)
#       , round((nrow(cda_2020))*.75)
#       , round((nrow(cda_2020))*1)
#       , round((nrow(cda_2020))*1.5)
#     ))) +
#   geom_line(
#     aes(x = ordered*4,
#       y = mean_hal_per_ye, #lty = "solid",
#       colour = Area), size = 1) +
#   geom_ribbon(aes(x = ordered*4,
#     ymin = lwr_mean_hal_ye,
#     ymax = upr_mean_hal_ye,
#     fill = Area), alpha=0.2) +
#   # coord_cartesian(
#   #   # expand = F,
#   #   # xlim = c(0, nrow(cda_2020)/2),
#   #   ylim = c(1,(10001))
#   # ) +
#   scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) +
#   # scale_y_continuous(breaks = c(1, 10, 100, 1000, 10000)) +
#   # scale_x_continuous(breaks = c(0, 500, 1000, 2000)) +
#   # scale_color_identity(name = "Area",
#   #   breaks = c("darkgreen","darkblue", "red"),
#   #   labels = c("5A", "5A3CD (non-CDA)", "CDA"),
#   #   guide = "legend") +
#   scale_fill_brewer(palette = "Set1", direction = -1) +
#   scale_colour_brewer(palette = "Set1", direction = -1) +
#   ylab("Mean ratio of halibut to YE (ratio + 1)") +
#   xlab(expression("Total area of cells selected to minimize YE ("~km^2~")")) +
#   # facet_wrap(~pair_name, ncol = 2)+
#   # theme(legend.position = c(0.65, 0.1))
#   theme(legend.position = c(0.85, 0.2))
