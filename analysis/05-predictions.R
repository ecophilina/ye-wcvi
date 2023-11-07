# generate predictions
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
library(sdmTMBextra)
theme_set(ggsidekick::theme_sleek())
# load misc custom functions
# includes a set of map boundaries that could be adjusted
source("analysis/functions.R")

# select which scale of grid to use
# grid_scale <- 2000
grid_scale <- 1000

latitude_cutoff <- 51
max_map_lat <- 51

cols <- c(
  "red",
  #"darkorchid4",
  "deeppink4",
  "deepskyblue4"
  # "cadetblue3"
)

# include_cc <- FALSE
include_cc <- TRUE # including non-survey catches

if (include_cc) {
obs_cols <- c("white", "#98FB98", "#FFDAB9")
} else{
obs_cols <- c("white", "#98FB98")
}

# # # load models if 03 not just run
# ye_model <- "w-deeper-all-yrs-450kn-true-year-RW-aniso"
# hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso"
ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-may23"
hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"

f <- paste0("models/halibut-model-", hal_model, "-stan.rds")
if (file.exists(f)) {
  m_hal_fixed <- readRDS(paste0("models/halibut-model-", hal_model, "-tmb.rds"))
  m_hal_stan <- readRDS(f)
}

f2 <- paste0("models/yelloweye-model-", ye_model, "-stan.rds")
if (file.exists(f2)) {
  m_ye_fixed <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))
  m_ye_stan <- readRDS(f2)
}

# load grid and add in fyear and dummy vessel id
full_s_grid <- readRDS(paste0("data-generated/full_filled_grid_w_ext_", grid_scale,".rds")) %>%
  filter(latitude <= latitude_cutoff) %>%
  mutate(fyear = as.factor(year),
         vessel_id = as.factor("survey"))

# add true year to work with RW versions of models
full_s_grid2 <- full_s_grid %>% filter(year == 2020) %>%
  replicate_df(., time_name = "year_true", time_values = unique(m_hal_fixed$data$year_true)) %>%
  mutate(
    year = year_true,
    year_pair = case_when(
      year %in% c(2007, 2008) ~ 2008,
      year %in% c(2009, 2010) ~ 2010,
      year %in% c(2011, 2012) ~ 2012,
      year %in% c(2013, 2014) ~ 2014,
      year %in% c(2015, 2016) ~ 2016,
      year %in% c(2017, 2018) ~ 2018,
      year %in% c(2019, 2020) ~ 2020  # no WCVI
    ),
    fyear = as.factor(year_pair)
  )

# grid_scale <- "1000-post"

f <- paste0("data-generated/halibut-", hal_model, "_", grid_scale, "-index-all-stan.rds")
if (!file.exists(f)) {
  if(m_hal_fixed$time == "year_true"){
    grid <- full_s_grid2
    }else{
    grid <- full_s_grid
    }
  i_hal <- get_all_sims(m_hal_fixed, newdata = grid, tmbstan_model = m_hal_stan)
  i_hal <- setNames(i_hal, c("all", paste(unique(grid$region))))
  saveRDS(i_hal, f)
} else{
  i_hal <- readRDS(f)
}

f <- paste0("data-generated/yelloweye-", ye_model, "_", grid_scale, "-index-all-stan.rds")
if (!file.exists(f)) {
  if(m_hal_fixed$time == "year_true"){
    grid <- full_s_grid2
  }else{
    grid <- full_s_grid
  }
  i_ye <- get_all_sims(m_ye_fixed, newdata = grid, tmbstan_model = m_ye_stan)
  i_ye <- setNames(i_ye, c("all", paste(unique(grid$region))))
  saveRDS(i_ye, f)
} else{
  i_ye <- readRDS(f)
}

i_hal_all <- i_hal$all$index
i_ye_all <- i_ye$all$index



ggplot(i_hal_all, aes(year, est)) + geom_line(colour = "darkgreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkgreen", alpha = 0.2) +
  geom_line(data = i_ye_all, colour = "orange") +
  geom_ribbon(data = i_ye_all, aes(ymin = lwr, ymax = upr), fill = "orange", alpha = 0.2)

i_hal_cda <- i_hal$CDA$index
i_ye_cda <- i_ye$CDA$index

i_hal_ext <- i_hal$`CDA adjacent`$index
i_ye_ext <- i_ye$`CDA adjacent`$index

ggplot(i_hal_cda, aes(year, est)) + geom_line(colour = "darkgreen") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkgreen", alpha = 0.2) +
  geom_line(data = i_ye_cda, colour = "orange") +
  geom_ribbon(data = i_ye_cda, aes(ymin = lwr, ymax = upr), fill = "orange", alpha = 0.2) +
  geom_line(data = i_hal_ext, colour = "darkgreen", lty = "dashed") +
  geom_ribbon(data = i_hal_ext, aes(ymin = lwr, ymax = upr), fill = "darkgreen", alpha = 0.2) +
  geom_line(data = i_ye_ext, colour = "orange", lty = "dashed") +
  geom_ribbon(data = i_ye_ext, aes(ymin = lwr, ymax = upr), fill = "orange", alpha = 0.2)


p_hal <- i_hal$all[[3]]
p_hal_sims <- i_hal$all[[4]]
p_hal$est <- apply(p_hal_sims, 1, function(x) {median(x)})
p_hal$est_sd <- apply(p_hal_sims, 1, function(x) {sd(x)})

p_ye <- i_ye$all[[3]]
p_ye_sims <- i_ye$all[[4]]
p_ye$est <- apply(p_ye_sims, 1, function(x) {median(x)})
p_ye$est_sd <- apply(p_ye_sims, 1, function(x) {sd(x)})


# halibute density maps
# # use TMB predictions instead of stan ones for comparison purposes
#
# hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"
# m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
# final_yr_grid <- full_s_grid %>% filter(year_true == 2020)
# p_hal <- predict(m_hal, newdata = final_yr_grid,
#                      type = "response")

d_hal_plots <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < max_map_lat) %>%
  filter(year %in% c(2018,2019,2020))

p_hal2020 <- p_hal %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)

h_2020 <- map_predictions(
  pred_data = p_hal2020,
  pred_min = 0, #
  # pred_min = min(p_hal2020$est, na.rm = T),
  pred_max = quantile(p_hal2020$est, 0.995, na.rm = T),
  # pred_max = max(p_hal2020$est, na.rm = T),
  # obs_data = d_hal_plots,
  # obs_col = obs_cols,
  fill_lab = "Predicted kg/ha",
  map_lat_limits = c(min_map_lat , max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Landable kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
# h_2020

ggsave(paste0("figs/halibut-", hal_model, "-", grid_scale, "-map-2020.png"),
       width = 6, height = 5,
       dpi = 400)

# p_hal2020c <- p_hal2020 %>%
#   filter(latitude < max_map_lat2)
#
# d_hal2 <- d_hal_plots %>%
#   filter(latitude < max_map_lat2)
#
# g <- map_predictions(
#   pred_data = p_hal2020c,
#   pred_min = 0, #min(p_hal2020c$est, na.rm = T),
#   pred_max = quantile(p_hal2020c$est, 0.995, na.rm = T),
#   # obs_data = d_hal2,
#   # obs_col = obs_cols,
#   fill_lab = "Predicted\nkg/ha",
#   max_size_obs = 6,
#   map_lat_limits = c(min_map_lat2, max_map_lat2),
#   map_lon_limits = c(min_map_lon2, max_map_lon2),
#   size_lab = "Landable\nkg/ha", #
#   size_aes = density
# ) + theme(legend.box = 'horizontal') +
#   guides(colour = "none")
# # g
# ggsave(paste0("figs/halibut-", hal_model, "_", grid_scale, "-map-2020-closeup.png"), width = 6, height = 5, dpi = 400)

# d_hal_plots2 <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
#   filter(latitude < max_map_lat)

# p_hal_mean <- p_hal %>%
#   filter(year > 2015) %>%
#   filter(latitude < max_map_lat) %>%
#   group_by(x,y,X,Y,longitude,latitude) %>%
#   summarise(est = mean(est))
#
# g <- map_predictions(
#   pred_data = p_hal_mean,
#   fill_aes = est,
#   pred_min = 0,
#   pred_max = quantile(p_hal_mean$est, 0.995, na.rm = T),
#   fill_lab = "Predicted kg/ha", #\n
#   # obs_data = d_hal_plots2,
#   # obs_col = obs_cols,
#   map_lat_limits = c(min_map_lat, max_map_lat),
#   map_lon_limits = c(min_map_lon, max_map_lon),
#   size_lab = "Observed kg/ha",
#   size_aes = density
# ) + theme(legend.spacing.y = unit(0.1, "cm"))
# # g
#
# ggsave(paste0("figs/halibut-", hal_model, "_", grid_scale, "-map-all.png"),
#        width = 6, height = 5,
#        dpi = 400)
#



# yelloweye density maps

d_ye_plots <- readRDS(("data-generated/yelloweye-model-data-hbll-weights.rds")) %>%
  filter(latitude < 52.15507)  %>% filter(year %in% c(#2017,
    2018, 2019, 2020))

p_ye2020 <- p_ye %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)

y_2020 <- map_predictions(
  pred_data = p_ye,
  fill_aes = est,
  pred_min = 0,
  pred_max = quantile(p_ye$est, 0.995, na.rm = T),
  fill_lab = "Predicted kg/ha", #\n
  # obs_data = d_ye_plots,
  # obs_col = obs_cols,
  map_lat_limits = c(min_map_lat, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Observed kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
# y_2020

ggsave(paste0("figs/yelloweye-", ye_model, "-", grid_scale, "-map-2020.png"),
       width = 6, height = 5,
       dpi = 400)

# d_ye_plots2 <- readRDS(("data-generated/yelloweye-model-data-hbll-weights.rds")) %>%
#   filter(latitude < 52.15507)
# p_ye_mean <- p_ye %>%
#   filter(latitude < max_map_lat) %>%
#   group_by(x,y,X,Y,longitude,latitude) %>%
#   summarise(est = mean(est))
#
# g <- map_predictions(
#   pred_data = p_ye_mean,
#   fill_aes = est,
#   pred_min = 0,
#   pred_max = quantile(p_ye_mean$est, 0.995, na.rm = T),
#   fill_lab = "Predicted kg/ha", #\n
#   # obs_data = d_ye_plots2,
#   # obs_col = obs_cols,
#   map_lat_limits = c(min_map_lat, max_map_lat),
#   map_lon_limits = c(min_map_lon, max_map_lon),
#   size_lab = "Observed kg/ha",
#   size_aes = density
# ) + theme(legend.spacing.y = unit(0.1, "cm"))
# # g
#
# ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-all.png"),
#        width = 6, height = 5,
#        dpi = 400)
#
#
#
# p_ye2020c <- p_ye2020 %>%
# filter(latitude < max_map_lat2)
#
# # d_ye2 <- d_ye_plots %>%
# #   filter(latitude < max_map_lat2)
#
# g <- map_predictions(
#   pred_data = p_ye2020c,
#   pred_min = 0, #min(p_ye2020c$est, na.rm = T),
#   pred_max = quantile(p_ye2020c$est, 0.995, na.rm = T),
#   # pred_max = max(p_ye2020c$est),
#   # obs_data = d_ye2,
#   # obs_col = obs_cols,
#   fill_lab = "Predicted\nkg/ha",
#   max_size_obs = 6,
#   map_lat_limits = c(min_map_lat2, max_map_lat2),
#   map_lon_limits = c(min_map_lon2, max_map_lon2),
#   size_lab = "Landable\nkg/ha", #
#   size_aes = density
# ) + theme(legend.box = 'horizontal') +
#   guides(colour = "none")
# # g
# ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-2020-closeup.png"), width = 6, height = 5, dpi = 400)

y <- y_2020 + ggtitle("(a) Total Yelloweye Rockfish catch weight") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
h <- h_2020 + ggtitle("(b) Landable Pacific Halibut catch weight")

y + h + patchwork::plot_layout(nrow = 2)

ggsave(paste0("figs/ye-", ye_model, "_hal-", hal_model, "-", grid_scale, "-map-2020.png"), width = 6, height = 8, dpi = 400)



# supplemental year facets

d_hal_surv <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < max_map_lat & survey != "NON-SURVEY" & !is.na(year_pair)) %>%
  mutate(pair_name = case_when(
    year %in% c(2007, 2008) ~ "2007-2008",
    year %in% c(2009, 2010) ~ "2009-2010",
    year %in% c(2011, 2012) ~ "2011-2012",
    year %in% c(2013, 2014) ~ "2013-2014",
    year %in% c(2015, 2016) ~ "2015-2016",
    year %in% c(2017, 2018) ~ "2017-2018",
    year %in% c(2019, 2020) ~ "2019-2020"
  ))

p_hal <- p_hal %>%
  filter(latitude < max_map_lat) %>%
  mutate(pair_name = case_when(
    year %in% c(2007, 2008) ~ "2007-2008",
    year %in% c(2009, 2010) ~ "2009-2010",
    year %in% c(2011, 2012) ~ "2011-2012",
    year %in% c(2013, 2014) ~ "2013-2014",
    year %in% c(2015, 2016) ~ "2015-2016",
    year %in% c(2017, 2018) ~ "2017-2018",
    year %in% c(2019, 2020) ~ "2019-2020"
  ))

h_all <- map_predictions(
  pred_data = p_hal,
  pred_min = 0, #
  # pred_min = min(p_hal2020$est, na.rm = T),
  pred_max = quantile(p_hal2020$est, 0.995, na.rm = T),
  # pred_max = max(p_hal2020$est, na.rm = T),
  obs_data = d_hal_surv,
  obs_col = obs_cols,
  fill_lab = "Predicted kg/ha",
  map_lat_limits = c(min_map_lat , max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Landable kg/ha",
  size_aes = density
) + facet_wrap(~pair_name, ncol = 2) +
theme(legend.spacing.y = unit(0.1, "cm"),
      panel.spacing.x = unit(1, "lines"),
      legend.position = "right")

ggsave(paste0("figs/halibut-", hal_model, "-", grid_scale, "-map-years.png"),
       width = 8, height = 10,
       dpi = 400)


d_ye_surv <- readRDS(("data-generated/yelloweye-model-data-hbll-weights.rds")) %>%
  filter(latitude < 52.15507 & survey != "NON-SURVEY" & !is.na(year_pair)) %>% mutate(      pair_name = case_when(
    year %in% c(2007, 2008) ~ "2007-2008",
    year %in% c(2009, 2010) ~ "2009-2010",
    year %in% c(2011, 2012) ~ "2011-2012",
    year %in% c(2013, 2014) ~ "2013-2014",
    year %in% c(2015, 2016) ~ "2015-2016",
    year %in% c(2017, 2018) ~ "2017-2018",
    year %in% c(2019, 2020) ~ "2019-2020"
  ))

p_ye <- p_ye %>%
  filter(latitude < max_map_lat) %>%
  mutate(pair_name = case_when(
    year %in% c(2007, 2008) ~ "2007-2008",
    year %in% c(2009, 2010) ~ "2009-2010",
    year %in% c(2011, 2012) ~ "2011-2012",
    year %in% c(2013, 2014) ~ "2013-2014",
    year %in% c(2015, 2016) ~ "2015-2016",
    year %in% c(2017, 2018) ~ "2017-2018",
    year %in% c(2019, 2020) ~ "2019-2020"
  ))

y_all <- map_predictions(
  pred_data = p_ye,
  fill_aes = est,
  pred_min = 0,
  pred_max = quantile(p_ye$est, 0.995, na.rm = T),
  fill_lab = "Predicted kg/ha", #\n
  obs_data = d_ye_surv,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Observed kg/ha",
  size_aes = density
) + facet_wrap(~pair_name, ncol = 2) +
  theme(legend.spacing.y = unit(0.1, "cm"),
        panel.spacing.x = unit(1, "lines"),
        legend.position = "right")

# ideally add a wider space between facets

ggsave(paste0("figs/yelloweye-", ye_model, "-", grid_scale, "-map-years.png"),
       width = 8, height = 10,
       dpi = 400)



# map ratios

pyd <- p_ye %>% rename(ye_est = est) %>% distinct()
phd <- p_hal %>%
  rename(hal_est = est) %>%
  dplyr::select(hal_est, X, Y, year)%>% distinct()

quantile(phd$hal_est, 0.025)
# hist(pyd$ye_est)
#quantile(c(phd$hal_est,pyd$ye_est), 0.025)

ratio_df <- left_join(pyd, phd) %>% mutate(
  halibut = hal_est,
  halibut2 = ifelse(hal_est < 1, 1, hal_est),
  yelloweye = ye_est,
  yelloweye2 = ifelse(ye_est < 1, 1, ye_est),
  ye_per_hal = (yelloweye)/(halibut2),
  hal_per_ye = (halibut)/(yelloweye2),
  hal_per_ye2 = (halibut2)/(yelloweye2)
)

ratio_df_2020 <- filter(ratio_df, year == 2020)

ggplot(ratio_df_2020) + geom_violin(aes(region,log10(ye_per_hal2)))

ratio_df_2020 %>% filter(depth > 20) %>%
  filter(region %in% c("CDA", "CDA adjacent"
                       , "non-CDA 3CD"
                                       )) %>%
  ggplot(aes(depth,log10(ye_per_hal), colour = region, fill = region)) +
  geom_point( alpha = 0.05)+
  geom_smooth() +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)


ggsave("figs/model-raw-ratio-by-depth-2020.png", width = 6, height = 3)


ratio_df_2020 %>%
  filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD"
  )) %>%
  ggplot(aes(depth,log10(hal_per_ye), colour = region, fill = region)) +
  geom_point( alpha = 0.05)+
  geom_smooth() +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

ggsave("figs/model-ratio-hal-to-ye-by-depth-2020.png", width = 6, height = 4)


y2h <- map_predictions(
  map_lat_limits = c(48.4, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  pred_data = ratio_df_2020,
  # pred_min = 0,
  pred_min = min(ratio_df_2020$ye_per_hal),
  # pred_max = max(ratio_df_2020$ye_per_hal),
  pred_max = quantile(ratio_df_2020$ye_per_hal, 0.995),
  fill_aes = ye_per_hal,
  # set_trans = ggsidekick::fourth_root_power_trans(),
  # set_trans = "log10",
  fill_lab = "Ratio"
)
# y2h
# ggsave(paste0("figs/ye-to-halibut-", hal_model, "-2020.png"), width = 6, height = 5, dpi = 400)
#        width = 5.5, height = 4.5, dpi = 400
#        # width = 6, height = 5, dpi = 200

options(scipen=999)
h2y <- map_predictions(
  map_lat_limits = c(48.4, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  pred_data = ratio_df_2020,
  # pred_min = 0,
  pred_min = min(ratio_df_2020$hal_per_ye),
  # pred_max = max(ratio_df_2020$hal_per_ye),
  pred_max = quantile(ratio_df_2020$hal_per_ye, 0.995),
  fill_aes = hal_per_ye,
  # set_trans = "log10",
  # set_trans = ggsidekick::fourth_root_power_trans(),
  fill_lab = "Ratio"
)
# h2y
# ggsave(paste0("figs/halibut-to-ye-truncated-", hal_model, "-2020.png"), width = 6, height = 5, dpi = 400)

y <- y2h + ggtitle("(a) Yelloweye to halibut weight ratios") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
h <- h2y + ggtitle("(b) Halibut to yelloweye weight ratios")

r <- y + h + patchwork::plot_layout(nrow = 2)

# r
# ggsave(paste0("figs/ratios-truncated-to-lower-quantile-for-hal-", grid_scale, "-2020.png"), width = 6, height = 8, dpi = 400)
# r
ggsave(paste0("figs/ratios-truncated-to-1-for-both-", grid_scale, "-post-2020.png"), width = 6, height = 8, dpi = 400)


ggsave(paste0("figs/ratios-ye-", ye_model, "_hal-", hal_model, "-", grid_scale, "-2020.png"), width = 6, height = 8, dpi = 400)



# CV maps

cv_hal <- i_hal$all[[3]]
cv_hal$cv <- apply(p_hal_sims, 1, function(x){ sd(x) / mean(x)})

cv_hal2020 <- cv_hal %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)

cv_ye <- i_ye$all[[3]]
cv_ye$cv <- apply(p_ye_sims, 1, function(x) sd(x) / mean(x))

cv_ye2020 <- cv_ye %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)


g <- map_predictions(
  pred_data = cv_hal2020,
  fill_aes = cv,
  fill_lab = "CV",
  pred_min = min(cv_hal2020$cv,cv_ye2020$cv),
  # pred_max = max(cv_hal2020$cv,cv_ye2020$cv),
  pred_max = quantile(c(cv_hal2020$cv,cv_ye2020$cv), 0.995),
  map_lat_limits = c(min_map_lat3, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon)
)+theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/halibut-", hal_model, "-", grid_scale, "-CV.png"), width = 6, height = 5, dpi = 400)


g <- map_predictions(
  pred_data = cv_ye2020,
  fill_aes = cv,
  fill_lab = "CV",
  pred_min = min(cv_hal2020$cv,cv_ye2020$cv),
  # pred_max = max(cv_hal2020$cv,cv_ye2020$cv),
  pred_max = quantile(c(cv_hal2020$cv,cv_ye2020$cv), 0.995),
  map_lat_limits = c(min_map_lat3, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon)
)+theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/ye-", ye_model, "-", grid_scale, "-CV.png"), width = 6, height = 5, dpi = 400)

