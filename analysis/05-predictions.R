# generate predictions

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())
# load misc custom functions
# includes a set of map boundaries that could be adjusted
source("analysis/functions.R")

# select which scale of grid to use
# grid_scale <- 2000
grid_scale <- 1000

cols <- c(
  "red",
  #"darkorchid4",
  "deeppink4",
  "deepskyblue4"
  # "cadetblue3"
)

include_cc <- FALSE
# OR experiment with including non-survey catches
include_cc <- TRUE

if (include_cc) {
obs_cols <- c("white", "#98FB98", "#FFDAB9")
} else{
obs_cols <- c("white", "#98FB98")
}

# load grid and add in fyear and dummy vessel id
full_s_grid <- readRDS(paste0("report-data/full_filled_grid_w_ext_", grid_scale,".rds")) %>%
  mutate(fyear = as.factor(year),
         vessel_id = as.factor("survey"))

# # load models if 03 not just run
hal_model <- "w-effort-500kn-delta-AR1-aniso"
ye_model <- "w-effort-500kn-delta-spatial-aniso"
#
# # hal_model <- "rocky-muddy-300kn-delta-IID-aniso"
# # hal_model <- "w-cc2-rocky-muddy-400kn-delta-IID-aniso"
# # ye_model <- "rocky-muddy-300kn-delta-spatial-aniso"
# # ye_model <- "w-cc2-rocky-muddy-300kn-delta-spatial-aniso"


f <- paste0("models/halibut-model-", hal_model, "-stan.rds")
if (file.exists(f)) {
  m_hal_fixed <- readRDS(paste0("models/halibut-model-", hal_model, "-tmb.rds"))
  m_hal_stan <- readRDS(f)
  # temporary fixed for working in dev branch with models from main
  # m_hal_fixed$tmb_data$simulate_t <- rep(1L, length(unique(m_hal_fixed$data$year)))
}

f2 <- paste0("models/yelloweye-model-", ye_model, "-stan.rds")
if (file.exists(f2)) {
  m_ye_fixed <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))
  m_ye_stan <- readRDS(f2)
  # temporary fixed for working in dev branch with models from main
  # m_ye_fixed$tmb_data$simulate_t <- rep(1L, length(unique(m_ye_fixed$data$year)))
}


f <- paste0("data-generated/halibut-", hal_model, "_", grid_scale, "-index-all-stan.rds")
if (!file.exists(f)) {
  i_hal <- get_all_sims(m_hal_fixed, newdata = full_s_grid, tmbstan_model = m_hal_stan)
  i_hal <- setNames(i_hal, c("all", paste(unique(full_s_grid$region))))
  saveRDS(i_hal, f)
} else{
  i_hal <- readRDS(f)
}

f <- paste0("data-generated/yelloweye-", ye_model, "_", grid_scale, "-index-all-stan.rds")
if (!file.exists(f)) {
  i_ye <- get_all_sims(m_ye_fixed, newdata = full_s_grid, tmbstan_model = m_ye_stan)
  i_ye <- setNames(i_ye, c("all", paste(unique(full_s_grid$region))))
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


# spatial predictions for whole grid
# can calculate individually to check get_all_sims results were up to date.
# f <- paste0("data-generated/halibut-", hal_model, "_", grid_scale, "-predictions-all-S.rds")
# if (!file.exists(f)) {
#   p_hal_sims <- predict(m_hal_fixed, newdata = full_s_grid, tmbstan_model = m_hal_stan)
#   saveRDS(p_hal_sims, f)
# } else{
#   p_hal_sims <- readRDS(f)
# }
# f <- paste0("data-generated/yelloweye-", ye_model, "_", grid_scale, "-predictions-all-S.rds")
# if (!file.exists(f)) {
#   p_ye_sims <- predict(m_ye_fixed, newdata = full_s_grid, tmbstan_model = m_ye_stan)
#   saveRDS(p_ye_sims, f)
# } else{
#   p_ye_sims <- readRDS(f)
# }
#
# p_hal <- full_s_grid
# p_hal$est <- apply(p_hal_sims, 1, function(x) {median(x)})


p_hal <- i_hal$all[[3]]
p_hal_sims <- i_hal$all[[4]]
p_hal$est <- apply(p_hal_sims, 1, function(x) {median(x)})
p_hal$est_sd <- apply(p_hal_sims, 1, function(x) {sd(x)})

p_ye <- i_ye$all[[3]]
p_ye_sims <- i_ye$all[[4]]
p_ye$est <- apply(p_ye_sims, 1, function(x) {median(x)})
p_ye$est_sd <- apply(p_ye_sims, 1, function(x) {sd(x)})


# halibute density maps

d_hal_plots <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < max_map_lat) %>%
  filter(year %in% c(2018,2019,2020))
  # filter(year < 2016)
p_hal2020 <- p_hal %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)
# filter(year == 2016)

g <- map_predictions(
  pred_data = p_hal2020,
  pred_min = 0, #
  # pred_min = min(p_hal2020$est, na.rm = T),
  pred_max = quantile(p_hal2020$est, 0.995, na.rm = T),
  # pred_max = max(p_hal2020$est, na.rm = T),
  obs_data = d_hal_plots,
  obs_col = obs_cols,
  fill_lab = "Predicted kg/ha",
  map_lat_limits = c(min_map_lat , max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Landable kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/halibut-", hal_model, "_", grid_scale, "-map-2020.png"), #width = 6, height = 5,
       dpi = 400)

p_hal2020c <- p_hal2020 %>%
  filter(latitude < max_map_lat2)

d_hal2 <- d_hal_plots %>%
  filter(latitude < max_map_lat2)

g <- map_predictions(
  pred_data = p_hal2020c,
  pred_min = 0, #min(p_hal2020c$est, na.rm = T),
  pred_max = quantile(p_hal2020c$est, 0.995, na.rm = T),
  obs_data = d_hal2,
  fill_lab = "Predicted\nkg/ha",
  max_size_obs = 6,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat2, max_map_lat2),
  map_lon_limits = c(min_map_lon2, max_map_lon2),
  size_lab = "Landable\nkg/ha", #
  size_aes = density
) + theme(legend.box = 'horizontal') +
  guides(colour = "none")
g
ggsave(paste0("figs/halibut-", hal_model, "_", grid_scale, "-map-2020-closeup.png"), width = 6, height = 5, dpi = 400)

d_hal_plots2 <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < max_map_lat)

p_hal_mean <- p_hal %>%
  filter(latitude < max_map_lat) %>%
  group_by(x,y,X,Y,longitude,latitude) %>%
  summarise(est = mean(est))

g <- map_predictions(
  pred_data = p_hal_mean,
  fill_aes = est,
  pred_min = 0,
  pred_max = quantile(p_hal_mean$est, 0.995, na.rm = T),
  fill_lab = "Predicted kg/ha", #\n
  obs_data = d_hal_plots2,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Observed kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-all.png"), #width = 6, height = 5,
       dpi = 400)





# yelloweye density maps

d_ye_plots <- readRDS(("data-generated/yelloweye-model-data-hbll-weights.rds")) %>%
  filter(latitude < 52.15507)  %>% filter(year %in% c(#2017,
    2018, 2019, 2020))

p_ye2020 <- p_ye %>%
  filter(latitude < max_map_lat) %>%
  filter(year == 2020)

g <- map_predictions(
  pred_data = p_ye,
  fill_aes = est,
  pred_min = 0,
  pred_max = quantile(p_ye$est, 0.995, na.rm = T),
  fill_lab = "Predicted kg/ha", #\n
  obs_data = d_ye_plots,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Observed kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-2020.png"), #width = 6, height = 5,
       dpi = 400)

d_ye_plots2 <- readRDS(("data-generated/yelloweye-model-data-hbll-weights.rds")) %>%
  filter(latitude < 52.15507)
p_ye_mean <- p_ye %>%
  filter(latitude < max_map_lat) %>%
  group_by(x,y,X,Y,longitude,latitude) %>%
  summarise(est = mean(est))

g <- map_predictions(
  pred_data = p_ye_mean,
  fill_aes = est,
  pred_min = 0,
  pred_max = quantile(p_ye_mean$est, 0.995, na.rm = T),
  fill_lab = "Predicted kg/ha", #\n
  obs_data = d_ye_plots2,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  size_lab = "Observed kg/ha",
  size_aes = density
) + theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-all.png"), #width = 6, height = 5,
       dpi = 400)



p_ye2020c <- p_ye2020 %>%
filter(latitude < max_map_lat2)

d_ye2 <- d_ye_plots %>%
  filter(latitude < max_map_lat2)

g <- map_predictions(
  pred_data = p_ye2020c,
  pred_min = 0, #min(p_ye2020c$est, na.rm = T),
  pred_max = quantile(p_ye2020c$est, 0.995, na.rm = T),
  # pred_max = max(p_ye2020c$est),
  obs_data = d_ye2,
  fill_lab = "Predicted\nkg/ha",
  max_size_obs = 6,
  obs_col = obs_cols,
  map_lat_limits = c(min_map_lat2, max_map_lat2),
  map_lon_limits = c(min_map_lon2, max_map_lon2),
  size_lab = "Landable\nkg/ha", #
  size_aes = density
) + theme(legend.box = 'horizontal') +
  guides(colour = "none")
g
ggsave(paste0("figs/yelloweye-", ye_model, "_", grid_scale, "-map-2020-closeup.png"), width = 6, height = 5, dpi = 400)


# map ratios

pyd <- p_ye %>% rename(ye_est = est) %>% distinct()
phd <- p_hal %>%
  rename(hal_est = est) %>%
  dplyr::select(hal_est, X, Y, year)%>% distinct()

ratio_df <- left_join(pyd, phd) %>% mutate(
  halibut = hal_est,
  halibut2 = ifelse(hal_est < 0.01, 0.01, hal_est),
  yelloweye = ye_est,
  yelloweye2 = ifelse(ye_est < 0.01, 0.01, ye_est), # make min for yelloweye of 1 per km2
  ye_per_hal = (yelloweye*100) / (halibut*100),
  ye_per_hal2 = (yelloweye + 0.001) / (halibut+ 0.001),# using halibut 2 doesn't change anything.
  hal_per_ye = (halibut*100) / (yelloweye2*100),
  hal_per_ye2 = (halibut+ 0.001)/(yelloweye + 0.001)
)

ratio_df_2020 <- filter(ratio_df, year == 2020)

g1 <- map_predictions(
  map_lat_limits = c(48.4, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon),
  pred_data = ratio_df_2020,
  pred_min = 0,
  # pred_min = min(ratio_df_2020$ye_per_hal),
  pred_max = quantile(ratio_df_2020$ye_per_hal, 0.99),
  fill_aes = ye_per_hal,
  fill_lab = "Yelloweye to Halibut\nbiomass ratio"
)
g1

ggsave(paste0("figs/ye-to-halibut-", hal_model, "-2020.png"), width = 6, height = 5, dpi = 400)
#        width = 5.5, height = 4.5, dpi = 400
#        # width = 6, height = 5, dpi = 200


ggplot(ratio_df_2020) + geom_violin(aes(region,log10(ye_per_hal2)))

ratio_df_2020 %>%
  filter(region %in% c("CDA", "CDA adjacent"#, "non-CDA 3CD"
                                       )) %>%
  ggplot(aes(depth,log10(ye_per_hal2), colour = region)) +
  geom_point( alpha = 0.2)+
  geom_smooth() +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

ratio_df_2020 %>%
  filter(region %in% c("CDA", "CDA adjacent"#, "non-CDA 3CD"
  )) %>%
  ggplot(aes(depth,log10(hal_per_ye2), colour = region)) +
  geom_point( alpha = 0.2)+
  geom_smooth() +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)


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
  pred_max = quantile(c(cv_hal2020$cv,cv_ye2020$cv), 0.995),
  map_lat_limits = c(min_map_lat3, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon)
)+theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/halibut-", hal_model, "_", grid_scale, "-CV.png"), width = 6, height = 5, dpi = 400)


g <- map_predictions(
  pred_data = cv_ye2020,
  fill_aes = cv,
  fill_lab = "CV",
  pred_min = min(cv_hal2020$cv,cv_ye2020$cv),
  pred_max = quantile(c(cv_hal2020$cv,cv_ye2020$cv), 0.995),
  map_lat_limits = c(min_map_lat3, max_map_lat),
  map_lon_limits = c(min_map_lon, max_map_lon)
)+theme(legend.spacing.y = unit(0.1, "cm"))
g

ggsave(paste0("figs/ye-", ye_model, "_", grid_scale, "-CV.png"), width = 6, height = 5, dpi = 400)


# explore importance of depth

cda_grid <- filter(full_s_grid, region == "CDA")
hist(cda_grid$depth)
sum(cda_grid$depth*cda_grid$area)/sum(cda_grid$area)

ext_grid <- filter(full_s_grid, region == "CDA adjacent")
hist(ext_grid$depth)
sum(ext_grid$depth*ext_grid$area)/sum(ext_grid$area)

nonCDA_grid <- filter(full_s_grid, region == "non-CDA 3CD")
hist(nonCDA_grid$depth)
sum(nonCDA_grid$depth*nonCDA_grid$area)/sum(nonCDA_grid$area)

# bin_width <- 50
# bin_width <- 25
bin_width <- 10

depth_bin_pred <- function(tmb_model, stan_model, grid, bin_width = 50) {

  bins <- tibble(
    min_depth = seq(0, 600 - bin_width, by = bin_width),
    max_depth = seq(bin_width, 600, by = bin_width)
  )

  i <- furrr::future_pmap_dfr(bins, function(min_depth, max_depth) {
  # i <- purrr::pmap_dfr(bins, function(min_depth, max_depth) {
    .grid <- filter(grid, depth > min_depth & depth <= max_depth)
    if(nrow(.grid)>1){
    .p <- predict(tmb_model, newdata = .grid, tmbstan_model = stan_model)
    .i <- get_index_sims(.p, area = .grid$area / 10000)
    .i <- .i %>% mutate(
      region = unique(.grid$region),
      min_depth = min_depth,
      max_depth = max_depth,
      depth_range = as.factor(paste0(min_depth, "-", max_depth)),
      area = sum(.grid$area / 10000),
      density = est / area,
      lwr_dens = lwr / area,
      upr_dens = upr / area
    )
    .i
    }
  })
  return(i)
}



f <- paste0("report-data/hal-", hal_model, "-depth_bins_in_ext_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_hal_x <- depth_bin_pred(m_hal_fixed, m_hal_stan, ext_grid, bin_width = bin_width)
  saveRDS(i_hal_x, f)
}else{ i_hal_x <- readRDS(f)}

f <- paste0("report-data/ye-", ye_model, "-depth_bins_in_ext_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_ye_x <- depth_bin_pred(m_ye_fixed, m_ye_stan, ext_grid, bin_width = bin_width)
  saveRDS(i_ye_x, f)
}else{ i_ye_x <- readRDS(f)}

f <- paste0("report-data/hal-", hal_model, "-depth_bins_in_cda_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_hal_cda <- depth_bin_pred(m_hal_fixed, m_hal_stan, cda_grid, bin_width = bin_width)
  saveRDS(i_hal_cda, f)
}else{ i_hal_cda <- readRDS(f) }

f <- paste0("report-data/ye-", ye_model, "-depth_bins_in_cda_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_ye_cda <- depth_bin_pred(m_ye_fixed, m_ye_stan, cda_grid, bin_width = bin_width)
  saveRDS(i_ye_cda, f)
}else{ i_ye_cda <- readRDS(f) }

f <- paste0("report-data/hal-", hal_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_hal_3cd <- depth_bin_pred(m_hal_fixed, m_hal_stan, nonCDA_grid, bin_width = bin_width)
  saveRDS(i_hal_3cd, f)
}else{ i_hal_3cd <- readRDS(f) }

f <- paste0("report-data/ye-", ye_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_ye_3cd <- depth_bin_pred(m_ye_fixed, m_ye_stan, nonCDA_grid, bin_width = bin_width)
  saveRDS(i_ye_3cd, f)
}else{ i_ye_3cd <- readRDS(f) }

# i_hal_cda <- readRDS(paste0("report-data/hal_depth_bins_in_cda.rds"))
# i_ye_cda <- readRDS(paste0("report-data/ye_depth_bins_in_cda.rds"))
# i_hal_3cd <- readRDS(paste0("report-data/hal_depth_bins_in_3cd.rds"))
# i_ye_3cd <- readRDS(paste0("report-data/ye_depth_bins_in_3cd.rds"))

i_hal_3cd <- i_hal_3cd %>% filter(area > 10000)
i_hal_x <- i_hal_x %>% filter(area > 10000)
i_hal_cda <- i_hal_cda %>% filter(area > 10000)

i_ye_3cd <- i_ye_3cd %>% filter(area > 10000)
i_ye_x <- i_ye_x %>% filter(area > 10000)
i_ye_cda <- i_ye_cda %>% filter(area > 10000)


i_hal_all <- bind_rows(i_hal_x, i_hal_cda, i_hal_3cd)
i_ye_all <- bind_rows(i_ye_x, i_ye_cda, i_ye_3cd)


# density by depth for both species
i_ye_all %>% filter(year == 2020) %>%
  ggplot( aes(min_depth, density, group = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
  geom_line(data = filter(i_hal_all, year == 2020), aes(colour = region), lty = "dashed") +
  geom_ribbon(data = filter(i_hal_all, year == 2020), aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1, lty = "dashed") +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)



i_ye_2020 <- i_ye_all %>% filter(year == 2020)
i_hal_2020 <- i_hal_all %>% filter(year == 2020)
i_ye_2020 %>%
  ggplot( aes(min_depth, density, group = region)) +
    geom_line(colour = "orange") +
    geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens), fill = "orange", alpha=0.1) +
    geom_line(data = i_hal_2020, colour = "darkgreen") +
    geom_ribbon(data = i_hal_2020, fill = "darkgreen", aes(ymin = lwr_dens, ymax = upr_dens), alpha=0.1) +
  facet_grid(rows = vars(region)) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

ggsave(paste0("figs/pred-densities-by-", bin_width, "m-bin-depth-", grid_scale, "-region.png"), width = 4, height = 5, dpi = 400)


i_ye_2020 %>%
  ggplot( aes(min_depth, density, group = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

i_hal_2020 %>%
  ggplot( aes(min_depth, density, group = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

# ratio of YE to halibut by depth
.i_hal_all <- i_hal_all %>% select(year, region, min_depth, density) %>% rename(halibut = density)
.i_ye_all <- i_ye_all %>% select(year, region, min_depth, density) %>% rename(ye = density)

ratio_all <- left_join(.i_hal_all, .i_ye_all) %>% mutate(ratio = ye/halibut)

ratio_all %>% filter(year == 2018) %>%
ggplot(aes(min_depth, ratio, colour = region)) + geom_line() + scale_colour_manual(values = cols)

# to get CI for above

# get_index_sims()
#
# i_hal$CDA[[4]]
# i_hal$CDA[[3]]$depth




# depth through time in CDA adjacent area
i_hal_x %>% mutate(depth_range = forcats::fct_reorder(depth_range, min_depth))%>%
  ggplot( aes(year, density, group = depth_range)) + geom_line(aes(colour = depth_range), size = 2, alpha = 0.5) +
  # geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill = depth_range), alpha=0.1) +
  geom_line(data = filter(i_hal_cda, min_depth <90 & min_depth >=80), aes(alpha = -min_depth), colour = "red", lty = "dashed", size = 1) +
  # geom_ribbon(data = i_hal_cda, aes(ymin = lwr_dens, ymax = upr_dens), fill="red", alpha=0.1) +
  scale_color_viridis_d(direction = -1) +  scale_fill_viridis_d(direction = -1) +
  scale_x_continuous(limits = c(2008,2020), n.breaks = 7) +
  ggtitle("Halibut density by depth bin in CDA adjacent waters", subtitle = "(in CDA halibut peak at 75-100m = red dashed line)")

ggsave(paste0("figs/halibut-densities-by-", bin_width, "m-bin-depth-", grid_scale, "-through-time.png"), width = 7, height = 5, dpi = 400)



# # no spatiotemporal random field so annual change less interesting
# i_ye_x %>%  mutate(depth_range = forcats::fct_reorder(depth_range, min_depth))%>%
#   ggplot( aes(year, density, group = depth_range)) + geom_line(aes(colour = depth_range), size = 2, alpha = 0.5) +
#   # geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill = depth_range), alpha=0.1) +
#   geom_line(data = filter(i_ye_cda, min_depth ==125), aes(alpha = min_depth), colour = "red", lty = "dashed", size = 1) +
#   # geom_ribbon(data = i_ye_cda, aes(ymin = lwr_dens, ymax = upr_dens), fill="red", alpha=0.1) +
#   scale_color_viridis_d(direction = -1) +  scale_fill_viridis_d(direction = -1) +
#   ggtitle("YE density by depth bin in CDA adjacent waters", subtitle = "(CDA = red)")
#
#




# For just southern outside HBLL
#
# # Just the HBLL S grid
# d_utm <- m_hal_fixed$data
# hbll_s_grid <- readRDS("data-generated/grids/full_hybrid_grid_w_substrate.rds") %>%
#   filter(survey == "HBLL")
# hbll_s_grid <- hbll_s_grid[complete.cases(hbll_s_grid), ]
# hbll_s_grid <- expand_prediction_grid(hbll_s_grid, years = years) %>%
#   mutate(depth_centred = log(depth) - mean(d_utm$depth_log)) %>%
#   mutate(depth_scaled = depth_centred / sd(d_utm$depth_centred))
#
#
#
# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-hbll-S-stan.rds"
# if (!file.exists(ss)) {
#
#   i_ye_S2b <- get_all_sims(m_ye, newdata = hbll_s_grid, split_by_region = F)
#   saveRDS(i_ye_S2b, ss)
# }

#
# # # Save the simulated indices for report
# #
# s1 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-all-S-stan.rds"
# if (!file.exists(s1)) {
#   i_hal_combined <- get_all_sims(fit_obj = m_hal_fixed, tmbstan_model = m_hal_stan,  newdata = full_s_grid)
#   i_hal_combined <- setNames(i_hal_combined, c("all", paste(unique(full_s_grid$region))))
#   saveRDS(i_hal_combined, s1)
# }
#
# s2 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-stan.rds"
# if (!file.exists(s2)) {
#   i_ye_combined <- get_all_sims(fit_obj = m_ye_fixed, tmbstan_model = m_ye_stan,
#                                 # area_divisor = 1000000,  # default is hectares, use this to get km2
#                                 newdata = full_s_grid)
#   i_ye_combined <- setNames(i_ye_combined, c("all", paste(unique(full_s_grid$region))))
#   saveRDS(i_ye_combined, s2)
# }
# #
# i_hal_cda <- mutate(i_hal_combined[["CDA"]]$index, area = sum(cda_grid$area/10000),
# density = est/area, lwr_dens = lwr/area, upr_dens = upr/area, depth_range = "0-150")
#
# i_ye_cda <- mutate(i_ye_combined[["CDA"]]$index, area = sum(cda_grid$area/10000),
#                    density = est/area, lwr_dens = lwr/area, upr_dens = upr/area, depth_range = "0-150")

# i_ye <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-stan.rds"))[["all"]][[1]]
#
#
# i_ye <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-stan.rds"))[[1]]
# saveRDS(i_ye, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-all-S-stan.rds"))
# i_ye_cda <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-cda-sim-500.rds"))[[1]]
# saveRDS(i_ye_cda, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-cda-sim-500.rds"))
# i_ye_noncda <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_ye_noncda, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))
# i_ye_noncdaN <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_ye_noncdaN, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))
# i_ye_noncdaS <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_ye_noncdaS, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))
#
#
# i_hal <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-all-S-stan.rds"))[[1]]
# saveRDS(i_hal, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-all-S-stan.rds"))
# i_hal_cda <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-cda-sim-500.rds"))[[1]]
# saveRDS(i_hal_cda, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-cda-sim-500.rds"))
# i_hal_noncda <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_hal_noncda, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))
# i_hal_noncdaN <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_hal_noncdaN, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))
# i_hal_noncdaS <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))[[1]]
# saveRDS(i_hal_noncdaS, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))
#
#

