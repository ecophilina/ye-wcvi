# generate predictions

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())
# load misc custom functions
source("analysis/functions.R")


# load grid
full_s_grid <- readRDS("report-data/full_filled_grid_w_ext.rds")

# load models
model_name <- "model-rocky-muddy-300kn-delta-IID-aniso"

f <- paste0("models/halibut-", model_name, "-stan.rds")
if (file.exists(f)) {
  m_hal_fixed <- readRDS(paste0("models/halibut-", model_name, "-tmb.rds"))
  m_hal_stan <- readRDS(f)
}

model_name2 <- "model-rocky-muddy-300kn-delta-spatial-aniso"
f2 <- paste0("models/yelloweye-", model_name2, "-stan.rds")
if (file.exists(f2)) {
  m_ye_fixed <- readRDS(paste0("models/yelloweye-", model_name2, "-tmb.rds"))
  m_ye_stan <- readRDS(f2)
}

# spatial predictions for whole grid

p_hal <- predict(m_hal_fixed, newdata = full_s_grid, tmbstan_model = m_hal_stan)
saveRDS(p_hal, "data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-all-S.rds")

p_ye <- predict(m_ye_fixed, newdata = full_s_grid, tmbstan_model = m_ye_stan)
saveRDS(p_ye, "data-generated/filled-keepable-yelloweye-est-rock-mud-predictions-all-S.rds")

# i_hal <- get_index_sims(p_hal)
# i_ye <- get_index_sims(p_ye)
#
# ggplot(i_hal, aes(year, est)) + geom_line(colour = "darkgreen") +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkgreen", alpha = 0.2) +
#   geom_line(data = i_ye, colour = "orange") +
#   geom_ribbon(data = i_ye, aes(ymin = lwr, ymax = upr), fill = "orange", alpha = 0.2)

cda_grid <- filter(full_s_grid, region == "CDA")
hist(cda_grid$depth)
sum(cda_grid$depth*cda_grid$area)/sum(cda_grid$area)

ext_grid <- filter(full_s_grid, region == "CDA adjacent")
hist(ext_grid$depth)
sum(ext_grid$depth*ext_grid$area)/sum(ext_grid$area)

nonCDA_grid <- filter(full_s_grid, region == "non-CDA 3CD")
hist(nonCDA_grid$depth)
sum(nonCDA_grid$depth*nonCDA_grid$area)/sum(nonCDA_grid$area)

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

i_hal_x <- depth_bin_pred(m_hal_fixed, m_hal_stan, ext_grid)
saveRDS(i_hal_x, "report-data/hal_depth_bins_in_ext.rds")

i_ye_x <- depth_bin_pred(m_ye_fixed, m_ye_stan, ext_grid)
saveRDS(i_ye_x, "report-data/ye_depth_bins_in_ext.rds")

i_hal_cda <- depth_bin_pred(m_hal_fixed, m_hal_stan, cda_grid)
saveRDS(i_hal_cda, "report-data/hal_depth_bins_in_cda.rds")

i_ye_cda <- depth_bin_pred(m_ye_fixed, m_ye_stan, cda_grid)
saveRDS(i_ye_cda, "report-data/ye_depth_bins_in_cda.rds")

i_hal_3cd <- depth_bin_pred(m_hal_fixed, m_hal_stan, nonCDA_grid)
saveRDS(i_hal_3cd, "report-data/hal_depth_bins_in_3cd.rds")

i_ye_3cd <- depth_bin_pred(m_ye_fixed, m_ye_stan, nonCDA_grid)
saveRDS(i_ye_3cd, "report-data/ye_depth_bins_in_3cd.rds")

i_hal_x <- readRDS("report-data/hal_depth_bins_in_ext.rds")
i_ye_x <- readRDS("report-data/ye_depth_bins_in_ext.rds")
i_hal_cda <- readRDS("report-data/hal_depth_bins_in_cda.rds")
i_ye_cda <- readRDS("report-data/ye_depth_bins_in_cda.rds")
i_hal_3cd <- readRDS("report-data/hal_depth_bins_in_3cd.rds")
i_ye_3cd <- readRDS("report-data/ye_depth_bins_in_3cd.rds")

i_hal_3cd <- i_hal_3cd %>% filter(area > 10000)
i_hal_x <- i_hal_x %>% filter(area > 10000)
i_hal_cda <- i_hal_cda %>% filter(area > 10000)

i_ye_3cd <- i_ye_3cd %>% filter(area > 10000)
i_ye_x <- i_ye_x %>% filter(area > 10000)
i_ye_cda <- i_ye_cda %>% filter(area > 10000)


i_hal_all <- bind_rows(i_hal_x, i_hal_cda, i_hal_3cd)
i_ye_all <- bind_rows(i_ye_x, i_ye_cda, i_ye_3cd)

cols <- c("red", "darkorchid4", "deepskyblue4")

# density by depth for both species
i_ye_all %>% filter(year == 2020) %>%
  ggplot( aes(min_depth, density, group = region)) + geom_line(aes(colour = region)) +
    geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
    geom_line(data = filter(i_hal_all, year == 2020), aes(colour = region), lty = "dashed") +
    geom_ribbon(data = filter(i_hal_all, year == 2020), aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1, lty = "dashed") +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols)

# ratio of YE to halibut by depth
i_hal_20 <- bind_rows(i_hal_x, i_hal_cda, i_hal_3cd) %>% select(year, region, min_depth, density) %>% rename(halibut = density) %>% filter(year == 2020)
i_ye_20 <- bind_rows(i_ye_x, i_ye_cda, i_ye_3cd) %>% select(year, region, min_depth, density) %>% rename(ye = density) %>% filter(year == 2020)

ratio_20 <- left_join(i_hal_20, i_ye_20) %>% mutate(ratio = ye/halibut)

ggplot(ratio_20, aes(min_depth, ratio, colour = region)) + geom_line() + scale_colour_manual(values = cols)

# will need to redo with  return_sims = TRUE to get CI for above


# depth through time in CDA adjacent area
i_hal_x %>% filter(area > 10000) %>% mutate(depth_range = forcats::fct_reorder(depth_range, min_depth))%>%
  ggplot( aes(year, density, group = as.factor(area))) + geom_line(aes(colour = depth_range)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill = depth_range), alpha=0.1) +
  geom_line(data = i_hal_cda, colour = "red", lty = "dashed") +
  geom_ribbon(data = i_hal_cda, aes(ymin = lwr_dens, ymax = upr_dens), fill="red", alpha=0.1) +
  scale_color_viridis_d(direction = -1) +  scale_fill_viridis_d(direction = -1) +
  ggtitle("Halibut density by depth bin in CDA adjacent waters", subtitle = "(CDA = red)")

i_ye_x %>% filter(area > 10000) %>% mutate(depth_range = forcats::fct_reorder(depth_range, min_depth))%>%
  ggplot( aes(year, density, group = as.factor(area))) + geom_line(aes(colour = depth_range)) +
  # geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill = depth_range),alpha=0.1) +
  geom_line(data = i_ye_cda, colour = "red", lty = "dashed") +
  geom_ribbon(data = i_ye_cda, aes(ymin = lwr_dens, ymax = upr_dens), fill="red", alpha=0.1) +
  scale_color_viridis_d(direction = -1) +  scale_fill_viridis_d(direction = -1) +
  ggtitle("YE density by depth bin in CDA adjacent waters", subtitle = "(CDA = red)")






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

