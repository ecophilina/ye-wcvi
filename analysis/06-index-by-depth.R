# indices by depth

# library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())
options(sdmTMB.cores = 2L)


# load misc custom functions
# includes a set of map boundaries that could be adjusted
source("analysis/functions.R")

# select which scale of grid to use
# grid_scale <- 2000
grid_scale <- 1000

# latitude_cutoff <- 51

cols <- c(
  "red",
  #"darkorchid4",
  "deeppink4",
  "deepskyblue4"
  # "cadetblue3"
)

include_cc <- FALSE
# # OR experiment with including non-survey catches
include_cc <- TRUE

if (include_cc) {
  obs_cols <- c("white", "#98FB98", "#FFDAB9")
} else{
  obs_cols <- c("white", "#98FB98")
}

# load grid and add in fyear and dummy vessel id
full_s_grid <- readRDS(paste0("report-data/full_filled_grid_w_ext_", grid_scale,".rds")) %>%
  # filter(latitude <= latitude_cutoff) %>%
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
bin_width <- 20
# bin_width <- 10

depth_bin_pred <- function(tmb_model, stan_model, grid, bin_width = 50) {

  bins <- tibble(
    min_depth = seq(0, 600 - bin_width, by = bin_width),
    max_depth = seq(bin_width, 600, by = bin_width)
  )

  p <- predict(tmb_model, tmbstan_model = stan_model,
               # nsims = 500,
               # re_form_iid = NA,
               newdata = grid)

  i <- furrr::future_pmap_dfr(bins, function(min_depth, max_depth) {
    # i <- purrr::pmap_dfr(bins, function(min_depth, max_depth) {
    # .grid <- filter(grid, depth > min_depth & depth <= max_depth)

    to_use <- sample(seq_len(1000), 1000)
    .p <- p[grid$depth > min_depth & grid$depth <= max_depth, to_use]
    .grid <- grid[grid$depth > min_depth & grid$depth <= max_depth, to_use]
    attributes(.p) <- attributes(p)

    if(nrow(.grid)>50){
      # .p <- predict(tmb_model, tmbstan_model = stan_model,
      #               # re_form_iid = NA,
      #               newdata = .grid)
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

# f <- paste0("report-data/hal-", hal_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
# if(!file.exists(f)) {
#   i_hal_3cd <- depth_bin_pred(m_hal_fixed, m_hal_stan, nonCDA_grid, bin_width = bin_width)
#   saveRDS(i_hal_3cd, f)
# }else{ i_hal_3cd <- readRDS(f) }
#
# f <- paste0("report-data/ye-", ye_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
# if(!file.exists(f)) {
#   i_ye_3cd <- depth_bin_pred(m_ye_fixed, m_ye_stan, nonCDA_grid, bin_width = bin_width)
#   saveRDS(i_ye_3cd, f)
# }else{ i_ye_3cd <- readRDS(f) }
#

# i_hal_3cd <- i_hal_3cd %>% filter(area > 10000)
i_hal_x <- i_hal_x %>% filter(area > 10000)
i_hal_cda <- i_hal_cda %>% filter(area > 10000)

# i_ye_3cd <- i_ye_3cd %>% filter(area > 10000)
i_ye_x <- i_ye_x %>% filter(area > 10000)
i_ye_cda <- i_ye_cda %>% filter(area > 10000)


i_hal_all <- bind_rows(
  #i_hal_3cd,
  i_hal_x, i_hal_cda)
i_ye_all <- bind_rows(
  # i_ye_3cd,
  i_ye_x, i_ye_cda)


# density by depth for both species
i_ye_cda %>% filter(year == 2020) %>%
  ggplot( aes(min_depth, density, group = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
  geom_line(data = filter(i_hal_cda, year == 2020), colour = "blue", lty = "dashed") +
  geom_ribbon(data = filter(i_hal_cda, year == 2020), aes(ymin = lwr_dens, ymax = upr_dens),
              fill= "blue", alpha=0.1, lty = "dashed") +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols)



# density by depth for both species
i_ye_all %>% filter(year == 2020) %>%
  ggplot( aes(min_depth, density, group = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(ymin = lwr_dens, ymax = upr_dens, fill=region), alpha=0.1) +
  geom_line(data = filter(i_hal_all, year == 2020),
            aes(colour = region), lty = "dashed") +
  geom_ribbon(data = filter(i_hal_all, year == 2020),
              aes(ymin = lwr_dens, ymax = upr_dens, fill=region),
              alpha=0.1, lty = "dashed") +
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

ggsave(paste0("figs/pred-densities-by-", bin_width, "m-bin-depth-", grid_scale, "-region.png"), width = 4, height = 3, dpi = 400)


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

ggsave(paste0("figs/ratio-by-", bin_width, "m-bin-depth-", grid_scale, "-region.png"), width = 4, height = 2, dpi = 400)


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

