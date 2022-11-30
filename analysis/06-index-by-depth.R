# indices by depth

# library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
library(patchwork)

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

# depth stats for regions

full_s_grid %>% filter(year == max(full_s_grid$year)) %>%
  group_by(region) %>%
  summarise(mean = mean(depth, na.rm = TRUE),
            median = median(depth, na.rm = TRUE),
            min = min(depth, na.rm = TRUE),
            max = max(depth, na.rm = TRUE),
            quantile_25th = quantile(depth, 0.25),
            quantile_75th = quantile(depth, 0.75))


# # load models if 03 not just run
# hal_model <- "w-effort-500kn-delta-AR1-aniso"
# ye_model <- "w-effort-500kn-delta-spatial-aniso"
hal_model <- "w-deeper-500kn-delta-AR1-aniso"
ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso"

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
bin_width <- 25
# bin_width <- 10


depth_bin_pred <- function(tmb_model, stan_model, grid, bin_width = 50, nsims = 1000) {

  bins <- tibble(
    min_depth = seq(0, 600 - bin_width, by = bin_width),
    max_depth = seq(bin_width, 600, by = bin_width)
  )

  p <- predict(tmb_model, tmbstan_model = stan_model, newdata = grid, nsim = nsims)
  to_use <- sample(seq_len(nsims), nsims) # here in case we need to subsample these

  i <- furrr::future_pmap_dfr(bins, function(min_depth, max_depth) {
    # i <- purrr::pmap_dfr(bins, function(min_depth, max_depth) {
    # .grid <- filter(grid, depth > min_depth & depth <= max_depth)

    .p <- p[grid$depth > min_depth & grid$depth <= max_depth, to_use]
    .grid <- grid[grid$depth > min_depth & grid$depth <= max_depth, ]

    if(nrow(.grid)>50){
      # browser()
      attributes(.p)["time"] <- attributes(p)["time"]
      attributes(.p)["link"] <- attributes(p)["link"]
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

f <- paste0("report-data/hal-", hal_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_hal_3cd <- depth_bin_pred(m_hal_fixed, m_hal_stan, nonCDA_grid,
                              nsims = 500,
                              bin_width = bin_width)
  saveRDS(i_hal_3cd, f)
}else{ i_hal_3cd <- readRDS(f) }

f <- paste0("report-data/ye-", ye_model, "-depth_bins_in_3cd_", bin_width, "_", grid_scale, ".rds")
if(!file.exists(f)) {
  i_ye_3cd <- depth_bin_pred(m_ye_fixed, m_ye_stan, nonCDA_grid,
                             nsims = 500,
                             bin_width = bin_width)
  saveRDS(i_ye_3cd, f)
}else{ i_ye_3cd <- readRDS(f) }


i_hal_3cd <- i_hal_3cd %>% filter(area > 10000)
i_hal_x <- i_hal_x %>% filter(area > 10000)
i_hal_cda <- i_hal_cda %>% filter(area > 10000)

i_ye_3cd <- i_ye_3cd %>% filter(area > 10000)
i_ye_x <- i_ye_x %>% filter(area > 10000)
i_ye_cda <- i_ye_cda %>% filter(area > 10000)


i_hal_all <- bind_rows(
  i_hal_3cd,
  i_hal_x, i_hal_cda)
i_ye_all <- bind_rows(
  i_ye_3cd,
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


i_ye_2020 <- i_ye_all %>%
  filter(region != "non-CDA 3CD") %>%
  filter(year == 2020)
i_hal_2020 <- i_hal_all %>%
  filter(region != "non-CDA 3CD") %>%
  filter(year == 2020)
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
r <- paste0("data-generated/ratios-by-depth-", hal_model,".rds")

if (!file.exists(r)) {

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



depth_bin_ratios <- function(pred_species1 = p_hal, pred_species2 = p_ye, grid = grid, bin_width = 50) {

  bins <- tibble(
    min_depth = seq(0, 600 - bin_width, by = bin_width),
    max_depth = seq(bin_width, 600, by = bin_width)
  )

  p1 <- pred_species1
  p2 <- pred_species2

  r <- purrr::pmap_dfr(bins, function(min_depth, max_depth) {
    .grid <- grid[grid$depth > min_depth & grid$depth <= max_depth, ]

    if(nrow(.grid)>50){
      # browser()
      .p1 <- p1[grid$depth > min_depth & grid$depth <= max_depth, ]
      .p2 <- p2[grid$depth > min_depth & grid$depth <= max_depth, ]

      attributes(.p1)["time"] <- attributes(p1)["time"]
      attributes(.p1)["link"] <- "response"
      # attributes(.p1)["link"] <- attributes(p1)["link"]

      attributes(.p2)["time"] <- attributes(p2)["time"]
      attributes(.p2)["link"] <- "response"
      # attributes(.p2)["link"] <- attributes(p2)["link"]

      .i1 <- get_index_sims(.p1, return_sims = T, area = .grid$area / 10000,
                            area_function = function(x, area) x * area,
                            agg_function = function(x) sum((x))) %>%
        rename(species1 = .value)

      .i2 <- get_index_sims(.p2, return_sims = T, area = .grid$area / 10000,
                            area_function = function(x, area) x * area,
                            agg_function = function(x) sum((x))) %>%
        rename(species2 = .value)

      .i <- left_join(.i1, .i2) %>% mutate(
        sp1_per_sp2 = species1 / species2,
        sp2_per_sp1 = species2 / species1
      ) %>%
        # filter(.iteration <= 500) %>%
        group_by(year) %>%
        summarise(
          lwr12 = quantile(sp1_per_sp2, 0.025),
          upr12 = quantile(sp1_per_sp2, 0.975),
          # has to be after as this overwrites the iteration values
          sp1_per_sp2 = mean(sp1_per_sp2),
          lwr21 = quantile(sp2_per_sp1, 0.025),
          upr21 = quantile(sp2_per_sp1, 0.975),
          sp2_per_sp1 = mean(sp2_per_sp1)
        )

      .i$region = unique(.grid$region)
      .i$min_depth = min_depth
      .i$max_depth = max_depth
      .i$depth_range = as.factor(paste0(min_depth, "-", max_depth))
      .i
    }
  })
  return(r)
}



list_regions <- c("CDA", "CDA adjacent", "non-CDA 3CD")

ratios <-list()

for (i in list_regions) {
  # browser()
p_hal <- i_hal[[i]]$sim.predictions
p_ye <- i_ye[[i]]$sim.predictions
grid <- i_ye$all$grid[i_ye$all$grid$region == i, ]

ratios[[i]] <- depth_bin_ratios(grid = grid, bin_width = bin_width)
}

ratios_df <- do.call("rbind", ratios)

saveRDS(ratios_df, paste0("data-generated/ratios-by-depth-", hal_model,".rds"))
}

ratios_df <- readRDS(paste0("data-generated/ratios-by-depth-", hal_model,".rds"))


(dr1 <- ratios_df %>% filter(year == 2020) %>%
  # filter(min_depth > 50) %>%
  ggplot(aes(min_depth + 25/2, sp1_per_sp2, fill = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(min_depth+ 25/2, ymin = lwr12, ymax = upr12), alpha = 0.1) +
  scale_y_log10() +
  geom_vline(xintercept = 175, lty = "dashed") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
    labs(x = paste0("Depth (m) of grid cells grouped in ", bin_width, " m bins"),
         y = "Ratios of halibut to yellow catch weights",
         colour = "Region", fill = "Region") +
    theme(legend.position = c(0.45,0.85))
  )

ggsave(paste0("figs/ratio-hal-to-YE-by-", bin_width, "m-bin-depth-", hal_model, "-region.png"), width = 7, height = 4, dpi = 400)

(dr2 <- ratios_df %>% filter(year == 2020) %>%
  # filter(min_depth > 50) %>%
  ggplot(aes(min_depth+ 25/2, sp2_per_sp1, fill = region)) +
  geom_line(aes(colour = region)) +
  geom_ribbon(aes(min_depth+ 25/2, ymin = lwr21, ymax = upr21), alpha = 0.1) +
  scale_y_log10() +
  geom_vline(xintercept = 175, lty = "dashed") +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  labs(x = paste0("Depth (m) of grid cells grouped in ", bin_width, " m bins"),
       y = "Ratios of yelloweye to halibut catch weights",
       colour = "Region", fill = "Region") +
  theme(legend.position = c(0.45,0.25))
  )
ggsave(paste0("figs/ratio-YE-to-hal-by-", bin_width, "m-bin-depth-", hal_model, "-region.png"),
       width = 7, height = 4, dpi = 400)


# add depth histogram inset
p2 <- full_s_grid %>%
  filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD") & year == max(full_s_grid$year)) %>%
  ggplot() + geom_histogram(aes(depth, fill = region),
                            colour = NA,
                            alpha = 0.7,
                            binwidth = 25, boundary = 0) +
  # ggplot() + geom_density(aes(depth, colour = region, fill = region), alpha = 0.25) +
  geom_vline(xintercept = 175, lty = "dashed") +
  scale_fill_manual(values = cols, name = "Region") +
  scale_colour_manual(values = cols, name = "Region") +
  ylab("Grid cells") +
  xlab("") +
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(n.breaks = 4) +
  coord_cartesian(expand = FALSE) +
  # scale_x_sqrt() +
  # facet_wrap(~region, ncol = 1) +
  ggsidekick::theme_sleek() +
  theme(legend.position = "none",
        text = element_text(size = 8),
        axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

(dr2 <- ratios_df %>% filter(year == 2020) %>%
    # filter(min_depth > 50) %>%
    ggplot(aes(min_depth+ 25/2, sp2_per_sp1, fill = region)) +
    # geom_plot(data = data.tb, aes(x, y, label = plot)) +
    geom_line(aes(colour = region)) +
    geom_ribbon(aes(min_depth+ 25/2, ymin = lwr21, ymax = upr21), alpha = 0.1) +
    scale_y_log10() +
    scale_x_continuous(n.breaks = 5, expand = c(0,0)) +
    geom_vline(xintercept = 175, lty = "dashed") +
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols) +
    labs(x = paste0("Depth (m) of grid cells grouped in ", bin_width, " m bins"),
         y = "Ratios of yelloweye to halibut catch weights",
         colour = "Region", fill = "Region") +
    theme(legend.position = c(0.6,0.23))+
    inset_element(p2,
                  left = 0.01, bottom = 0.00001,
                  right = 0.5, top = 0.4)
)


ggsave(paste0("figs/ratio-YE-to-hal-by-", bin_width, "m-bin-depth-", hal_model, "-region-w-inset.png"),
       width = 7, height = 4, dpi = 400)




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

