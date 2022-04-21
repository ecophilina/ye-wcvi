# generate simulated predictions using MVN approximation

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)

# load misc custom functions
source("analysis/functions.R")





# Catch predictions and indices
## Pacific Halibut
# using filled in grid (includes partial cells but not sure how to account for cell area index code) and HBLL gear type
#
# For entire filled in southern outside HBLL grid area including CDA, but excluding RCAs


f <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-all-S.rds"
if (!file.exists(f)) {
  p_hal_S_bin <- predict(m_halibut_bin, newdata = full_s_grid, return_tmb_object = TRUE)

  p_hal_S_pos <- predict(m_halibut_pos, newdata = full_s_grid, return_tmb_object = TRUE)

  p_hal_S <-  p_hal_S_bin
  p_hal_S$data$est_bin <- p_hal_S$data$est
  p_hal_S$data$est_pos <- p_hal_S_pos$data$est
  p_hal_S$data$est <- log(plogis(p_hal_S$data$est_bin) * exp(p_hal_S$data$est_pos))
  saveRDS(p_hal_S, f)
}


# Non-CDA 3CD5A


ss <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"

if (!file.exists(ss)) {
  i_hal_noncda <- get_all_sims(fit_obj_bin = m_halibut_bin, fit_obj_pos = m_halibut_pos,
                               newdata = noncda_grid, split_by_region = F)
  saveRDS(i_hal_noncda, ss)
}




# For subregions using same batch of simulations

## this takes up a lot of space and duplicates the components saved below...
# ss <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-list-sim-500.rds"
# if (!file.exists(ss)) {
#   i_hal_combined <- get_all_sims(m_halibut, newdata = full_s_grid)
#   saveRDS(i_hal_combined, ss)
# }

s1 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-all-S-sim-500.rds"
if (!file.exists(s1)) {
  i_hal_combined <- get_all_sims(fit_obj_bin = m_halibut_bin, fit_obj_pos = m_halibut_pos,  newdata = full_s_grid)
  i_hal_S <- i_hal_combined[[1]]
  saveRDS(i_hal_S, s1)
}

s3 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-cda-sim-500.rds"
if (!file.exists(s3)) {
  i_hal_cda2 <- i_hal_combined[[3]]
  saveRDS(i_hal_cda2, s3)
}

s4 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"
if (!file.exists(s4)) {
  i_hal_noncda2N <- i_hal_combined[[4]]
  saveRDS(i_hal_noncda2N, s4)
}

s2 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"
if (!file.exists(s2)) {
  i_hal_noncda2S <- i_hal_combined[[2]]
  saveRDS(i_hal_noncda2S, s2)
}

s5 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-other-sim-500.rds"
if (!file.exists(s5)) {
  i_hal_other <- i_hal_combined[[5]]
  saveRDS(i_hal_other, s5)
}



# For management regions using same batch of simulations

## this takes up a lot of space and duplicates the components saved below...
# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-list-sim-500.rds"
# if (!file.exists(ss)) {
#   i_ye_combined <- get_all_sims(m_ye, newdata = full_s_grid)
#   saveRDS(i_ye_combined, ss)
# }

s1 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-all-regions-sim-500.rds"
if (!file.exists(s1)) {
  i_hal_combined <- get_all_sims(fit_obj_bin = m_halibut_bin, fit_obj_pos = m_halibut_pos, newdata = regions_grid)
  i_hal_1 <- i_hal_combined[[1]]
  saveRDS(i_hal_1, s1)
}

s2 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-3C-sim-500.rds"
if (!file.exists(s2)) {
  i_hal_2 <- i_hal_combined[[2]]
  saveRDS(i_hal_2, s2)
}

s3 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-3D-sim-500.rds"
if (!file.exists(s3)) {
  i_hal_3 <- i_hal_combined[[3]]
  saveRDS(i_hal_3, s3)
}

s4 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A-sim-500.rds"
if (!file.exists(s4)) {
  i_hal_4 <- i_hal_combined[[4]]
  saveRDS(i_hal_4, s4)
}

s5 <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5B-sim-500.rds"
if (!file.exists(s5)) {
  i_hal_5 <- i_hal_combined[[5]]
  saveRDS(i_hal_5, s5)
}


# For just southern outside HBLL

# hbll_s_grid <- readRDS(here::here("grids/hbll_s_grid.rds"))
f <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-hbll-S.rds"
if (!file.exists(f)) {

  p_hal_S_bin <- predict(m_halibut_bin, newdata = hbll_s_grid, return_tmb_object = TRUE)
  p_hal_S_pos <- predict(m_halibut_pos, newdata = hbll_s_grid, return_tmb_object = TRUE)

  p_hal_S <-  p_hal_S_bin
  p_hal_S$data$est_bin <- p_hal_S$data$est
  p_hal_S$data$est_pos <- p_hal_S_pos$data$est
  p_hal_S$data$est <- log(plogis(p_hal_S$data$est_bin) * exp(p_hal_S$data$est_pos))
  saveRDS(p_hal_S, f)
}

ss <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-index-hbll-S-sim-500.rds"
if (!file.exists(ss)) {
  i_hal_S2b <- get_all_sims(fit_obj_bin = m_halibut_bin, fit_obj_pos = m_halibut_pos,
                            newdata = hbll_s_grid, split_by_region = F)
  saveRDS(i_hal_S2b, ss)
}



## Yelloweye Rockfish

# For entire filled in southern outside HBLL grid including CDA, but excluding RCAs.
# This is roughly equivilant to entire southern outside yelloweye stock.

f <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S.rds"
if (!file.exists(f)) {
  p_ye_S <- predict(m_ye, newdata = full_s_grid, return_tmb_object = TRUE)
  saveRDS(p_ye_S, "data-generated/filled-keepable-yelloweye-est-rock-mud-predictions-all-S.rds")

  i_ye_S <- get_index(p_ye_S, bias_correct = F)
  saveRDS(i_ye_S, f)
}

# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-sim-500.rds"
# if (!file.exists(ss)) {
#   i_ye_S2 <- get_all_sims(m_ye, newdata = full_s_grid)
#   saveRDS(i_ye_S2, ss)
# }


# For 5A3CD outside CDA

s0 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"

if (!file.exists(s0)) {
  i_ye_noncda <- get_all_sims(m_ye, newdata = noncda_grid, split_by_region = F)
  saveRDS(i_ye_noncda, s0)
}

# f <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda.rds"
# if (!file.exists(f)) {
#   p_ye_noncda <- predict(m_ye, newdata = noncda_grid, return_tmb_object = TRUE)
#   saveRDS(p_ye_noncda, "data-generated/filled-keepable-yelloweye-est-rock-mud-predictions-5A3CD-outside-cda.rds")
#
#   i_ye_noncda <- get_index(p_ye_noncda, bias_correct = F)
#   saveRDS(i_ye_noncda, f)
# }
#
# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"
# if (!file.exists(ss)) {
#   i_ye_noncda2 <- get_all_sims(m_ye, newdata = noncda_grid, split_by_region = F)
#   saveRDS(i_ye_noncda2, ss)
# }



# For subregions using same batch of simulations

## this takes up a lot of space and duplicates the components saved below...
# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-list-sim-500.rds"
# if (!file.exists(ss)) {
#   i_ye_combined <- get_all_sims(m_ye, newdata = full_s_grid)
#   saveRDS(i_ye_combined, ss)
# }

s1 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-sim-500.rds"
if (!file.exists(s1)) {
  i_ye_combined <- get_all_sims(m_ye, newdata = full_s_grid)
  i_ye_S <- i_ye_combined[[1]]
  saveRDS(i_ye_S, s1)
}

s3 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-cda-sim-500.rds"
if (!file.exists(s3)) {
  i_ye_cda2 <- i_ye_combined[[3]]
  saveRDS(i_ye_cda2, s3)
}

s4 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"
if (!file.exists(s4)) {
  i_ye_noncda2N <- i_ye_combined[[4]]
  saveRDS(i_ye_noncda2N, s4)
}

s2 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"
if (!file.exists(s2)) {
  i_ye_noncda2S <- i_ye_combined[[2]]
  saveRDS(i_ye_noncda2S, s2)
}

s5 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-other-outside-cda-sim-500.rds"
if (!file.exists(s5)) {
  i_ye_other <- i_ye_combined[[5]]
  saveRDS(i_ye_other, s5)
}


# For management regions using same batch of simulations
## this takes up a lot of space and duplicates the components saved below...
# ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-list-sim-500.rds"
# if (!file.exists(ss)) {
#   i_ye_combined <- get_all_sims(m_ye, newdata = full_s_grid)
#   saveRDS(i_ye_combined, ss)
# }

s1 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-regions-sim-500.rds"
if (!file.exists(s1)) {
  i_ye_combined <- get_all_sims(m_ye, newdata = regions_grid)
  saveRDS(i_ye_combined, "data-generated/filled-keepable-yelloweye-est-rock-mud-predictions-all-regions-sim-500.rds")
  i_ye_1 <- i_ye_combined[[1]]
  saveRDS(i_ye_1, s1)
}

s2 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-3C-sim-500.rds"
if (!file.exists(s2)) {
  i_ye_2 <- i_ye_combined[[2]]
  saveRDS(i_ye_2, s2)
}

s3 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-3D-sim-500.rds"
if (!file.exists(s3)) {
  i_ye_3 <- i_ye_combined[[3]]
  saveRDS(i_ye_3, s3)
}

s4 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A-sim-500.rds"
if (!file.exists(s4)) {
  i_ye_4 <- i_ye_combined[[4]]
  saveRDS(i_ye_4, s4)
}

s5 <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-5B-sim-500.rds"
if (!file.exists(s5)) {
  i_ye_5 <- i_ye_combined[[5]]
  saveRDS(i_ye_5, s5)
}



# For just southern outside HBLL

ss <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-hbll-S-sim-500.rds"
if (!file.exists(ss)) {

  i_ye_S2b <- get_all_sims(m_ye, newdata = hbll_s_grid, split_by_region = F)
  saveRDS(i_ye_S2b, ss)
}

## not needed right now
# f <- "data-generated/filled-keepable-yelloweye-est-rock-mud-index-hbll-S.rds"
# if (!file.exists(f)) {
#
#   p_ye_Sb <- predict(m_ye, newdata = hbll_s_grid, return_tmb_object = TRUE)
#   saveRDS(p_ye_Sb, "data-generated/filled-keepable-yelloweye-est-rock-mud-predictions-hbll-S.rds")
#
#   i_ye_Sb <- get_index(p_ye_Sb, bias_correct = F) # bias_correction not working only for full grid
#   saveRDS(i_ye_Sb, f)
# }


# Save the simulated indices for report

i_ye <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-all-S-sim-500.rds"))[[1]]
saveRDS(i_ye, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-all-S-sim-500.rds"))
i_ye_cda <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-cda-sim-500.rds"))[[1]]
saveRDS(i_ye_cda, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-cda-sim-500.rds"))
i_ye_noncda <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_ye_noncda, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))
i_ye_noncdaN <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_ye_noncdaN, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))
i_ye_noncdaS <- readRDS(here("data-generated/filled-keepable-yelloweye-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_ye_noncdaS, here("report-data/filled-keepable-yelloweye-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))


i_hal <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-all-S-sim-500.rds"))[[1]]
saveRDS(i_hal, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-all-S-sim-500.rds"))
i_hal_cda <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-cda-sim-500.rds"))[[1]]
saveRDS(i_hal_cda, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-cda-sim-500.rds"))
i_hal_noncda <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_hal_noncda, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CD-outside-cda-sim-500.rds"))
i_hal_noncdaN <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_hal_noncdaN, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDN-outside-cda-sim-500.rds"))
i_hal_noncdaS <- readRDS(here("data-generated/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))[[1]]
saveRDS(i_hal_noncdaS, here("report-data/filled-keepable-halibut-delta-est-rock-mud-index-5A3CDS-outside-cda-sim-500.rds"))


# Model checks

p <- predict(m_halibut_tweedie)
p2 <- predict(m_halibut_bin)
p3 <- predict(m_halibut_pos, newdata = m_halibut_bin$data)
p$est_delta <- log(plogis(p2$est) * exp(p3$est))
plot(est_delta~est, data=p)
abline(a = 0, b = 1, lty = 2)



f <- "data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-all-S-sim2.rds"
if (!file.exists(f)) {

  p_hal_S_bin <- predict(m_halibut_bin, newdata = full_s_grid, se_fit = F, sims = 500)
  p_hal_S_pos <- predict(m_halibut_pos, newdata = full_s_grid, se_fit = F, sims = 500)

  p_hal_S <- full_s_grid

  p_hal_S_combined <- log(plogis(p_hal_S_bin) * exp(p_hal_S_pos))
  p_hal_S$est <- apply(p_hal_S_combined, 1, function(x) {median(x)})


  # p_hal_S_bin_out <- apply(p_hal_S_bin, 1, function(x) {median(x)})
  # p_hal_S_pos_out <- apply(p_hal_S_pos, 1, function(x) {median(x)})
  # p_hal_S$est <- log(plogis(p_hal_S_bin_out) * exp(p_hal_S_pos_out))

  # p_hal_S_bin_out <- apply(p_hal_S_bin, 1, function(x) {
  #   data.frame(est = median(x))
  # })
  # p_hal_S_bin_out <- do.call("rbind", p_hal_S_bin_out)
  # p_hal_S_bin_out <- p_hal_S_bin_out[, c("est"), drop = FALSE]
  #
  # p_hal_S_bin2 <- bind_cols(full_s_grid, p_hal_S_bin_out)
  #
  # p_hal_S_pos_out <- apply(p_hal_S_pos, 1, function(x) {
  #   data.frame(est = median(x)
  #   )
  # })
  # p_hal_S_pos_out <- do.call("rbind", p_hal_S_pos_out)
  # p_hal_S_pos_out <- p_hal_S_pos_out[, c("est"), drop = FALSE]
  #
  # p_hal_S_pos2 <- bind_cols(full_s_grid, p_hal_S_pos_out)
  #
  # p_hal_S$data$est <- log(plogis(p_hal_S_bin2$est) * exp(p_hal_S_pos2$est))

  saveRDS(p_hal_S, f)
}



p_hal_delta <- readRDS("data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-all-S.rds")[[1]] %>% select(X, Y, year, est) %>% rename(est_delta = est)

p_hal_delta_sim <- readRDS("data-generated/filled-keepable-halibut-delta-est-rock-mud-predictions-all-S-sim2.rds") %>% select(X, Y, year, est) %>% rename(est_delta_sim = est)

p_hal_tw <- readRDS("data-generated/filled-keepable-halibut-est-rock-mud-predictions-all-S.rds")[[1]] %>% rename(est_tw = est)

p_hal <- left_join(p_hal_tw, p_hal_delta)

plot(est_delta~est_tw, data = p_hal, alpha=0.5)
abline(a = 0, b = 1, lty = 2)

p_hal <- left_join(p_hal_tw, p_hal_delta_sim)

plot(est_delta_sim~est_tw, data = p_hal, alpha=0.5)
abline(a = 0, b = 1, lty = 2)

p_hal <- left_join(p_hal_delta, p_hal_delta_sim)

plot(est_delta_sim~est_delta, data = p_hal, alpha=0.5)
abline(a = 0, b = 1, lty = 2)



