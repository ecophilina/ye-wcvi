# stan version?

library(dplyr)
library(tidyr)
library(sdmTMB)


# Load both data sets

substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

years <- sort(unique(d_ye$year))

d_hal <- d_hal %>% filter(year %in% years)
d_hal$present <- ifelse(d_hal$density > 0, 1, 0)

d_ye <- d_ye %>% filter(year %in% years)
d_ye$present <- ifelse(d_ye$density > 0, 1, 0)

# Make relatively finescale mesh
# the same grid should work for both species because the sets are identical in both
mesh400kn <- make_mesh(d_hal, c("X", "Y"), n_knots = 400)
plot(mesh400kn$mesh, asp = 1, main = "");points(d_hal$X, d_hal$Y, pch = ".")


# sdmTMB calls chosen in 02-fit-sdmTMB.R

chosen_priors <- sdmTMBpriors(
  matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
  matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)
)

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F2.rds")
if (!file.exists(f)) {
m_hal <- sdmTMB(
  formula = density ~ 0 +
    as.factor(year) +
    as.factor(survey) +
    poly(rocky, 3) +
    poly(muddy, 2) +
    poly(depth_scaled, 2),
  data = d_hal,
  mesh = mesh400kn,
  spatial = "on",
  spatiotemporal = "AR1",
  share_range = FALSE,
  priors = chosen_priors,
  time = "year",
  silent = FALSE,
  # reml = T, # works both ways
  family = delta_gamma()
)
} else {
  m_hal <- readRDS(f)
}

f <- paste0("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1r.rds")
if (!file.exists(f)) {
m_ye <- sdmTMB(
  formula = density ~ 0 +
    as.factor(year) +
    as.factor(survey) +
    poly(rocky, 3) +
    poly(muddy, 3) +
    poly(depth_scaled, 2),
  data = d_ye,
  mesh = mesh400kn,
  spatial = "on",
  spatiotemporal = "AR1",
  share_range = FALSE,
  priors = chosen_priors,
  time = "year",
  silent = FALSE,
  reml = T, # only works when true
  family = delta_gamma()
)
} else {
  m_ye <- readRDS(f)
}
