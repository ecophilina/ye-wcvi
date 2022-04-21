# library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
# will need this version to replicate report
# remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
library(sdmTMB)

dir.create("models", showWarnings = FALSE)

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
d_hal$year_f <- as.factor(d_hal$year)

d_ye <- d_ye %>% filter(year %in% years)
d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
d_ye$year_f <- as.factor(d_ye$year)

# Make relatively finescale mesh
# the same grid should work for both species because the sets are identical in both
mesh400kn <- make_mesh(d_hal, c("X", "Y"), n_knots = 400)
plot(mesh400kn$mesh, asp = 1, main = "");points(d_hal$X, d_hal$Y, pch = ".")

# mesh_cu <- make_mesh(d_hal, c("X", "Y"), cutoff = 0.075)
# mesh_cu$mesh$n
# plot(mesh_cu$mesh, asp = 1, main = "");points(d_hal$X, d_hal$Y, pch = ".")


# Compare possible Pacific Halibut models

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tweedie.rds")
if (!file.exists(f)) {
  m_halibut_tweedie <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_halibut_tweedie, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tw-anisotropy.rds")
if (!file.exists(f)) {
  m_halibut_tw2 <- sdmTMB(
    formula = density ~
      0 + as.factor(year) +
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_halibut_tw2, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta.rds")
if (!file.exists(f)) {
  m_halibut_delta <- sdmTMB(
    formula = density ~
      0 +
      as.factor(year) +
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta, file = f)
}

## TRY with spatial "off" - doesn't change anything and conceptually problematic
# f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1-sp-off.rds")
# if (!file.exists(f)) {
#   m_halibut_delta1 <- sdmTMB(
#     formula = density ~ 0 + as.factor(year) + as.factor(survey) +
#       s(rocky, k = 5) + s(muddy, k = 5) +
#       depth_scaled + I(depth_scaled^2),
#     data = d_hal,
#     mesh = mesh400kn,
#     spatial = "off",
#     spatiotemporal = "AR1",
#     time = "year",
#     silent = FALSE,
#     # anisotropy = TRUE, # not implemented yet
#     reml = T, # F is simpler to explain.
#     family = delta_gamma()
#   )
#   saveRDS(m_halibut_delta1, file = f)
# }

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1.rds")
if (!file.exists(f)) {
  m_halibut_delta2 <- sdmTMB(
    formula = density ~
      0 +
      as.factor(year) +
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implemented yet
    reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2, file = f)
}

# # Try a random walk on year intercepts - doesn't converge well and > 10 AIC worse
# # also explored smoothers on year, but also not useful
# f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1-rwyr.rds")
# if (!file.exists(f)) {
#   m_halibut_delta2b <- sdmTMB(
#     formula = density ~
#       0 +
#       # as.factor(year) +
#       as.factor(survey) +
#       # s(rocky, k = 5) + s(muddy, k = 5) +
#       rocky + I(rocky^2) +
#       muddy + I(muddy^2) +
#       depth_scaled + I(depth_scaled^2),
#     time_varying = ~1,
#     data = d_hal,
#     mesh = mesh400kn,
#     spatial = "on",
#     spatiotemporal = "AR1",
#     time = "year",
#     silent = FALSE,
#     # anisotropy = TRUE, # not implemented yet
#     # reml = T, # F is simpler to explain.
#     family = delta_gamma()
#   )
#   saveRDS(m_halibut_delta2b, file = f)
# }

# AIC(m_halibut_delta2, m_halibut_delta2b)

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-RW.rds")
if (!file.exists(f)) {
  m_halibut_delta3 <- sdmTMB(
    formula = density ~
      0 + as.factor(year) +
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    # time_varying = ~ 1,
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta3, file = f)
}

## Load saved models

m_halibut_tweedie <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tweedie.rds"))
m_halibut_tw_anisotropy <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tw-anisotropy.rds"))

m_halibut_delta_iid <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta.rds"))
m_halibut_delta_ar1 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1.rds"))
m_halibut_delta_rw <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-RW.rds"))
# m_halibut_delta_ar1_off <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1-sp-off.rds"))


## Comparing models

AIC(m_halibut_tweedie, m_halibut_tw_anisotropy,
    m_halibut_delta_iid, m_halibut_delta_ar1, m_halibut_delta_rw)

#  REML = T with I()
#                         df      AIC
# m_halibut_tweedie        5 4203.499
# m_halibut_tw_anisotropy  7 4200.896
# m_halibut_delta_iid      7 2022.485
# m_halibut_delta_ar1      9 2002.604
# m_halibut_delta_rw       7 2028.019


#  REML = T with s()
#                         df      AIC
# m_halibut_tweedie        9 4213.975
# m_halibut_tw_anisotropy 11 4211.417
# m_halibut_delta_iid     15 2042.924
# m_halibut_delta_ar1     17 2023.441
# m_halibut_delta_rw      15 2049.362
# m_halibut_delta_ar1_off 15 2022.831

#                         df      AIC
# m_halibut_tweedie        9 4271.514
# m_halibut_tw_anisotropy 11 4269.305
# m_halibut_delta_iid     15 2052.309
# m_halibut_delta_ar1     17 2036.876
# m_halibut_delta_rw      15 2074.266
# m_halibut_delta_ar1_off 15 2037.700


# faulty version with REML = F
#                         df      AIC
# m_halibut_tweedie       19 4264.005
# m_halibut_tw_anisotropy 21 4262.296
# m_halibut_delta_iid     35 2031.504
# m_halibut_delta_ar1     35 2017.323
# m_halibut_delta_rw      33 2059.692



## RERUN top model without REML
f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F.rds")
if (!file.exists(f)) {
  m_halibut_delta2F <- sdmTMB(
    formula = density ~
      0 +
      as.factor(year) +
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    # reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2F, file = f)
}

# f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1Fs.rds")
# if (!file.exists(f)) {
#   m_halibut_delta2Fs <- sdmTMB(
#     formula = density ~
#       0 +
#       as.factor(year) +
#       as.factor(survey) +
#       s(rocky, k=3) +
#       s(muddy, k=3) +
#       depth_scaled + I(depth_scaled^2),
#     data = d_hal,
#     mesh = mesh400kn,
#     spatial = "on",
#     spatiotemporal = "AR1",
#     time = "year",
#     silent = FALSE,
#     # reml = T, # F is simpler to explain.
#     family = delta_gamma()
#   )
#   saveRDS(m_halibut_delta2Fs, file = f)
# }

m_halibut_delta_ar1 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F.rds"))
# m_halibut_delta_ar1s <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1Fs.rds"))
#
# AIC(m_halibut_delta_ar1, m_halibut_delta_ar1s)

m_halibut_delta_ar1
tidy(m_halibut_delta_ar1, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta_ar1, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta_ar1$gradients)

s1 <- simulate(m_halibut_delta_ar1, nsim = 200)
dharma_residuals(s1, m_halibut_delta_ar1)



# Yelloweye Rockfish model
# Much less temporal variation, so have confirmed that an AR1 didn't improve model. But what about a spatial-only model?

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn.rds"
if (!file.exists(f)) {
  m_ye <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye, file = f)
}



f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-no-anisotropy.rds"
if (!file.exists(f)) {
  m_ye0 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye0, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta.rds"
if (!file.exists(f)) {
  m_ye1 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye1, file = f)
}

# AR1 with spatial on doesn't converge, RW on it's own might
f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-AR1.rds"
if (!file.exists(f)) {
  m_ye2 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye2, file = f)
}



f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1.rds"
if (!file.exists(f)) {
  m_ye3 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) + #I(rocky^3) +
      muddy + I(muddy^2) + #I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye3, file = f)
}

m_ye_tw_iid_anisotopy <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn.rds")
m_ye_tw_iid <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-no-anisotropy.rds")

m_ye_tw_ar1 <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-AR1.rds")
m_ye_delta_iid <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta.rds")
m_ye_delta_ar1 <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1.rds")

AIC(m_ye, m_ye0, m_ye1, m_ye2, m_ye3)

AIC(m_ye_tw_iid_anisotopy ,
    m_ye_tw_iid ,
    m_ye_tw_ar1 ,
    m_ye_delta_iid ,
    m_ye_delta_ar1 )

# df      AIC
# m_ye   7 3727.548
# m_ye0  5 3740.245
# m_ye1  7 2860.381
# m_ye2  8 3728.826
# m_ye3  9 2861.937

## check that IDD is still best with 3rd order poly
f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-3.rds"
if (!file.exists(f)) {
  m_ye3_3 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) + I(rocky^3) +
      muddy + I(muddy^2) + I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye3_3, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-3.rds"
if (!file.exists(f)) {
  m_ye1_3 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) + I(rocky^3) +
      muddy + I(muddy^2) + I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye1_3, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-RW-IID-3.rds"
if (!file.exists(f)) {
  m_ye1n3_3 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      # s(rocky, k = 5) +
      # s(muddy, k = 5) +
      rocky + I(rocky^2) + I(rocky^3) +
      muddy + I(muddy^2) + I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = c("RW", "IID"),
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye1n3_3, file = f)
}


f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-priors-3F.rds"
if (!file.exists(f)) {
  m_ye3p <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      rocky + I(rocky^2) + #I(rocky^3) +
      muddy + I(muddy^2) + I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = c("AR1"),
    share_range = F,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye3p, file = f)
}

tidy(m_ye3p, "ran_pars", conf.int = TRUE)
tidy(m_ye3p, "ran_pars", conf.int = TRUE, model = 2)

AIC(m_ye1_3, m_ye3_3, m_ye1n3_3)

m_ye1n3_3
tidy(m_ye1n3_3, "ran_pars", conf.int = TRUE)
tidy(m_ye1n3_3, "ran_pars", conf.int = TRUE, model = 2)

# check which fixed effects are best
f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1F.rds"
# if (!file.exists(f)) {
  m_ye3 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      rocky + I(rocky^2) + #I(rocky^3) +
      muddy + I(muddy^2) + #I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = c("AR1"),
    share_range = F,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye3, file = f)
}

#
# m_ye3
# tidy(m_ye3, "ran_pars", conf.int = TRUE)
# tidy(m_ye3, "ran_pars", conf.int = TRUE, model = 2)
# max(m_ye3$gradients)
#

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-3F.rds"
# if (!file.exists(f)) {
  m_ye4 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      rocky + I(rocky^2) + I(rocky^3) +
      muddy + I(muddy^2) + I(muddy^3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = c("AR1"),
    share_range = F,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye4, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-sF.rds"
# if (!file.exists(f)) {
  m_ye5 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 5) +
      s(muddy, k = 5) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = c("AR1"),
    share_range = F,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye5, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-polyF.rds"
if (!file.exists(f)) {
  m_ye6 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      poly(rocky, 3) +
      poly(muddy, 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = c("AR1"),
    share_range = F,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)),
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye6, file = f)
}

m_ye3 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1F.rds")
m_ye4 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-3F.rds")
m_ye5 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-sF.rds")
m_ye6 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-polyF.rds")

AIC(m_ye3, m_ye4, m_ye5, m_ye6)

# df      AIC
# m_ye3 37 2869.325
# m_ye4 41 2865.230
# m_ye5 39 2863.494

m_ye3
tidy(m_ye3, "ran_pars", conf.int = TRUE)
tidy(m_ye3, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye3$gradients)

m_ye4
tidy(m_ye4, "ran_pars", conf.int = TRUE)
tidy(m_ye4, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye4$gradients)

m_ye6
tidy(m_ye6, "ran_pars", conf.int = TRUE)
tidy(m_ye6, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye6$gradients)


s2 <- simulate(m_ye6, nsim = 200)
dharma_residuals(s2, m_ye6)

