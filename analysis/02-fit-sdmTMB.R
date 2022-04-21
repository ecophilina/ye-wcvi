# library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
# will need this version to replicate report
# remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
library(sdmTMB)

dir.create("models", showWarnings = FALSE)

# choose a starting formula

full_formula <- density ~ 0 + as.factor(year) + as.factor(survey) +
  poly(rocky, 3) + poly(muddy, 3) + poly(depth_scaled, 2)

chosen_priors <- sdmTMBpriors(
  matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
  matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)
)


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
    formula = full_formula,
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
    formula = full_formula,
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
    formula = full_formula,
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implemented yet
    reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta, file = f)
}

## TRY with spatial "off" - doesn't change anything and conceptually problematic
# f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1-sp-off.rds")
# if (!file.exists(f)) {
#   m_halibut_delta1 <- sdmTMB(
#     formula = full_formula,
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
    formula = full_formula,
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
#       poly(rocky, 3) + poly(muddy, 3) +
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
    formula = full_formula,
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

# REML with poly 3
#                         df      AIC
# m_halibut_tweedie        5 4181.155
# m_halibut_tw_anisotropy  7 4178.695
# m_halibut_delta_iid      7 1974.846
# m_halibut_delta_ar1      9 1954.744
# m_halibut_delta_rw       7 1978.946

#  REML = T with I() 2nd order only
#                         df      AIC
# m_halibut_tweedie        5 4203.499
# m_halibut_tw_anisotropy  7 4200.896
# m_halibut_delta_iid      7 2022.485
# m_halibut_delta_ar1      9 2002.604
# m_halibut_delta_rw       7 2028.019


#  REML = T with s( k = 3?)
#                         df      AIC
# m_halibut_tweedie        9 4213.975
# m_halibut_tw_anisotropy 11 4211.417
# m_halibut_delta_iid     15 2042.924
# m_halibut_delta_ar1     17 2023.441
# m_halibut_delta_rw      15 2049.362
# m_halibut_delta_ar1_off 15 2022.831

# this is best so far
m_halibut_delta_ar1
tidy(m_halibut_delta_ar1, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta_ar1, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta_ar1$gradients)

# let ranges be different and add priors
f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1r.rds")
if (!file.exists(f)) {
  m_halibut_delta2r <- sdmTMB(
    formula = full_formula,
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    share_range = FALSE,
    priors = chosen_priors,
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implemented yet
    reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2r, file = f)
}

# improves upon shared ranges ever so slightly
AIC(m_halibut_delta_ar1, m_halibut_delta_ar1_unshared)

# still looks good
m_halibut_delta_ar1_unshared <- readRDS("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1r.rds")
m_halibut_delta_ar1_unshared
tidy(m_halibut_delta_ar1_unshared, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta_ar1_unshared, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta_ar1_unshared$gradients)

## RERUN top model without REML
f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F.rds")
if (!file.exists(f)) {
  m_halibut_deltaF <- sdmTMB(
    formula = full_formula,
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    share_range = FALSE,
    priors = chosen_priors,
    time = "year",
    silent = FALSE,
    # reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_deltaF, file = f)
}

# try simplifying muddy
f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F2.rds")
if (!file.exists(f)) {
  m_halibut_deltaF2 <- sdmTMB(
    formula = density ~
      0 +
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
    # reml = T, # F is simpler to explain.
    family = delta_gamma()
  )
  saveRDS(m_halibut_deltaF2, file = f)
}

m_halibut_delta_ar1F <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F.rds"))
m_halibut_delta_ar1Fs <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F2.rds"))

AIC(m_halibut_delta_ar1F, m_halibut_delta_ar1Fs)

s1 <- simulate(m_halibut_delta_ar1Fs, nsim = 200)
dharma_residuals(s1, m_halibut_delta_ar1Fs)



# Yelloweye Rockfish model

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn.rds"
if (!file.exists(f)) {
  m_ye <- sdmTMB(
    formula = full_formula,
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
    formula = full_formula,
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
    formula = full_formula,
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
    formula = full_formula,
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye2, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta.rds"
if (!file.exists(f)) {
  m_ye3 <- sdmTMB(
    formula = full_formula,
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
  saveRDS(m_ye3, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-r.rds"
if (!file.exists(f)) {
  m_ye3_p <- sdmTMB(
    formula = full_formula,
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "IID",
    share_range = FALSE,
    priors = chosen_priors,
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye3_p, file = f)
}


f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1.rds"
if (!file.exists(f)) {
  m_ye4 <- sdmTMB(
    formula = full_formula,
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
  saveRDS(m_ye4, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1r.rds"
if (!file.exists(f)) {
  m_ye4p <- sdmTMB(
    formula = full_formula,
    data = d_ye,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    share_range = FALSE,
    priors = chosen_priors,
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye4p, file = f)
}




m_ye_tw_iid_anisotopy <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn.rds")
m_ye_tw_iid <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-no-anisotropy.rds")
m_ye_tw_ar1 <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-AR1.rds")
m_ye_delta_iid <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta.rds")
m_ye_delta_iid_unshared <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-r.rds")
m_ye_delta_ar1 <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1.rds")
m_ye_delta_ar1_unshared <- readRDS( "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1r.rds")

AIC(m_ye_tw_iid_anisotopy,
    m_ye_tw_iid,
    m_ye_tw_ar1,
    m_ye_delta_iid,
    m_ye_delta_ar1,
    m_ye_delta_iid_unshared,
    m_ye_delta_ar1_unshared)

#                         df      AIC
# m_ye_tw_iid_anisotopy    7 3730.988
# m_ye_tw_iid              5 3684.114
# m_ye_tw_ar1              8 3673.368
# m_ye_delta_iid           7 2768.750
# m_ye_delta_ar1           9 2770.323
# m_ye_delta_iid_unshared  9 2774.729
# m_ye_delta_ar1_unshared 11 2775.324

# not the simplest, but < 10 delta AIC and most flexible
m_ye_delta_ar1_unshared
tidy(m_ye_delta_ar1_unshared, "ran_pars", conf.int = TRUE)
tidy(m_ye_delta_ar1_unshared, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye_delta_ar1_unshared$gradients)


# # check which fixed effects are best
# # but doesn't converge with ML
# f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1F.rds"
# if (!file.exists(f)) {
#   m_ye3f <- sdmTMB(
#     formula = full_formula,
#     data = d_ye,
#     mesh = mesh400kn,
#     time = "year",
#     spatial = "on",
#     spatiotemporal = "AR1",
#     share_range = F,
#     priors = chosen_priors,
#     silent = FALSE,
#     # anisotropy = TRUE, # not implimented yet
#     # reml = T, # F is simplier to explain.
#     family = delta_gamma()
#   )
#   saveRDS(m_ye3f, file = f)
# }

# this time it's rocky that could use simplifying

# f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-poly2-3.rds"
# if (!file.exists(f)) {
#   m_ye4p2 <- sdmTMB(
#     formula = density ~ 0 + as.factor(year) +
#       as.factor(survey) +
#       poly(rocky, 2) +
#       poly(muddy, 3) +
#       poly(depth_scaled, 2),
#     data = d_ye,
#     mesh = mesh400kn,
#     time = "year",
#     spatial = "on",
#     spatiotemporal = "AR1",
#     share_range = F,
#     priors = chosen_priors,
#     silent = FALSE,
#     # anisotropy = TRUE, # not implimented yet
#     reml = T, # F is simplier to explain.
#     family = delta_gamma()
#   )
#   saveRDS(m_ye4p2, file = f)
# }
#
# m_ye_delta_poly23 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-poly2-3.rds")
# m_ye_delta_poly23 <- run_extra_optimization(m_ye_delta_poly23)
# # model one can't estimate sigma_E and rho


f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-poly2.rds"
if (!file.exists(f)) {
  m_ye_mixed <- sdmTMB(
    formula = density ~ 0 + as.factor(year) +
      as.factor(survey) +
      poly(rocky, 2) +
      poly(muddy, 2) +
      poly(depth_scaled, 2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = list("AR1","IID"),
    share_range = F,
    priors = chosen_priors,
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye_mixed, file = f)
}

# m_ye_delta_poly2 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-poly-2.rds")
# this time its model 2 that can't estimate sigma_E and rho, so letting model two be IID

m_ye_delta_poly2 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1-poly2.rds")
m_ye_delta_poly2
tidy(m_ye_delta_poly2, "ran_pars", conf.int = TRUE)
tidy(m_ye_delta_poly2, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye_delta_poly2$gradients)

# delta AR1 with unshared ranges only estimates fully with REML = F and 3 order poly on both substrates

m_ye_delta_poly3 <- readRDS("models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-AR1r.rds")
m_ye_delta_poly3
tidy(m_ye_delta_poly3, "ran_pars", conf.int = TRUE)
tidy(m_ye_delta_poly3, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye_delta_poly3$gradients)


s2 <- simulate(m_ye_delta_poly3, nsim = 200)
dharma_residuals(s2, m_ye_delta_poly3)

