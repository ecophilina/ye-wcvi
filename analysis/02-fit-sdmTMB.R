# model selection using sdmTMB
library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)

dir.create("models", showWarnings = FALSE)

# choose a starting formula

full_formula <- density ~ 0 + as.factor(year) + as.factor(survey) +
  poly(rocky, 3) + poly(muddy, 3) + poly(depth_scaled, 2)

# these priors only work without anisotropy
chosen_priors <- sdmTMBpriors(
  matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
  matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)
)

# Load both data sets
# originally conducted prior to the addition of commercial data, so filtered here
# AIC reports still from original analyses that included more of the coast


substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
    select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(survey != "NON-SURVEY") %>% # using survey data only
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(survey != "NON-SURVEY") %>% # using survey data only
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
    reml = T,
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
    reml = T,
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
    # anisotropy = TRUE, # not tried at this stage, added later
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta, file = f)
}


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
    # anisotropy = TRUE, # not tried at this stage, added later
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2, file = f)
}

# # Try a random walk on year intercepts - doesn't converge well and > 10 AIC worse
# # also explored smoothers on year, but also not useful

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
    reml = T,
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

# REML = T with poly 3
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
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2r, file = f)
}


# still looks good
m_halibut_delta_ar1_unshared <- readRDS("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1r.rds")
m_halibut_delta_ar1_unshared
tidy(m_halibut_delta_ar1_unshared, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta_ar1_unshared, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta_ar1_unshared$gradients)

# improves upon shared ranges ever so slightly
AIC(m_halibut_tweedie, m_halibut_tw_anisotropy,
    m_halibut_delta_iid, m_halibut_delta_rw,
    m_halibut_delta_ar1, m_halibut_delta_ar1_unshared)

# df      AIC
# m_halibut_tweedie             5 4181.155
# m_halibut_tw_anisotropy       7 4178.695
# m_halibut_delta_iid           7 1974.846
# m_halibut_delta_rw            7 1978.946
# m_halibut_delta_ar1           9 1954.744
# m_halibut_delta_ar1_unshared 11 1952.177


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
    # reml = T,
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
      # poly(rocky, 2) +
      # poly(muddy, 2) +
      rocky +
      muddy +
      poly(depth_scaled, 2),
    data = d_hal,
    mesh = mesh400kn,
    spatial = "on",
    spatiotemporal = "AR1",
    share_range = FALSE,
    priors = chosen_priors,
    time = "year",
    silent = FALSE,
    # reml = T,
    family = delta_gamma()
  )
  saveRDS(m_halibut_deltaF2, file = f)
}

m_halibut_delta_ar1F <- readRDS("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F.rds")
m_halibut_delta_ar1Fs <- readRDS("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1F2.rds")

AIC(m_halibut_delta_ar1F, m_halibut_delta_ar1Fs)
# df      AIC
# m_halibut_delta_ar1F  43 13194.41
# m_halibut_delta_ar1Fs 35 13190.34

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
    reml = T,
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
    reml = T,
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
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_ye1, file = f)
}


f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-r.rds"
if (!file.exists(f)) {
  m_ye1_pr <- sdmTMB(
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
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_ye3_pr, file = f)
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
    reml = T,
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
# m_ye_delta_iid           7 2768.750
# m_ye_delta_ar1           9 2770.323
# m_ye_delta_iid_unshared  9 2774.729
# m_ye_delta_ar1_unshared 11 2775.324


m_ye_delta_iid
tidy(m_ye_delta_iid, "ran_pars", conf.int = TRUE)
tidy(m_ye_delta_iid, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye_delta_iid$gradients)

# try simplifying fixed effects
f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta-IID-s.rds"
if (!file.exists(f)) {
  m_ye_delta_s <- sdmTMB(
    formula = density ~ 0 + as.factor(year) +
      as.factor(survey) +
      poly(rocky, 2) +
      poly(muddy, 2) +
      # rocky +
      # muddy +
      poly(depth_scaled, 2),
    data = d_ye,
    mesh = mesh400kn,
    time = "year",
    spatial = "on",
    spatiotemporal = "IID",
    # share_range = F,
    priors = chosen_priors,
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    reml = T,
    family = delta_gamma()
  )
  saveRDS(m_ye_delta_s, file = f)
}


AIC(m_ye_delta_iid,
  m_ye_delta_s)

#                 df      AIC
# m_ye_delta_iid  39 10072.07
# m_ye_delta_s    35 10095.27

## moved on to using a bayesian framework at this point and also decided to add commercial data
## we apply what we learned here about the spatiotemporal structure of each species
## choosing delta models over the tweedie
## some additional adjustments are made based on model diagnostics and fitting times
## at the bayesian stage
