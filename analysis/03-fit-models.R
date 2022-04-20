library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
# will need this version to replicate report
# remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
library(sdmTMB)

dir.create("models", showWarnings = FALSE)

# Load both data sets

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 52.15507) %>%
  filter(year %in% years)%>% left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>%
  filter(year %in% years)%>% left_join(select(substrate, -X, -Y))


# Make relatively finescale mesh
# the same grid should work for both species because the sets are identical in both
mesh400kn <- make_mesh(d_hal, c("X", "Y"), n_knots = 400)
# mesh_cu <- make_mesh(d_utm, c("X", "Y"), cutoff = 0.075)
# mesh_cu$mesh$n
# plot(mesh_cu$mesh, asp = 1, main = "");points(d_utm$X, d_utm$Y, pch = ".")
plot(mesh400kn$mesh, asp = 1, main = "");points(d_utm$X, d_utm$Y, pch = ".")

# Pacific Halibut models

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tweedie.rds")
if (!file.exists(f)) {
  m_halibut_tweedie <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) + s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    family = tweedie(link = "log")
  )
  saveRDS(m_halibut_tweedie, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tw-anisotropy.rds")
if (!file.exists(f)) {
  m_halibut_tw2 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) + s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    family = tweedie(link = "log")
  )
  saveRDS(m_halibut_tw2, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta.rds")
if (!file.exists(f)) {
  d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
  m_halibut_delta <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) + s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE,
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1.rds")
if (!file.exists(f)) {
  d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
  m_halibut_delta2 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) + s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    spatial = "off",
    spatiotemporal = "AR1",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implemented yet
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta2, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-RW.rds")
if (!file.exists(f)) {
  d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
  # d_hal$year_f <- as.factor(d_hal$year)
  m_halibut_delta3 <- sdmTMB(
    formula = density ~ 0 +
      as.factor(year) +
      as.factor(survey) +
      s(rocky, k = 3) + s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh400kn,
    # time_varying = ~ 1,
    spatial = "off",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE,
    family = delta_gamma()
  )
  saveRDS(m_halibut_delta3, file = f)
}

## Load saved models

m_halibut_tweedie <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tweedie.rds"))
m_halibut_tw2 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-tw-anisotropy.rds"))
m_halibut_delta1 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta.rds"))
m_halibut_delta2 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-AR1.rds"))
m_halibut_delta3 <- readRDS(paste0("models/halibut-stitch-keepable-model-rocky-muddy-400kn-delta-RW.rds"))

## Comparing models

m_halibut_tweedie
tidy(m_halibut_tweedie, "ran_pars", conf.int = TRUE)

m_halibut_delta1
tidy(m_halibut_delta1, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta1, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta1$gradients)

m_halibut_delta2
tidy(m_halibut_delta2, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta2, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta2$gradients)

m_halibut_delta3
tidy(m_halibut_delta3, "ran_pars", conf.int = TRUE)
tidy(m_halibut_delta3, "ran_pars", conf.int = TRUE, model = 2)
max(m_halibut_delta3$gradients)

AIC(m_halibut_tweedie, m_halibut_tw2, m_halibut_delta, m_halibut_delta2, m_halibut_delta3)

#                   df      AIC
# m_halibut_tweedie 19 4264.005
# m_halibut_tw2     21 4262.296
# m_halibut_delta   35 2031.504
# m_halibut_delta2  35 2017.323
# m_halibut_delta3  33 2059.692

s1 <- simulate(m_halibut_delta2, nsim = 200)
dharma_residuals(s1, m_halibut_delta2)



# Yelloweye Rockfish model
# Much less temporal variation, so have confirmed that an AR1 didn't improve model. But what about a spatial-only model?

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn.rds"
if (!file.exists(f)) {
  m_ye <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) +
      s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatiotemporal = "IID",
    time = "year",
    silent = FALSE,
    anisotropy = TRUE,
    # reml = T, # F is simplier to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-no-anisotropy-RW.rds"
if (!file.exists(f)) {
  m_ye0 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) +
      s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "off",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE,
    # reml = T, # F is simplier to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m_ye0, file = f)
}

f <- "models/yelloweye-stitch-hbll-mw-rocky-muddy-400kn-delta.rds"
if (!file.exists(f)) {
  d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
  m_ye1 <- sdmTMB(
    formula = density ~ 0 + as.factor(year) + as.factor(survey) +
      s(rocky, k = 3) +
      s(muddy, k = 3) +
      depth_scaled + I(depth_scaled^2),
    data = d_ye,
    mesh = mesh400kn,
    spatial = "off",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    # anisotropy = TRUE, # not implimented yet
    # reml = T, # F is simplier to explain.
    family = delta_gamma()
  )
  saveRDS(m_ye1, file = f)
}

m_ye
tidy(m_ye, "ran_pars", conf.int = TRUE)
max(m_ye$gradients)

m_ye0
tidy(m_ye0, "ran_pars", conf.int = TRUE)
max(m_ye0$gradients)

m_ye1
tidy(m_ye1, "ran_pars", conf.int = TRUE)
tidy(m_ye1, "ran_pars", conf.int = TRUE, model = 2)
max(m_ye1$gradients)

AIC(m_ye, m_ye0, m_ye1)

#       df      AIC
# m_ye  21 3535.701
# m_ye0 18 3667.041
# m_ye1 35 2756.618


s2 <- simulate(m_ye, nsim = 200)
dharma_residuals(s2, m_ye)

