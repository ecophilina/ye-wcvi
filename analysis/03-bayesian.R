library(dplyr)
library(sdmTMB)
library(tmbstan)
library(bayesplot)
library(visreg)

options(mc.cores = parallel::detectCores())
TMB::openmp(n = 1L)
options(sdmTMB.cores = 4L)


include_cc <- FALSE
# OR experiment with including non-survey catches
include_cc <- TRUE

latitude_cutoff <- 52.15507 # include all of HBLL S

if (include_cc) {
  # hal_model <- "w-cc2-rocky-muddy-400kn-delta-IID-aniso"
  # ye_model <- "w-cc2-rocky-muddy-400kn-delta-spatial-aniso"
  #
  hal_model <- "w-cc2-wcvi-poly3-500kn-delta-AR1-aniso"
  ye_model <- "w-cc2-wcvi-poly3-500kn-delta-spatial-aniso"
  latitude_cutoff <- 51 # include only west coast Vancouver Island

} else {
  hal_model <- "rocky-muddy-400kn-delta-IID-aniso"
  ye_model <- "rocky-muddy-400kn-delta-spatial-aniso"
}


# Load both data sets

substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)
d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < latitude_cutoff & depth_m < 600) %>%
  left_join(select(substrate, -X, -Y)) %>% filter(muddy >= 0)
d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < latitude_cutoff & depth_m < 600) %>%
  left_join(select(substrate, -X, -Y)) %>% filter(muddy >= 0)

years <- sort(unique(d_ye$year))

if (include_cc) {

  d_hal <- d_hal %>% filter(year %in% years) %>%
    # filter(!(survey == "NON-SURVEY" & year < 2015)) %>%
    mutate(
      fyear = as.factor(year),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      # year = year_true, # try out true year
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  d_ye <- d_ye %>% filter(year %in% years) %>%
    filter(!(survey == "NON-SURVEY" & year > 2015)) %>%
    mutate(
      fyear = as.factor(year),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      # year = year_true,
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 500)
  mesh$mesh$n
  d1 <- filter(d_hal, survey != "NON-SURVEY")
  d2 <- filter(d_hal, survey == "NON-SURVEY")

  plot(mesh$mesh, asp = 1, main = "")
  points(d2$X, d2$Y, pch = ".", col = "red")
  points(d1$X, d1$Y, pch = ".", col = "blue")


  mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = 500)
  mesh$mesh$n
  d1 <- filter(d_ye, survey != "NON-SURVEY")
  d2 <- filter(d_ye, survey == "NON-SURVEY")

  plot(mesh2$mesh, asp = 1, main = "")
  points(d2$X, d2$Y, pch = ".", col = "red")
  points(d1$X, d1$Y, pch = ".", col = "blue")


} else {

  d_hal <- d_hal %>%
    filter(year %in% years) %>%
    filter(survey != "NON-SURVEY") %>%
    mutate(
      fyear = as.factor(year), wt = 1, survey = as.factor(survey))

  d_ye <- d_ye %>%
    filter(year %in% years) %>%
    filter(survey != "NON-SURVEY") %>%
    mutate(
      fyear = as.factor(year), wt = 1, survey = as.factor(survey))

  mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 500)
  mesh$mesh$n

  plot(mesh$mesh, asp = 1, main = "")
  points(d_hal$X, d_hal$Y, pch = ".", col = "blue")
}

# Make relatively fine-scale mesh
# the same grid should work for both species because
# the sets are identical in both

# bnd <- INLA::inla.nonconvex.hull(
  # cbind(d_hal$X, d_hal$Y), convex = 0.5)
# mesh <- INLA::inla.mesh.2d(
#   loc = cbind(d_hal$X, d_hal$Y),
#   boundary = bnd,
#   max.edge = c(0.4, 0.8),
#   offset = c(-0.01, -0.04),
#   cutoff = c(0.1, 0.2),
#   # min.angle = 10
# )
# mesh$n
# plot(mesh)
# points(d_hal$X, d_hal$Y, pch = ".", col = "red")

# mesh <- make_mesh(d_hal, c("X", "Y"), mesh = mesh)

# HAL -------------------------------------------------------

year_prior_sd <- 10
q_sd <- 5
poly_sd <- 20


if(include_cc) {
priors <- sdmTMBpriors(
  b = normal(rep(0, 17),
             scale = c(
               rep(year_prior_sd, 7),
               rep(q_sd, 2),
               rep(poly_sd, 8)
             )
  )
)
} else {
priors <- sdmTMBpriors(
  b = normal(rep(0, 16),
    scale = c(
      rep(year_prior_sd, 7),
      rep(q_sd, 1),
      rep(poly_sd, 8)
    )
  )
  # matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
  # matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2)
)
}


if(include_cc) {

fixed_formula <- density ~ 0 +
  fyear +
  survey +
  poly(rocky, 3) +
  poly(muddy, 3) +
  poly(depth_scaled, 2) + (1|vessel_id)

hal_spatiotemporal <- list("off", "ar1")

} else {

fixed_formula <- density ~ 0 +
  fyear +
  survey +
  poly(rocky, 3) +
  poly(muddy, 3) +
  poly(depth_scaled, 2)

hal_spatiotemporal <- list("off", "iid")

}


m_hal <- sdmTMB(
  fixed_formula,
  # weights = d_hal$wt,
  data = d_hal,
  mesh = mesh,
  spatial = "on",
  spatiotemporal = hal_spatiotemporal,
  share_range = FALSE,
  priors = priors,
  time = "year",
  silent = FALSE,
  anisotropy = TRUE,
  reml = TRUE,
  family = delta_gamma()
)

print(m_hal)
plot_anisotropy(m_hal)
plot_anisotropy(m_hal, model = 2)
m_hal$sd_report
tidy(m_hal, conf.int = TRUE)
tidy(m_hal, conf.int = TRUE, model = 2)
tidy(m_hal, "ran_pars", conf.int = TRUE)
tidy(m_hal, "ran_pars", conf.int = TRUE, model = 2)

saveRDS(m_hal, paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
# m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))

# visreg_delta(m_hal, xvar = "depth_scaled", scale = "response",
#              model = 1, nn = 10)
#
# visreg_delta(m_hal, xvar = "rocky", scale = "response",
#              model = 1, nn = 10)
#
# visreg_delta(m_hal, xvar = "muddy", scale = "response",
#              model = 1, nn = 10)
#
# visreg_delta(m_hal, xvar = "depth_scaled", scale = "response",
#              model = 2, nn = 10)
#
# visreg_delta(m_hal, xvar = "rocky", scale = "response",
#              model = 2, nn = 10)
#
# visreg_delta(m_hal, xvar = "muddy", scale = "response",
#              model = 2, nn = 10)


pars <- sdmTMB:::get_pars(m_hal)
kappa_map <- factor(rep(NA, length(pars$ln_kappa)))
H_map <- factor(rep(NA, length(pars$ln_H_input)))

m_hal_fixed <- update(
  m_hal,
  control = sdmTMBcontrol(
    start = list(
      ln_kappa = pars$ln_kappa,
      ln_H_input = pars$ln_H_input
    ),
    map = list(
      ln_kappa = kappa_map,
      ln_H_input = H_map
    )
  ),
  do_fit = FALSE
)


f <- paste0("models/halibut-model-", hal_model, "-stan.rds")

if (!file.exists(f)) {
  saveRDS(m_hal_fixed, paste0("models/halibut-model-", hal_model, "-tmb.rds"))
  m_hal_stan <- tmbstan(
    obj = m_hal_fixed$tmb_obj,
    iter = 2000,
    chains = 6,
    seed = 192819,
    thin = 4,
    control = list(adapt_delta = 0.9, max_treedepth = 15)
  )
  saveRDS(m_hal_stan, file = f)
} else {
  m_hal_stan <- readRDS(f)
}
m_hal_stan

post <- as.array(m_hal_stan)

pars <- c("ln_phi", "ln_tau_O[1]", "ln_tau_O[2]",
    "ln_tau_E", "omega_s[1]", "epsilon_st[1]")
regex_pars <- "b_j"
bayesplot::mcmc_pairs(post, pars = pars, off_diag_fun = "hex")
bayesplot::mcmc_trace(post, pars = pars, regex_pars = regex_pars)

p1 <- predict(m_hal_fixed, tmbstan_model = m_hal_stan, model = 1)
p2 <- predict(m_hal_fixed, tmbstan_model = m_hal_stan, model = 2)

qres_binomial_ <- function(y, mu, n = NULL) {
  p <- mu
  if (is.null(n)) n <- rep(1, length(y))
  y <- n * y
  a <- stats::pbinom(y - 1, n, p)
  b <- stats::pbinom(y, n, p)
  u <- stats::runif(n = length(y), min = pmin(a, b), max = pmax(a, b))
  stats::qnorm(u)
}

qres_gamma_ <- function(y, mu, phi) {
  s1 <- phi
  s2 <- mu / s1
  u <- stats::pgamma(q = y, shape = s1, scale = s2)
  stats::qnorm(u)
}

d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
mu <- plogis(apply(p1, 1, mean))
q <- qres_binomial_(y = d_hal$present, mu)
qqnorm(q);qqline(q)

post <- extract(m_hal_stan)
pos <- which(d_hal$present == 1)
dpos <- d_hal[pos, ]
p2pos <- p2[pos, ]
mu <- apply(exp(p2pos), 1, mean)

q <- qres_gamma_(y = dpos$density, mu, phi = mean(exp(post$ln_phi)))
qqnorm(q);qqline(q)

s <- simulate(m_hal_fixed, tmbstan_model = m_hal_stan, nsim = 50L)

pos2 <- which(d_hal$present == 1 & d_hal$survey != "NON-SURVEY")
bayesplot::pp_check(d_hal$density[pos2], t(s[pos2,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10()

s <- simulate(m_hal_fixed, tmbstan_model = m_hal_stan, nsim = 50L, model = 1)
table(s[,1])/sum(table(s[,1]))
table(s[,2])/sum(table(s[,2]))
table(s[,3])/sum(table(s[,3]))
table(d_hal$present)/sum(table(d_hal$present))

# YE -------------------------------------------------------


m_ye <- sdmTMB(
  fixed_formula,
  # weights = d_ye$wt,
  data = d_ye,
  mesh = mesh2,
  spatial = "on",
  spatiotemporal = list("off", "off"),
  priors = priors,
  time = "year",
  silent = FALSE,
  reml = TRUE,
  anisotropy = TRUE,
  family = delta_gamma()
  # family = tweedie()
)

saveRDS(m_ye, paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))
# m_ye <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))

m_ye
m_ye$sd_report
plot_anisotropy(m_ye)
plot_anisotropy(m_ye, model = 2)
tidy(m_ye, conf.int = TRUE)
tidy(m_ye, conf.int = TRUE, model = 2)
tidy(m_ye, "ran_pars", conf.int = TRUE)
tidy(m_ye, "ran_pars", conf.int = TRUE, model = 2)

#
# visreg_delta(m_ye, xvar = "depth_scaled", model = 1,
#              scale = "response", nn = 10)
#
# visreg_delta(m_ye, xvar = "depth_scaled", model = 2,
#                scale = "response", nn = 10)

pars <- sdmTMB:::get_pars(m_ye)
kappa_map <- factor(rep(NA, length(pars$ln_kappa)))
H_map <- factor(rep(NA, length(pars$ln_H_input)))

m_ye_fixed <- update(
  m_ye,
  control = sdmTMBcontrol(
    start = list(
      ln_kappa = pars$ln_kappa,
      ln_H_input = pars$ln_H_input
    ),
    map = list(
      ln_kappa = kappa_map,
      ln_H_input = H_map
    )
  ),
  do_fit = FALSE
)


f <- paste0("models/yelloweye-model-", ye_model, "-stan.rds")

if (!file.exists(f)) {

  saveRDS(m_ye_fixed, paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))
  m_ye_stan <- tmbstan(
    obj = m_ye_fixed$tmb_obj,
    iter = 2000,
    chains = 6,
    thin = 4,
    seed = 192819,
    control = list(adapt_delta = 0.9, max_treedepth = 15)
  )
  saveRDS(m_ye_stan, file = f)
} else {
  m_ye_stan <- readRDS(f)
}
m_ye_stan

# p_ye <- predict(m_ye_fixed, tmbstan_model = m_ye_stan)
p1_ye <- predict(m_ye_fixed, tmbstan_model = m_ye_stan, model = 1)
p2_ye <- predict(m_ye_fixed, tmbstan_model = m_ye_stan, model = 2)

d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
mu <- plogis(apply(p1_ye, 1, mean))
q <- qres_binomial_(y = d_ye$present, mu)
qqnorm(q);qqline(q)

post <- extract(m_ye_stan)
pos <- which(d_ye$present == 1)
dpos <- d_ye[pos, ]
p2pos <- p2_ye[pos, ]

# i <- 1
# q <- qres_gamma_(y = dpos$density, exp(p2pos[,i,drop=TRUE]),
#   phi = exp(phipos)[i])
# qqnorm(q);qqline(q)

mu <- apply(exp(p2pos), 1, mean)
q <- qres_gamma_(y = dpos$density, mu, phi = mean(exp(post$ln_phi)))
qqnorm(q);qqline(q)

post <- as.array(m_ye_stan)
pars <- c("ln_phi", "ln_tau_O[1]", "ln_tau_O[2]", "omega_s[1]")
regex_pars <- "b_j"
bayesplot::mcmc_pairs(post, pars = pars, off_diag_fun = "hex")
bayesplot::mcmc_trace(post, pars = pars, regex_pars = regex_pars)
s <- simulate(m_ye_fixed, tmbstan_model = m_ye_stan, nsim = 50L)

pos2 <- which(d_ye$present == 1 & d_ye$survey != "NON-SURVEY")
bayesplot::pp_check(d_ye$density[pos2], t(s[pos2,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10()

s <- simulate(m_ye_fixed, tmbstan_model = m_ye_stan, nsim = 50L, model = 1)
table(s[,1])/sum(table(s[,1]))
table(s[,2])/sum(table(s[,2]))
table(s[,3])/sum(table(s[,3]))
table(d_ye$present)/sum(table(d_ye$present))

