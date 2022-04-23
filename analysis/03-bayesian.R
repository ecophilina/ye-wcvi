library(dplyr)
# library(tidyr)
library(sdmTMB)
library(tmbstan)
options(mc.cores = parallel::detectCores())
TMB::openmp(n = 1L)
options(sdmTMB.cores = 4L)

# Load both data sets

substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)
d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 52.15507) %>%
  left_join(select(substrate, -X, -Y))
d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>%
  left_join(select(substrate, -X, -Y))

years <- sort(unique(d_ye$year))

d_hal <- d_hal %>% filter(year %in% years)
d_ye <- d_ye %>% filter(year %in% years)

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

mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 300)
mesh$mesh$n

plot(mesh$mesh, asp = 1, main = "")
points(d_hal$X, d_hal$Y, pch = ".", col = "blue")

# HAL -------------------------------------------------------

year_prior_sd <- 10
q_sd <- 5
poly_sd <- 5

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

formula <- density ~ 0 +
  as.factor(year) +
  as.factor(survey) +
  poly(rocky, 3) +
  poly(muddy, 3) +
  poly(depth_scaled, 2)

m_hal <- sdmTMB(
  formula,
  data = d_hal,
  mesh = mesh,
  spatial = "on",
  spatiotemporal = list("off", "iid"),
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

f <- paste0(
  "models/halibut-stitch-keepable-model-rocky-",
  "muddy-400kn-delta-IID-stan-aniso.rds"
)

if (!file.exists(f)) {
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
# bayesplot::mcmc_trace(post, pars = c("b_j[1]"))

bayesplot::mcmc_pairs(post,
  pars = c("ln_phi", "ln_tau_O[1]", "ln_tau_O[2]", "b_j[1]",
    "ln_tau_E", "omega_s[1]", "epsilon_st[1]"),
  off_diag_fun = "hex"
)

p1 <- predict(m_hal_fixed, tmbstan_model = m_hal_stan, delta_prediction = "1")
p2 <- predict(m_hal_fixed, tmbstan_model = m_hal_stan, delta_prediction = "2")

qres_binomial_ <- function(y, mu, n = NULL) {
  p <- plogis(mu)
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
mu <- apply(p1, 1, mean)
q <- qres_binomial_(y = d_hal$present, mu)
qqnorm(q);qqline(q)

post <- extract(m_hal_stan)
pos <- which(d_hal$present == 1)
dpos <- d_hal[pos, ]
p2pos <- p2[pos, ]
mu <- apply(exp(p2pos), 1, mean)

q <- qres_gamma_(y = dpos$density, mu, phi = mean(exp(post$ln_phi)))
qqnorm(q);qqline(q)

# pmean <- apply(p, 1, mean)
# psd <- apply(p, 1, sd)

# YE -------------------------------------------------------

priors <- sdmTMBpriors(
  b = normal(rep(0, 16),
    scale = c(
      rep(10, 7),
      rep(5, 1),
      rep(5, 8)
    )
  )
  # matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2)
)

formula <- density ~ 0 +
    as.factor(year) +
    as.factor(survey) +
    poly(rocky, 3) +
    poly(muddy, 3) +
    poly(depth_scaled, 2)

m_ye <- sdmTMB(
  formula,
  data = d_ye,
  mesh = mesh,
  spatiotemporal = list("off", "off"),
  priors = priors,
  time = "year",
  silent = FALSE,
  reml = TRUE,
  anisotropy = TRUE,
  family = delta_gamma()
)

m_ye
m_ye$sd_report
plot_anisotropy(m_ye)
plot_anisotropy(m_ye, model = 2)
tidy(m_ye, conf.int = TRUE)
tidy(m_ye, conf.int = TRUE, model = 2)
tidy(m_ye, "ran_pars", conf.int = TRUE)
tidy(m_ye, "ran_pars", conf.int = TRUE, model = 2)

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

m_ye_stan <- tmbstan(
  obj = m_ye_fixed$tmb_obj,
  iter = 2000,
  chains = 6,
  thin = 4,
  seed = 192819,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)
m_ye_stan

f <- paste0(
  "models/yelloweye-stitch-keepable-model-rocky-",
  "muddy-400kn-delta-spatial-stan-aniso.rds"
)
saveRDS(m_ye_stan, file = f)
m_ye_stan <- readRDS(f)

p1_ye <- predict(m_ye_fixed, tmbstan_model = m_ye_stan, delta_prediction = "1")
p2_ye <- predict(m_ye_fixed, tmbstan_model = m_ye_stan, delta_prediction = "2")

d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
mu <- apply(p1_ye, 1, mean)
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
