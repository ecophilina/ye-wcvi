library(dplyr)
library(sdmTMB)
library(tmbstan)
library(bayesplot)
library(visreg)
options(scipen=999)
options(mc.cores = parallel::detectCores())

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

# include_cc <- FALSE
# OR experiment with including non-survey catches
include_cc <- TRUE
st_year_pair <- TRUE
set_knots <- 500

if (include_cc) {
  # hal_model <- "w-cc2-rocky-muddy-400kn-delta-IID-aniso"
  # ye_model <- "w-cc2-rocky-muddy-400kn-delta-spatial-aniso"
  #
  # hal_model <- "w-good-depths-500kn-delta-AR1-aniso"
  # ye_model <- "w-good-depths-500kn-delta-iid-aniso"

  # hal_model <- "w-deeper-500kn-delta-AR1-aniso-dec22"
  # # hal_model <- "w-deeper-500kn-delta-AR1-aniso"
  # ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-dec22"

  ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-may23"
  hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"

  # hal_model <- "w-deeper-5km-delta-AR1-aniso-may23"
  # hal_model <- "w-deeper-450kn-delta-AR1-aniso-may23"
  # hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso"
  # hal_model <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"
  # hal_model <- "w-deeper-450kn-both-true-year-RW-no-yf-aniso"
  # hal_model <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"

  # hal_model <- "w-deeper-5km-delta-true-year-RW-no-yf-aniso"

  # ye_model <- "w-deeper-all-yrs-450kn-delta-iid-aniso"
  # ye_model <- "w-deeper-all-yrs-450kn-true-year-iid-aniso"
  # ye_model <- "w-deeper-all-yrs-450kn-true-year-RW-aniso"
  # ye_model <- "w-deeper-450kn-delta-true-year-RW-RE-aniso"
  # ye_model <- "w-deeper-450kn-both-true-year-RW-no-yf-aniso"
  # ye_model <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"

  # ye_model <- "w-deeper-5km-delta-true-year-RW-no-yf-aniso"

  latitude_cutoff <- 51 # include only west coast Vancouver Island

} else {

  # latitude_cutoff <- 52.15507 # include all of HBLL S
  # hal_model <- "rocky-muddy-400kn-delta-IID-aniso"
  # ye_model <- "rocky-muddy-400kn-delta-spatial-aniso"

  # hal_model <- "w-deeper-500kn-delta-AR1-aniso-no-com"
  hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso-no-com"
  ye_model <- "w-deeper-all-yrs-450kn-true-year-iid-aniso-no-com"
  # ye_model <- "w-deeper-all-yrs-450kn-true-year-aniso-RW-no-com"
  hal_model <- "w-deeper-6km-both-true-year-RW-no-yf-aniso-no-com"
  ye_model <- "w-deeper-200kn-both-true-year-RW-no-yf-aniso-no-com"

  latitude_cutoff <- 51 # include only west coast Vancouver Island
}


# Load both data sets
# make sure the substrate data has been updated since any new fishing events were added
substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < latitude_cutoff & depth_m < 1000) %>%
  left_join(select(substrate, -X, -Y)) %>% filter(muddy >= 0)
d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < latitude_cutoff & depth_m < 1000) %>%
  left_join(select(substrate, -X, -Y)) %>% filter(muddy >= 0)

years <- sort(unique(d_ye$year))

if (include_cc) {

  d_hal <- d_hal %>%
    filter(year %in% years) %>%
    filter(!is.na(vessel_id)) %>%
    filter(survey != "NON-SURVEY" | (dist_km_fished < 3 & dist_km_fished > 0.2)) %>%
    # filter(!(survey == "NON-SURVEY" & year < 2015)) %>%
    mutate(
      fyear = as.factor(year),
      fyear_true = as.factor(year_true),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      # year = year_true, # try out true year
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  d_ye <- d_ye %>%
    filter(year %in% years) %>%
    filter(!is.na(vessel_id)) %>%
    filter(survey != "NON-SURVEY" | (dist_km_fished < 3 & dist_km_fished > 0.2)) %>%
    # exclude samples after restrictions introduced
    # filter(!(survey == "NON-SURVEY" & year > 2015)) %>%
    mutate(
      survey = ifelse(survey == "NON-SURVEY" & year > 2015, "RESTRICTED", survey),
      fyear = as.factor(year),
      fyear_true = as.factor(year_true),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      # year = year_true,
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  # crs_utm9 <- 3156 # Pick a projection, here UTM9
  # st_crs(bc_coast) <- 4326 # 'WGS84'; necessary on some installs
  # bc_coast2 <- st_transform(bc_coast, crs_utm9)

  mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = set_knots)
  # mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 400)
  # mesh <- make_mesh(d_hal, c("X", "Y"), cutoff = 0.05)
  # mesh <- add_barrier_mesh(
  #   mesh,
  #   bc_coast2,
  #   range_fraction = 0.1,
  #   proj_scaling = 100000,
  #   plot = TRUE
  # )

  mesh$mesh$n

  d1 <- filter(d_hal, survey != "NON-SURVEY")
  d2 <- filter(d_hal, survey == "NON-SURVEY")
  plot(mesh$mesh, asp = 1, main = "")
  points(d2$X, d2$Y, pch = ".", col = "red")
  points(d1$X, d1$Y, pch = ".", col = "blue")


  mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = set_knots)
  # mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = 400)
  # mesh2 <- make_mesh(d_ye, c("X", "Y"), cutoff = 0.05)
  # mesh2 <- add_barrier_mesh(
  #   mesh2,
  #   bc_coast2,
  #   range_fraction = 0.1,
  #   proj_scaling = 100000,
  #   plot = TRUE
  # )

  mesh2$mesh$n
  d1 <- filter(d_ye, survey != "NON-SURVEY")
  d2 <- filter(d_ye, survey == "NON-SURVEY")
  d3 <- filter(d_ye, survey == "RESTRICTED")
  plot(mesh2$mesh, asp = 1, main = "")
  points(d2$X, d2$Y, pch = ".", col = "red")
  points(d1$X, d1$Y, pch = ".", col = "blue")
  points(d3$X, d3$Y, pch = ".", col = "orange")

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

  # mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 250)
  # mesh <- make_mesh(d_hal, c("X", "Y"), cutoff = 0.08)
  mesh$mesh$n

  plot(mesh$mesh, asp = 1, main = "")
  points(d_hal$X, d_hal$Y, pch = ".", col = "blue")

  mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = 200)
  # mesh <- make_mesh(d_hal, c("X", "Y"), cutoff = 0.06)
}

unique(d_hal$survey)
filter(d_hal, survey == "NON-SURVEY") %>% nrow()
filter(d_hal, survey != "NON-SURVEY") %>% nrow()


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

# Choose priors --------------------------------------------
year_prior_sd <- 10
q_sd <- 5
poly_sd <- 20

if (include_cc) {
  formula1 <- density ~ 0 +
    fyear +
    survey +
    rocky +
    muddy +
    poly(depth_scaled, 2) +
    (1 | vessel_id)

  priors1 <- sdmTMBpriors(
    b = normal(rep(0, 13),
      scale = c(
        rep(year_prior_sd, 7),
        rep(q_sd, 2),
        rep(poly_sd, 4)
      )
    )
  )

  formulaR <- density ~ 1 +
    survey +
    rocky +
    muddy +
    poly(depth_scaled, 2) +
    # (1 | fyear) +
    (1 | vessel_id)

  priorsR <- sdmTMBpriors(
    b = normal(rep(0, 7),
               scale = c(
                 rep(year_prior_sd, 1),
                 rep(q_sd, 2),
                 rep(poly_sd, 4)
               )
    )
  )

  # formula2 <- density ~ 0 +
  #   fyear +
  #   survey +
  #   poly(rocky, 2) +
  #   poly(muddy, 2) +
  #   poly(depth_scaled, 2) + (1|vessel_id)
  #
  # priors2 <- sdmTMBpriors(
  #   b = normal(rep(0, 15),
  #              scale = c(
  #                rep(year_prior_sd, 7),
  #                rep(q_sd, 2),
  #                rep(poly_sd, 6)
  #              )
  #   )
  # )
  #
  # formula3 <- density ~ 0 +
  #   fyear +
  #   survey +
  #   poly(rocky, 3) +
  #   poly(muddy, 3) +
  #   poly(depth_scaled, 2) + (1|vessel_id)
  #
  # priors3 <- sdmTMBpriors(
  #   b = normal(rep(0, 17),
  #              scale = c(
  #                rep(year_prior_sd, 7),
  #                rep(q_sd, 2),
  #                rep(poly_sd, 8)
  #              )
  #   )
  # )


  if (st_year_pair){
  hal_formula <- formula1
  ye_formula <- formula1

  hal_priors <- priors1
  # ye_priors <- priors1
  ye_priors <- sdmTMBpriors(
    b = normal(rep(0, 14),
      scale = c(
        rep(year_prior_sd, 7),
        rep(q_sd, 3),
        rep(poly_sd, 4)
      )
    )
  )
  hal_spatiotemporal <- list("off", "ar1")

  ye_spatiotemporal <- list("off", "iid")

  st_time_var <- "year"
  } else {

  hal_formula <- formulaR
  ye_formula <- formulaR

  hal_priors <- priorsR
  ye_priors <- sdmTMBpriors(
    b = normal(rep(0, 8),
               scale = c(
                 rep(year_prior_sd, 1),
                 rep(q_sd, 3),
                 rep(poly_sd, 4)
               )
    )
  )

  hal_spatiotemporal <- list("off", "rw") # if using year_true
  # hal_spatiotemporal <- list("rw", "rw") # if using year_true
  ye_spatiotemporal <- list("off", "rw")

  st_time_var <- "year_true"
  }

} else {
  # fixed_formula <- density ~ 0 +
  #   fyear +
  #   survey +
  #   poly(rocky, 3) +
  #   poly(muddy, 3) +
  #   poly(depth_scaled, 2)


  # fixed_formula <- density ~ 0 + fyear +
  fixed_formula <- density ~ 1 +
    survey +
    rocky +
    muddy +
    poly(depth_scaled, 2)


  hal_formula <- fixed_formula
  ye_formula <- fixed_formula

  # hal_spatiotemporal <- list("off", "iid")
  # hal_spatiotemporal <- list("off", "ar1")
  # hal_spatiotemporal <- list("rw", "rw") # if using year_true
  hal_spatiotemporal <- list("off", "rw") # if using year_true

  priors <- sdmTMBpriors(
    # # matern_s = pc_matern(range_gt = 0.1, sigma_lt = 2),
    # # matern_st = pc_matern(range_gt = 0.1, sigma_lt = 2),
    # b = normal(rep(0, 12),
    #   scale = c(
    #     rep(year_prior_sd, 7),
    b = normal(rep(0, 6),
        scale = c(
          rep(year_prior_sd, 1),
          rep(q_sd, 1),
          rep(poly_sd, 4)
      )
    )
  )
  hal_priors <- priors
  ye_priors <- priors # use same for ye
}

# HAL -------------------------------------------------------
f <- paste0("models/halibut-model-", hal_model, "-tmbfit.rds")

if (!file.exists(f)) {

# hal_spatiotemporal <- list("off", "rw") # if using year_true
m_hal <- sdmTMB(
  hal_formula,
  priors = hal_priors,
  # priors = priors1,
  # weights = d_hal$wt,
  data = d_hal,
  mesh = mesh,
  spatial = "on",
  spatiotemporal = hal_spatiotemporal,
  share_range = FALSE,
  # time = "year",
  time = st_time_var,
  # extra_time = c(2015),
  # silent = FALSE,
  anisotropy = TRUE,
  reml = TRUE,
  family = delta_gamma()
)

saveRDS(m_hal, paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
} else {
m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
}


# hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso"
# m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
#
# hal_model1 <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"
# m_hal1<- readRDS(paste0("models/halibut-model-", hal_model1, "-tmbfit.rds"))
#
# hal_model2 <- "w-deeper-450kn-both-true-year-RW-no-yf-aniso"
# m_hal2 <- readRDS(paste0("models/halibut-model-", hal_model2, "-tmbfit.rds"))
#
# AIC(m_hal, m_hal1, m_hal2)
# # df      AIC
# # m_hal  37 74251.04 # 450kn-true-year-RW-aniso (no binomial st, w fyear_pair)
# # m_hal1 25 74169.28 # 450kn-true-year-RW-aniso (no binomial st, no fyear_pair)
# # m_hal2 27 74101.09 # 450kn-both-true-year-RW-no-yf-aniso


# m_hal_sr <- m_hal
# m_hal2 <- m_hal
# AIC(m_hal_sr, m_hal2) # with REML, Share_range = F is 20 AIC lower
#
# # with REML = F
# m_hal1 <- m_hal
# m_hal2 <- m_hal
# m_hal3 <- m_hal
# AIC(m_hal1, m_hal2, m_hal3)
# df       AIC
# m_hal1 38 -13511.39
# m_hal2 42 -13447.79
# m_hal3 46 -13420.87
# m_hal <- m_hal1

sanity(m_hal)
print(m_hal)

plot_anisotropy(m_hal)
# plot_anisotropy(m_hal, model = 2)
m_hal$sd_report

tidy(m_hal, conf.int = TRUE)
tidy(m_hal, conf.int = TRUE, model = 2)
tidy(m_hal, "ran_pars", conf.int = TRUE)
tidy(m_hal, "ran_pars", conf.int = TRUE, model = 2)

# visreg_delta(m_hal, xvar = "vessel_id", #scale = "response",
#              model = 1)
#
# visreg_delta(m_hal, xvar = "vessel_id", #scale = "response",
#              model = 2)
#
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
G_map <- factor(rep(NA, length(pars$ln_tau_G)))

m_hal_fixed <- update(
  m_hal,
  control = sdmTMBcontrol(
    start = list(
      ln_kappa = pars$ln_kappa,
      ln_H_input = pars$ln_H_input,
      ln_tau_G = pars$ln_tau_G
    ),
    map = list(
      ln_kappa = kappa_map,
      ln_H_input = H_map,
      ln_tau_G = G_map
    )
  ),
  bayesian = TRUE,
  do_fit = FALSE
)


f <- paste0("models/halibut-model-", hal_model, "-stan.rds")

if (!file.exists(f)) {
  saveRDS(m_hal_fixed, paste0("models/halibut-model-", hal_model, "-tmb.rds"))
  m_hal_stan <- tmbstan(
    obj = m_hal_fixed$tmb_obj,
    iter = 5000,
    warmup = 1000,
    chains = 6,
    seed = 192819,
    thin = 5,
    control = list(adapt_delta = 0.9, max_treedepth = 15)
  )
  saveRDS(m_hal_stan, file = f)
} else {
  m_hal_stan <- readRDS(f)
}
m_hal_stan

post <- as.array(m_hal_stan)
# [1] "ln_phi"        "ln_tau_O[1]"   "ln_tau_O[2]"   "ln_tau_E"      "omega_s[1]"    "epsilon_st[1]"
pars <- c("ln_phi", "ln_tau_O[1]", "ln_tau_O[2]",
    "ln_tau_E", "omega_s[1]", "omega_s[2]", "epsilon_st[1]")


regex_pars <- "b_j"
bayesplot::mcmc_pairs(post, pars = pars, off_diag_fun = "hex")
bayesplot::mcmc_trace(post, pars = pars, regex_pars = regex_pars)




plot(m_hal_stan, pars = c("b_j[8]", "b_j2[8]", # non-survey
                         "b_j[9]", "b_j2[9]", # Trawl
                         "b_j[10]", "b_j2[10]", #rocky
                         "b_j[11]", "b_j2[11]", # muddy
                         # "b_j[12]", "b_j2[12]", # depth poly
                         #"b_j[13]", "b_j2[13]", #depth poly
                         "ln_tau_O[1]",
                         "ln_tau_O[2]",
                         "ln_tau_E",
                         "ln_phi" ))

# depth coefficients
plot(m_hal_stan, pars = c("b_j[12]", "b_j2[12]", "b_j[13]","b_j2[13]"
))

# pars_plot <- c("b_j[8]", "b_j2[8]",
#                "b_j[9]", "b_j2[9]",
#                "b_j[10]", "b_j2[10]",
#                "b_j[11]", "b_j2[11]",
#                "b_j[12]", "b_j2[12]",
#                "b_j[13]", "b_j2[13]",
#                "ln_tau_O[1]",
#                "ln_tau_O[2]",
#                "ln_tau_E",
#                "ln_phi")
#
# all_pars <- c(
#   "b_j[1]", "b_j2[1]",
#   "b_j[2]", "b_j2[2]",
#   "b_j[3]", "b_j2[3]",
#   "b_j[4]", "b_j2[4]",
#   "b_j[5]", "b_j2[5]",
#   "b_j[6]", "b_j2[6]",
#   "b_j[7]", "b_j2[7]",
#   "b_j[8]", "b_j2[8]",
#   "b_j[9]", "b_j2[9]",
#   "b_j[10]", "b_j2[10]",
#   "b_j[11]", "b_j2[11]",
#   "b_j[12]", "b_j2[12]",
#   "b_j[13]", "b_j2[13]",
#   "ln_tau_O[1]",
#   "ln_tau_O[2]",
#   "ln_tau_E",
#   "ln_phi")

m_hal_fixed <- readRDS(paste0("models/halibut-model-", hal_model, "-tmb.rds"))



set.seed(19292)
hal_samps <- sdmTMBextra::extract_mcmc(m_hal_stan)
# p0 <- predict(m_hal_fixed, mcmc_samples = hal_samps, model = NA, type = "link")
p1 <- predict(m_hal_fixed, mcmc_samples = hal_samps, model = 1)
p2 <- predict(m_hal_fixed, mcmc_samples = hal_samps, model = 2)


m_hal_fixed$data$present <- ifelse(m_hal_fixed$data$density > 0, 1, 0)
mu <- plogis(apply(p1, 1, mean))
q <- qres_binomial_(y = m_hal_fixed$data$present, mu)
qqnorm(q);qqline(q)

post <- extract(m_hal_stan)
pos <- which(m_hal_fixed$data$present == 1)
dpos <- m_hal_fixed$data[pos, ]
p2pos <- p2[pos, ]
mu <- apply(exp(p2pos), 1, mean)

q <- qres_gamma_(y = dpos$density, mu, phi = mean(exp(post$ln_phi)))
qqnorm(q);qqline(q)

s_hal <- simulate(m_hal_fixed, mcmc_samples = hal_samps, nsim = 50L)

bayesplot::pp_check(
  m_hal_fixed$data$density,
  yrep = t(s_hal),
  fun = bayesplot::ppc_dens_overlay
)


bayesplot::pp_check(m_hal_fixed$data$density[pos], t(s_hal[pos,1:50]), bayesplot::ppc_dens_overlay) +
  scale_x_log10(limits = c(0.001,1800)) +
  ylab("Frequency") + xlab("Halibut density when present (observed and simulated)") +
  ggsidekick::theme_sleek()

ggsave(paste0("figs/bayes-pp-check-hal-", hal_model, ".png"), height = 4, width = 5.5)

# pos2 <- which(d_hal$present == 1 & d_hal$survey != "NON-SURVEY")
# bayesplot::pp_check(d_hal$density[pos2], t(s[pos2,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10()

# just model 1
s <- simulate(m_hal_fixed, mcmc_samples = hal_samps, nsim = 50L, model = 1)
table(s[,1])/sum(table(s[,1]))
table(s[,2])/sum(table(s[,2]))
table(s[,3])/sum(table(s[,3]))
table(m_hal_fixed$data$present)/sum(table(m_hal_fixed$data$present))
# s2 <- simulate(m_hal_fixed, mcmc_samples = hal_samps, nsim = 50L, model = 2)




# YE -------------------------------------------------------
f <- paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds")

if (!file.exists(f)) {

m_ye <- sdmTMB(
  ye_formula,
  priors = ye_priors,
  # formula3,
  # priors = priors3,
  # weights = d_ye$wt,
  data = d_ye,
  mesh = mesh2,
  share_range = FALSE,
  # time = "year",
  spatial = "on",
  time = st_time_var,
  # spatial = list("on","off"),
  # spatiotemporal = list("off", "iid"),
  spatiotemporal = ye_spatiotemporal,
  # spatiotemporal = list("rw", "rw"),
  # extra_time = c(2015),
  # silent = FALSE,
  reml = TRUE,
  anisotropy = TRUE,
  family = delta_gamma()
  # family = tweedie()
)

saveRDS(m_ye, paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))
} else {
m_ye <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))
}

m_ye
sanity(m_ye)
m_ye$sd_report
plot_anisotropy(m_ye)
# plot_anisotropy(m_ye, model = 2)
tidy(m_ye, conf.int = TRUE)
tidy(m_ye, conf.int = TRUE, model = 2)
tidy(m_ye, "ran_pars", conf.int = TRUE)
tidy(m_ye, "ran_pars", conf.int = TRUE, model = 2)


library(patchwork)
library(grid)

(p1 <- plot_anisotropy(m_ye) +
    # xlim(-1.48, 1.48) +
    # ylim(-1.48, 1.48) +
    coord_cartesian(xlim = c(-1.48, 1.48), ylim = c(-1.48, 1.48)) +
    # coord_cartesian(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8)) +
    # xlim(-2.48, 2.48) +
    # ylim(-2.48, 2.48) +
    ggtitle("(a) Yelloweye Rockfish") +
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          title = element_text(size = 10))
  )

(p2 <- plot_anisotropy(m_hal) +
    # xlim(-1.48, 1.48) +
    # ylim(-1.48, 1.48) +
    coord_cartesian(xlim = c(-1.48, 1.48), ylim = c(-1.48, 1.48)) +
    # coord_cartesian(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8)) +
    # xlim(-2.48, 2.48) +
    # ylim(-2.48, 2.48) +
    ggtitle("(b) Pacific Halibut") +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          title = element_text(size = 10))
  )

ty <- textGrob("Latitude in 100 kms", rot=90)
tx <- textGrob("Longitude in 100 kms", hjust = 0.75)

{wrap_elements(ty) + p1 + p2 + plot_layout(widths = c(0.85,10, 10))}/tx + plot_layout(heights=c(30,1))

ggsave(file.path("figs", paste0("anisotropy-", hal_model, ye_model, ".png")), width= 6, height = 2.85)


# hal_model0 <- "w-deeper-500kn-delta-AR1-aniso-may23"
# m_hal0 <- readRDS(paste0("models/halibut-model-", hal_model0, "-tmbfit.rds"))
#
# hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso"
# m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
# AIC(m_hal, m_hal0)

hal_model1 <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"
m_hal1<- readRDS(paste0("models/halibut-model-", hal_model1, "-tmbfit.rds"))

hal_model2 <- "w-deeper-450kn-both-true-year-RW-no-yf-aniso"
m_hal2 <- readRDS(paste0("models/halibut-model-", hal_model2, "-tmbfit.rds"))

AIC(m_hal, m_hal1, m_hal2)
# df      AIC
# m_hal  37 74251.04 # 450kn-true-year-RW-aniso (no binomial st, w fyear_pair)
# m_hal1 25 74169.28 # 450kn-true-year-RW-aniso (no binomial st, no fyear_pair)
# m_hal2 27 74101.09 # 450kn-both-true-year-RW-no-yf-aniso

# ye_model0 <- "w-deeper-all-yrs-500kn-delta-iid-aniso"
# m_ye0 <- readRDS(paste0("models/yelloweye-model-", ye_model0, "-tmbfit.rds"))
#
# ye_model <- "w-deeper-all-yrs-450kn-true-year-RW-aniso"
# m_ye <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmbfit.rds"))
# AIC(m_ye, m_ye0)

ye_model1 <- "w-deeper-450kn-delta-true-year-RW-no-yf-aniso"
m_ye1 <- readRDS(paste0("models/yelloweye-model-", ye_model1, "-tmbfit.rds"))

ye_model2 <- "w-deeper-450kn-both-true-year-RW-no-yf-aniso"
m_ye2 <- readRDS(paste0("models/yelloweye-model-", ye_model2, "-tmbfit.rds"))

AIC(m_ye, m_ye1, m_ye2)
# df      AIC
# m_ye  39 33215.57
# m_ye1 27 33156.62
# m_ye2 29 33122.72
# visreg_delta(m_ye, xvar = "vessel_id", scale = "response",
#              model = 1)
# visreg_delta(m_ye, xvar = "vessel_id", scale = "response",
#              model = 2)
#
# visreg_delta(m_ye, xvar = "rocky", model = 1,
#              scale = "response", nn = 10)
# visreg_delta(m_ye, xvar = "rocky", model = 2,
#                scale = "response", nn = 10)
#
# visreg_delta(m_ye, xvar = "muddy", model = 1,
#              scale = "response", nn = 10)
# visreg_delta(m_ye, xvar = "muddy", model = 2,
#                scale = "response", nn = 10)
#
# visreg_delta(m_ye, xvar = "depth_scaled", model = 1,
#              scale = "response", nn = 10)
# visreg_delta(m_ye, xvar = "depth_scaled", model = 2,
#                scale = "response", nn = 10)

pars <- sdmTMB:::get_pars(m_ye)
kappa_map <- factor(rep(NA, length(pars$ln_kappa)))
H_map <- factor(rep(NA, length(pars$ln_H_input)))
G_map <- factor(rep(NA, length(pars$ln_tau_G)))

m_ye_fixed <- update(
  m_ye,
  control = sdmTMBcontrol(
    start = list(
      ln_kappa = pars$ln_kappa,
      ln_H_input = pars$ln_H_input,
      ln_tau_G = pars$ln_tau_G
    ),
    map = list(
      ln_kappa = kappa_map,
      ln_H_input = H_map,
      ln_tau_G = G_map
    )
  ),
  bayesian = TRUE,
  do_fit = FALSE
)


f <- paste0("models/yelloweye-model-", ye_model, "-stan.rds")

if (!file.exists(f)) {

  saveRDS(m_ye_fixed, paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))
  m_ye_stan <- tmbstan(
    obj = m_ye_fixed$tmb_obj,
    iter = 5000,
    warmup = 1000,
    chains = 6,
    seed = 192819,
    thin = 5,
    control = list(adapt_delta = 0.9, max_treedepth = 15)
  )
  saveRDS(m_ye_stan, file = f)
} else {
  m_ye_stan <- readRDS(f)
}
m_ye_stan

# plot(m_ye_stan, plotfun = "rhat")
# plot(m_ye_stan)
# plot(m_hal_stan)


# plot(m_ye_stan, pars = c("b_j[8]","b_j[9]","b_j[10]", "b_j[11]", "b_j[12]",#"b_j[13]","b_j[14]",
#                          "b_j2[8]","b_j2[9]","b_j2[10]", "b_j2[11]", "b_j2[12]",#"b_j2[13]","b_j2[14]",
#                          "ln_tau_O[1]",
#                          "ln_tau_O[2]",
#                          "ln_tau_E",
#                          "ln_phi" ))

#
plot(m_ye_stan, pars = c("b_j[8]", "b_j2[8]",
                         "b_j[9]", "b_j2[9]",
                         "b_j[10]", "b_j2[10]",
                         "b_j[11]", "b_j2[11]",
                         "b_j[12]", "b_j2[12]",
                         #"b_j[13]", "b_j[14]",
                         #"b_j2[13]", "b_j2[14]",
                         "ln_tau_O[1]",
                         "ln_tau_O[2]",
                         "ln_tau_E",
                         "ln_phi" ))

# depth coefficients
plot(m_ye_stan, pars = c("b_j[13]","b_j[14]",
                         "b_j2[13]","b_j2[14]"
                         ))

pars_plot <- c("b_j[8]", "b_j2[8]",
               "b_j[9]", "b_j2[9]",
               "b_j[10]", "b_j2[10]",
               "b_j[11]", "b_j2[11]",
               "b_j[12]", "b_j2[12]",
               "b_j[13]", "b_j2[13]",
               "b_j[14]", "b_j2[14]",
               "ln_tau_O[1]",
               "ln_tau_O[2]",
               "ln_tau_E",
               "ln_phi")

all_pars <- c(
  "b_j[1]", "b_j2[1]",
  "b_j[2]", "b_j2[2]",
  "b_j[3]", "b_j2[3]",
  "b_j[4]", "b_j2[4]",
  "b_j[5]", "b_j2[5]",
  "b_j[6]", "b_j2[6]",
  "b_j[7]", "b_j2[7]",
  "b_j[8]", "b_j2[8]",
  "b_j[9]", "b_j2[9]",
  "b_j[10]", "b_j2[10]",
  "b_j[11]", "b_j2[11]",
  "b_j[12]", "b_j2[12]",
  "b_j[13]", "b_j2[13]",
  "b_j[14]", "b_j2[14]",
  "ln_tau_O[1]",
  "ln_tau_O[2]",
  "ln_tau_E",
  "ln_phi")

# bayesplot::mcmc_trace(m_ye_stan, pars = all_pars)
# bayesplot::mcmc_pairs(m_ye_stan, pars = pars_plot)


m_ye_fixed <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))


set.seed(19292)
ye_samps <- sdmTMBextra::extract_mcmc(m_ye_stan)
p1_ye <- predict(m_ye_fixed, mcmc_samples = ye_samps, model = 1)
p2_ye <- predict(m_ye_fixed, mcmc_samples = ye_samps, model = 2)


m_ye_fixed$data$present <- ifelse(m_ye_fixed$data$density > 0, 1, 0)
mu <- plogis(apply(p1_ye, 1, mean))
q <- qres_binomial_(y = m_ye_fixed$data$present, mu)
qqnorm(q);qqline(q)

post <- extract(m_ye_stan)
pos <- which(m_ye_fixed$data$present == 1)
dpos <- m_ye_fixed$data[pos, ]
p2pos <- p2_ye[pos, ]

i <- 1
q <- qres_gamma_(y = dpos$density, exp(p2pos[,i,drop=TRUE]),
                 phi = exp(post$ln_phi)[i])
qqnorm(q);qqline(q)

mu <- apply(exp(p2pos), 1, mean)
q <- qres_gamma_(y = dpos$density, mu, phi = mean(exp(post$ln_phi)))
qqnorm(q);qqline(q)

post <- as.array(m_ye_stan)
pars <- c("ln_phi", "ln_tau_O[1]", "ln_tau_O[2]", "omega_s[1]", "omega_s[2]", "ln_tau_E", "epsilon_st[1]")
regex_pars <- "b_j"
bayesplot::mcmc_pairs(post, pars = pars, off_diag_fun = "hex")
bayesplot::mcmc_trace(post, pars = pars, regex_pars = regex_pars)


s_ye <- simulate(m_ye_fixed, mcmc_samples = ye_samps, nsim = 50L)
bayesplot::pp_check(
  m_ye_fixed$data$density,
  yrep = t(s_ye),
  fun = bayesplot::ppc_dens_overlay
) #+ xlim(0,5)

pos2 <- which(m_ye_fixed$data$present == 0 & m_ye_fixed$data$survey != "NON-SURVEY")
bayesplot::pp_check(m_ye_fixed$data$density[pos2], t(s_ye[pos2,1:50]), bayesplot::ppc_dens_overlay) #+ scale_x_log10()

pos2 <- which(m_ye_fixed$data$present == 1 & m_ye_fixed$data$survey == "NON-SURVEY")
bayesplot::pp_check(m_ye_fixed$data$density[pos2], t(s_ye[pos2,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10()

pos2 <- which(m_ye_fixed$data$present == 1 & m_ye_fixed$data$survey != "NON-SURVEY")
bayesplot::pp_check(m_ye_fixed$data$density[pos2], t(s_ye[pos2,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10()

pos <- which(m_ye_fixed$data$present == 1)
bayesplot::pp_check(m_ye_fixed$data$density[pos], t(s_ye[pos,1:50]), bayesplot::ppc_dens_overlay) + scale_x_log10() + ylab("Frequency") + xlab("Yelloweye density when present (observed and simulated)") + ggsidekick::theme_sleek()

ggsave(paste0("figs/bayes-pp-check-ye-", ye_model, ".png"), height = 4, width = 5.5)


s <- simulate(m_ye_fixed, mcmc_samples = ye_samps, nsim = 50L, model = 1)
table(s[,1])/sum(table(s[,1]))
table(s[,2])/sum(table(s[,2]))
table(s[,3])/sum(table(s[,3]))
table(d_ye$present)/sum(table(d_ye$present))

