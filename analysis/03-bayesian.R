library(dplyr)
library(sdmTMB)
library(tmbstan)
library(bayesplot)
library(visreg)
options(scipen=999)
options(mc.cores = parallel::detectCores())

# load misc custom functions
source("analysis/functions.R")

# include_cc <- FALSE
include_cc <- TRUE # including non-survey catches
st_year_pair <- TRUE # biennial spatiotemporal fields
set_knots <- 500

# latitude_cutoff <- 52.15507 # include all of HBLL S
# final analysis focused only on west coast vancouver island
# this allows a more consistent effect of anisotropy
latitude_cutoff <- 51 # include only west coast Vancouver Island

if (include_cc) {
  ## only include well sampled depths
  # hal_model <- "w-good-depths-500kn-delta-AR1-aniso"
  # ye_model <- "w-good-depths-500kn-delta-iid-aniso"

  ## expand to include more depths to better answer depth question
  # hal_model <- "w-deeper-500kn-delta-AR1-aniso-dec22"
  # ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-dec22

  ## rerun to update figures for submission
  ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-may23"
  hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"

} else {

  # hal_model <- "rocky-muddy-400kn-delta-IID-aniso"
  # ye_model <- "rocky-muddy-400kn-delta-spatial-aniso"
  # hal_model <- "w-deeper-500kn-delta-AR1-aniso-no-com"
  hal_model <- "w-deeper-450kn-delta-true-year-RW-aniso-no-com"
  ye_model <- "w-deeper-all-yrs-450kn-true-year-iid-aniso-no-com"
  hal_model <- "w-deeper-6km-both-true-year-RW-no-yf-aniso-no-com"
  ye_model <- "w-deeper-200kn-both-true-year-RW-no-yf-aniso-no-com"

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
    mutate(
      fyear = as.factor(year),
      fyear_true = as.factor(year_true),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  d_ye <- d_ye %>%
    filter(year %in% years) %>%
    filter(!is.na(vessel_id)) %>%
    filter(survey != "NON-SURVEY" | (dist_km_fished < 3 & dist_km_fished > 0.2)) %>%
    mutate(
      survey = ifelse(survey == "NON-SURVEY" & year > 2015, "RESTRICTED", survey),
      fyear = as.factor(year),
      fyear_true = as.factor(year_true),
      vessel_id = ifelse(survey != "NON-SURVEY", "survey", vessel_id),
      vessel_id = as.factor(vessel_id),
      survey = as.factor(survey),
      wt = ifelse(survey != "NON-SURVEY", 1, 1e-8))

  mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = set_knots)

  mesh$mesh$n

  d1 <- filter(d_hal, survey != "NON-SURVEY")
  d2 <- filter(d_hal, survey == "NON-SURVEY")
  plot(mesh$mesh, asp = 1, main = "")
  points(d2$X, d2$Y, pch = ".", col = "red")
  points(d1$X, d1$Y, pch = ".", col = "blue")

  mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = set_knots)
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

  mesh <- make_mesh(d_hal, c("X", "Y"), n_knots = 200)
  mesh$mesh$n
  plot(mesh$mesh, asp = 1, main = "")
  points(d_hal$X, d_hal$Y, pch = ".", col = "blue")

  mesh2 <- make_mesh(d_ye, c("X", "Y"), n_knots = 200)
}

unique(d_hal$survey)
filter(d_hal, survey == "NON-SURVEY") %>% nrow()
filter(d_hal, survey != "NON-SURVEY") %>% nrow()


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

  if (st_year_pair){

  st_time_var <- "year"

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


  } else {

  # if using year_true
  st_time_var <- "year_true"

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

  # hal_spatiotemporal <- list("rw", "rw")
  hal_spatiotemporal <- list("off", "rw")
  ye_spatiotemporal <- list("off", "rw")

  }

} else {
  ## without commercial catch data
  fixed_formula <- density ~ 0 + fyear +
  # fixed_formula <- density ~ 1 + # use if testing RW option and year_true
    survey +
    rocky +
    muddy +
    poly(depth_scaled, 2)


  hal_formula <- fixed_formula
  ye_formula <- fixed_formula

  # hal_spatiotemporal <- list("off", "iid")
  hal_spatiotemporal <- list("off", "ar1")
  # hal_spatiotemporal <- list("off", "rw") # if using year_true

  priors <- sdmTMBpriors(
    # # # can't use priors with anisotropy
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

m_hal <- sdmTMB(
  hal_formula,
  priors = hal_priors,
  # weights = d_hal$wt,
  data = d_hal,
  mesh = mesh,
  spatial = "on",
  spatiotemporal = hal_spatiotemporal,
  share_range = FALSE,
  time = st_time_var,
  # extra_time = c(2015), #if using year_true
  # silent = FALSE,
  anisotropy = TRUE,
  reml = TRUE,
  family = delta_gamma()
)

saveRDS(m_hal, paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
} else {
m_hal <- readRDS(paste0("models/halibut-model-", hal_model, "-tmbfit.rds"))
}

sanity(m_hal)
print(m_hal)

plot_anisotropy(m_hal)
# plot_anisotropy(m_hal, model = 2)
m_hal$sd_report

tidy(m_hal, conf.int = TRUE)
tidy(m_hal, conf.int = TRUE, model = 2)
tidy(m_hal, "ran_pars", conf.int = TRUE)
tidy(m_hal, "ran_pars", conf.int = TRUE, model = 2)

## very slow so not always run
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
  # weights = d_ye$wt,
  data = d_ye,
  mesh = mesh2,
  share_range = FALSE,
  spatial = "on",
  time = st_time_var,
  spatiotemporal = ye_spatiotemporal,
  # extra_time = c(2015),
  # silent = FALSE,
  reml = TRUE,
  anisotropy = TRUE,
  family = delta_gamma()
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

# pars_plot <- c("b_j[8]", "b_j2[8]",
#                "b_j[9]", "b_j2[9]",
#                "b_j[10]", "b_j2[10]",
#                "b_j[11]", "b_j2[11]",
#                "b_j[12]", "b_j2[12]",
#                "b_j[13]", "b_j2[13]",
#                "b_j[14]", "b_j2[14]",
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
#   "b_j[14]", "b_j2[14]",
#   "ln_tau_O[1]",
#   "ln_tau_O[2]",
#   "ln_tau_E",
#   "ln_phi")
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

