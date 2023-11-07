library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
library(ggsidekick)

# cols <- c(
#   "red",
#   #"darkorchid4",
#   "deeppink4",
#   "deepskyblue4"
#   # "cadetblue3"
# )

# # load models if 03 not just run
ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-may23"
hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"

f <- paste0("models/halibut-model-", hal_model, "-stan.rds")
if (file.exists(f)) {
  m1 <- readRDS(paste0("models/halibut-model-", hal_model, "-tmb.rds"))
  m_hal_stan <- readRDS(f)
}

f2 <- paste0("models/yelloweye-model-", ye_model, "-stan.rds")
if (file.exists(f2)) {
  m2 <- readRDS(paste0("models/yelloweye-model-", ye_model, "-tmb.rds"))
  m_ye_stan <- readRDS(f2)
}


get_effects <- function(m,
                        newdata = pd,
                        tmbstan_model = NULL,
                        sims = NULL,
                        logit = FALSE,
                        type = "response") {

  if (!is.null(sims)|!is.null(tmbstan_model)) {

    post <- sdmTMBextra::extract_mcmc(tmbstan_model)
    set.seed(102838)
    downsampled <- sample(seq(1, ncol(post)), 1000)
    post <- post[,downsampled]

    p <- predict(m,  mcmc_samples = post, newdata = newdata,
                 se_fit = F, re_form = NA, type = type, nsims = 10000)

    out <- apply(p, 1, function(x) {
      data.frame(
        est = median(x),
        lwr = stats::quantile(x, probs = (1 - 0.95) / 2),
        upr = stats::quantile(x, probs = 1 - (1 - 0.95) / 2),
        est_se = stats::sd(x)
      )
    })
    out <- do.call("rbind", out)
    out <- out[, c("est", "lwr", "upr", "est_se"), drop = FALSE]

    p <- bind_cols(pd, out)

    if (type != "response") {
      stop("this needs updating to work with stan and type not response")
  }
  } else {
    # this is non-stan and non-sim version...
    if (type != "response") {
    stop("This needs updating to work with non-stan and type not response")
    } else{
    p <- predict(m, newdata = newdata, type = type, se_fit = TRUE, re_form = NA)
  }
  }
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  prediction = est,
  lowCI = lwr,
  highCI = upr
)
  p
}

m <- m1

# depth predictions
pd <- expand.grid(
  depth_scaled = seq(min(m$data$depth_scaled),
    max(m$data$depth_scaled),
    length.out = 50
  ),
  fyear = as.factor(max(unique(m$data$year)))
)

pd$muddy <- mean(m$data$muddy)
pd$rocky <- mean(m$data$rocky)
pd$mixed <- mean(m$data$mixed) # not actually in these models
pd$survey <- as.factor("HBLL")
pd$vessel_id <- as.factor("survey")
pd$year <- max(unique(m$data$year))

p1 <- get_effects(m1, tmbstan_model = m_hal_stan)
p1 %>% ggplot(., aes(Depth, est, ymin = lwr, ymax = upr)) + geom_line() + geom_ribbon(alpha= 0.1)
saveRDS(p1, here::here(paste0("data-generated/hal-depth.rds")))

p2 <- get_effects(m2, tmbstan_model = m_ye_stan)
p2 %>% ggplot(., aes(Depth, est, ymin = lwr, ymax = upr)) + geom_line() + geom_ribbon(alpha= 0.1)
saveRDS(p2, here::here(paste0("data-generated/ye-depth.rds")))

# rocky predictions
pd <- expand.grid(
  rocky = seq(min(m$data$rocky),
    max(m$data$rocky),
    length.out = 20
  ),
  fyear = as.factor(max(unique(m$data$year)))
)
pd$depth_scaled <- -1
pd$muddy <- 0
pd$mixed <- 0
pd$survey <- as.factor("HBLL")
pd$vessel_id <- as.factor("survey")
pd$year <- max(unique(m$data$year))

p1 <- get_effects(m1, tmbstan_model = m_hal_stan)
saveRDS(p1, here::here(paste0("data-generated/hal-rocky.rds")))
p2 <- get_effects(m2, tmbstan_model = m_ye_stan)
saveRDS(p2, here::here(paste0("data-generated/ye-rocky.rds")))

# muddy predictions
pd <- expand.grid(
  muddy = seq(min(m$data$muddy),
    max(m$data$muddy),
    length.out = 20
  ),
  fyear = as.factor(max(unique(m$data$year)))
)
pd$survey <- as.factor("HBLL")
pd$vessel_id <- as.factor("survey")
pd$year <- max(unique(m$data$year))
# pd$year <- 2020
pd$depth_scaled <- -1
pd$rocky <- 0
pd$mixed <- 0

p1 <- get_effects(m1, tmbstan_model = m_hal_stan)
saveRDS(p1, here::here(paste0("data-generated/hal-muddy.rds")))
p2 <- get_effects(m2, tmbstan_model = m_ye_stan)
saveRDS(p2, here::here(paste0("data-generated/ye-muddy.rds")))


# facet plot of effects
#depth
p1 <- readRDS(here::here(paste0("data-generated/ye-depth.rds")))
p2 <- readRDS(here::here(paste0("data-generated/hal-depth.rds")))
p1$Species <- "Yelloweye Rockfish"
p2$Species <- "Landable Pacific Halibut"
p <- bind_rows(p2, p1)
p$Species <- ordered(p$Species, levels = c("Yelloweye Rockfish", "Landable Pacific Halibut"))
p$var <- "Depth (m)"
p$x <- p$Depth

#muddy
p3 <- readRDS(here::here(paste0("data-generated/ye-muddy.rds")))
p4 <- readRDS(here::here(paste0("data-generated/hal-muddy.rds")))
p3$Species <- "Yelloweye Rockfish"
p4$Species <- "Landable Pacific Halibut"
p3$var <- "Proportion muddy"
p4$var <- "Proportion muddy"
p3$x <- p3$muddy
p4$x <- p4$muddy
p <- bind_rows(p, p4, p3)

#rocky
p5 <- readRDS(here::here(paste0("data-generated/ye-rocky.rds")))
p6 <- readRDS(here::here(paste0("data-generated/hal-rocky.rds")))
p5$Species <- "Yelloweye Rockfish"
p6$Species <- "Landable Pacific Halibut"
p5$var <- "Proportion rocky"
p6$var <- "Proportion rocky"
p5$x <- p5$rocky
p6$x <- p6$rocky
p <- bind_rows(p, p5, p6)

p$Species <- ordered(p$Species, levels = c("Yelloweye Rockfish", "Landable Pacific Halibut"))


dplot <- ggplot(
  p,
  aes(x, prediction,
    ymin = lowCI,
    ymax = highCI,
    group = year
  )
) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(aes(alpha = year), linewidth = 0.75) +
  # ylab("Biomass (kg/km2)") +
  ylab("") +
  xlab("") +
  facet_grid(Species~var,
    scales = "free",
    switch = "both"
  ) +
  scale_x_continuous(n.breaks = 4) +
  guides(alpha = "none") +
  gfplot::theme_pbs() +
  theme(legend.position = "none",
        strip.placement = "outside")

dplot <- egg::tag_facet(dplot)

ggsave("figs/effects-delta-iid-ar1-ansio.png", width = 6, height = 4)
