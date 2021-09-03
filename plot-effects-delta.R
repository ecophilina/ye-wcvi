library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
library(ggsidekick)

m1 <- readRDS(file = "models/yelloweye-hybrid-hbll-mw-rocky-muddy-400kn.rds")
# m2 <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn-tweedie.rds")
m2 <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn-binomial.rds")
m3 <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn-gamma.rds")


get_effects <- function(m, newdata = pd, logit = F, sims = NULL){
  if (!is.null(sims)){
    p <- predict(m, newdata = newdata, se_fit = F, re_form = NA, sims = 10000)

    out <- apply(p, 1, function(x) {
      data.frame(
        est = median(x),
        lwr = stats::quantile(x, probs = (1 - 0.95) / 2),
        upr = stats::quantile(x, probs = 1 - (1 - 0.95) / 2),
        est_se = stats::sd(x)
      )
    })
    out <- do.call("rbind", out)
    out <- out[, c("est", "lwr", "upr", "est_se"
    ), drop = FALSE]

    p <- bind_cols(pd, out)

    if (logit){
      p <- p %>% mutate(
        Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
        prediction = plogis(est),
        lowCI = plogis(lwr),
        highCI = plogis(upr))
    } else{
      p <- p %>% mutate(
        Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
        prediction = exp(est) ,
        lowCI =  exp(lwr),
        highCI =  exp(upr))
    }

  } else {
    p <- predict(m, newdata = newdata, se_fit = TRUE, re_form = NA)

    if (logit){
      p <- p %>% mutate(
        Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
        prediction = plogis(est),
        lowCI = plogis(est - 1.96 * est_se),
        highCI = plogis(est + 1.96 * est_se))
    } else{
      p <- p %>% mutate(
        Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
        prediction = exp(est) ,
        lowCI =  exp(est - 1.96 * est_se),
        highCI =  exp(est + 1.96 * est_se))
    }
  }
  p
}


# depth predictions
pd <- expand.grid(
  depth_scaled = seq(min(m$data$depth_scaled),
    max(m$data$depth_scaled), length.out = 100),
  year = unique(m$data$year)
)

pd$muddy <- mean(m$data$muddy)
pd$rocky <- mean(m$data$rocky)
pd$mixed <- mean(m$data$mixed) # not actually in these models
pd$survey <- "HBLL"

p1 <- get_effects(m1)
saveRDS(p1, here::here(paste0("data-generated/ye-tv-depth.rds")))
p2 <- get_effects(m2, logit = T)
saveRDS(p2, here::here(paste0("data-generated/halibut-depth-bin.rds")))
p3 <- get_effects(m3)
saveRDS(p3, here::here(paste0("data-generated/halibut-depth-pos.rds")))

# rocky predictions
pd <- expand.grid(
  rocky = seq(min(m$data$rocky),
    max(m$data$rocky), length.out = 20),
  year = unique(m$data$year)
)
pd$survey <- "HBLL"
pd$depth_scaled <- -1
pd$muddy <- 0
pd$mixed <- 0
pd$survey <- "HBLL"

p1 <- get_effects(m1)
saveRDS(p1, here::here(paste0("data-generated/ye-tv-rocky.rds")))
p2 <- get_effects(m2, logit = T)
saveRDS(p2, here::here(paste0("data-generated/halibut-rocky-bin.rds")))
p3 <- get_effects(m3)
saveRDS(p3, here::here(paste0("data-generated/halibut-rocky-pos.rds")))

# muddy predictions
pd <- expand.grid(
  muddy = seq(min(m$data$muddy),
    max(m$data$muddy), length.out = 20),
  # survey = unique(m$data$survey)
  year = unique(m$data$year)
)
pd$survey <- "HBLL"
# pd$year <- 2020
pd$depth_scaled <- -1
pd$rocky <- 0
pd$mixed <- 0

p1 <- get_effects(m1)
saveRDS(p1, here::here(paste0("data-generated/ye-tv-muddy.rds")))
p2 <- get_effects(m2, logit = T)
saveRDS(p2, here::here(paste0("data-generated/halibut-muddy-bin.rds")))
p3 <- get_effects(m3)
saveRDS(p3, here::here(paste0("data-generated/halibut-muddy-pos.rds")))


# depth plots
p1 <- readRDS( here::here(paste0("data-generated/ye-tv-depth.rds")))
p2 <- readRDS( here::here(paste0("data-generated/halibut-depth-bin.rds")))%>% rename(prediction = Probability)
p3 <- readRDS( here::here(paste0("data-generated/halibut-depth-pos.rds")))%>% rename(prediction = Biomass)

p1$Species <- "YE"
p2$Species <- "Halibut present"
p3$Species <- "Halibut biomass"
p <- bind_rows(p2, p1) %>% bind_rows(p3)
p$Model <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"), labels =  c("Tweedie", "Binomial", "Gamma"))
p$Species <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"), labels =  c("YE", "Landable Halibut presence", "Landable Halibut biomass (if present)"))

dplot <- ggplot(p,
  aes(Depth, prediction,
    ymin = lowCI,
    ymax = highCI,
    group= year,
    fill= Model, colour = Model
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(aes(alpha=year), size = 0.75) +
  scale_fill_viridis_d(name = "Model type", option = "C", end = 0.7, direction = -1) +
  scale_colour_viridis_d(name = "Model type", option = "C", end = 0.7, direction = -1) +
  ylab("Landable Halibut (kg/ha)        Halibut (probability present)             YE Biomass (kg/ha) ") +
  facet_wrap(~Species,
    # scales = "free_y",
    nrow = 3) +
  guides(alpha = "none") +
  gfplot::theme_pbs() +
  # theme(legend.position = "none")
  theme(legend.position = c(0.7, 0.9), strip.text.x = element_blank())

# ggsave("figs/depth-effect-delta.png", width = 2.25, height = 6)


# rocky plots
p1 <- readRDS( here::here(paste0("data-generated/ye-tv-rocky.rds")))
p2 <- readRDS( here::here(paste0("data-generated/halibut-rocky-bin.rds")))
p3 <- readRDS( here::here(paste0("data-generated/halibut-rocky-pos.rds")))

p1$Species <- "YE"
p2$Species <- "Halibut present"
p3$Species <- "Halibut biomass"
p <- bind_rows(p2, p1) %>% bind_rows(p3)
p$Model <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"),
  labels =  c("Tweedie", "Binomial", "Gamma"))
p$Species <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"),
  labels =  c("YE", "Landable Halibut presence", "Landable Halibut biomass (if present)"))

rplot <- ggplot(p,
  aes(rocky, prediction,
    ymin = lowCI,
    ymax = highCI,
    group= year,
    fill= Species, colour = Species
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(aes(alpha=year), size = 0.75) +
  scale_fill_viridis_d(name = "Model", option = "C", end = 0.7, direction = -1) +
  scale_colour_viridis_d(name = "Model", option = "C", end = 0.7, direction = -1) +
  facet_wrap(~Species,
    # scales = "free_y",
    nrow = 3) +
  guides(alpha = "none") +
  ylab("Biomass (kg/ha)                     Probability                       Biomass (kg/ha) ") +
  xlab("Proportion rocky") +
  gfplot::theme_pbs() +
  # theme(legend.position = c(0.7, 0.9))
  theme(legend.position = "none",
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    strip.text.x = element_blank())

# muddy plots
p1 <- readRDS( here::here(paste0("data-generated/ye-tv-muddy.rds")))
p2 <- readRDS( here::here(paste0("data-generated/halibut-muddy-bin.rds")))
p3 <- readRDS( here::here(paste0("data-generated/halibut-muddy-pos.rds")))

p1$Species <- "YE"
p2$Species <- "Halibut present"
p3$Species <- "Halibut biomass"
p <- bind_rows(p2, p1) %>% bind_rows(p3)
p$Model <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"),
  labels =  c("Tweedie", "Binomial", "Gamma"))
p$Species <- ordered(p$Species, levels =  c("YE", "Halibut present", "Halibut biomass"),
  labels =  c("YE", "Landable Halibut presence", "Landable Halibut biomass (if present)"))

mplot <- ggplot(p,
  aes(muddy, prediction,
    ymin = lowCI,
    ymax = highCI,
    group= year,
    fill= Species, colour = Species
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(aes(alpha=year), size = 0.75) +
  scale_fill_viridis_d(name = "Model", option = "C", end = 0.7, direction = -1) +
  scale_colour_viridis_d(name = "Model", option = "C", end = 0.7, direction = -1) +
  facet_wrap(~Species,
    # scales = "free_y",
    nrow = 3) +
  guides(alpha = "none") +
  # ylab("Biomass (kg/ha)                     Probability                       Biomass (kg/ha) ") +
  xlab("Proportion muddy") +
  gfplot::theme_pbs() +
  # theme(legend.position = c(0.7, 0.9))
  theme(legend.position = "none",
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    strip.text.x = element_blank())


dplot + rplot + mplot + patchwork::plot_layout(ncol = 3)

ggsave("figs/effects-delta-allfixed.png", width = 8, height = 7)



# # mixed predictions...
# m <- readRDS(file = "models/yelloweye-hybrid-RW-w-rocky-mixed-400kn.rds")
#
# pd <- expand.grid(
#   mixed = seq(min(m$data$mixed),
#     max(m$data$mixed), length.out = 20),
#   survey = unique(m$data$survey)
#   # year = unique(m$data$year)
# )
# # pd$survey <- "HBLL"
# pd$year <- 2008
#
# pd$depth_scaled <- min(m$data$depth_scaled)
# pd$depth_scaled2 <- pd$depth_scaled^2
# pd$muddy <- 0
# pd$rocky <- 0
# # pd$mixed <- mean(m$data$mixed)


