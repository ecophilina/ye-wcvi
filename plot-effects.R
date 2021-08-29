library(dplyr)
library(ggplot2)
library(tidyr)
library(sdmTMB)
library(ggsidekick)

m <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn.rds")
m <- readRDS(file = "models/yelloweye-hybrid-hbll-mw-rocky-muddy-400kn.rds")

# depth plots
pd <- expand.grid(
  depth_scaled = seq(min(m$data$depth_scaled),
    max(m$data$depth_scaled), length.out = 100),
  year = unique(m$data$year)
)

# pd$depth_scaled2 <- pd$depth_scaled^2
pd$muddy <- mean(m$data$muddy)
pd$rocky <- mean(m$data$rocky)
pd$mixed <- mean(m$data$mixed)
pd$survey <- "HBLL"


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
    Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
    Biomass = exp(est) ,
    lowCI = exp(est - 1.96 * est_se),
    highCI = exp(est + 1.96 * est_se))

# p$species <- "Halibut"
# saveRDS(p, here::here(paste0("data-generated/halibut-tv-depth.rds")))

# p$species <- "YE"
# saveRDS(p, here::here(paste0("data-generated/ye-tv-depth.rds")))
p <- readRDS( here::here(paste0("data-generated/ye-tv-depth.rds")))

p2 <- readRDS( here::here(paste0("data-generated/halibut-tv-depth.rds")))

p <- bind_rows(p2, p) #%>% filter(year==2008)
p <-  p %>% rename(Species = species)

ggplot(p,
  aes(Depth, Biomass, group= year, fill=Species, colour = Species)) +
  geom_ribbon(data = filter(p, Species == "Halibut"),aes(
    ymin = lowCI,
    ymax = highCI), alpha = 0.05, colour = NA) +
  geom_ribbon(data = filter(p, Species == "YE"),aes(
    ymin = lowCI,
    ymax = highCI), alpha = 0.1, colour = NA) +
  geom_line(data = filter(p, Species == "Halibut"), aes(alpha=year), size = 1) + #, alpha =0.75
  geom_line(data = filter(p, Species == "YE"), aes(alpha=year), size = 1) +
  # add line at mean depth -1 SD, value used for further effect plots
  geom_vline(xintercept = exp(-1 * m$data$depth_sd[1] + m$data$depth_mean[1]), linetype = "dashed") +
  scale_fill_viridis_d(option = "C", begin = 0, end = 0.7) +
  scale_colour_viridis_d(option = "C", begin = 0, end = 0.7) +
  ylab("Biomass density (kg/ha)") +
  scale_x_reverse(limits = c(600,10)) +
  guides(alpha = FALSE) +
  coord_flip(ylim = c(0, quantile(p$highCI, 1)-0.2)) +
  # scale_x_continuous(limits = c(18,210)) +
  # coord_cartesian(xlim = c(20, 700), ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs() + theme(legend.position = c(0.7,0.3))
ggsave("figs/depth-profiles.png", width = 4, height = 4)


# p <- readRDS( here::here(paste0("data-generated/halibut-tv-depth.rds")))
# p <- readRDS( here::here(paste0("data-generated/ye-tv-depth.rds")))
#
# ggplot(p,
#   aes(Depth, Biomass,
#     ymin = lowCI,
#     ymax = highCI,
#     group = as.factor(year), fill=as.factor(year), colour = as.factor(year))) +
#   geom_ribbon(alpha = 0.1, colour = NA) +
#   geom_line(size = 0.5, alpha =0.85) +
#   # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
#   # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
#   scale_fill_viridis_d(name = "Year", option = "C", end = 0.7) +
#   scale_colour_viridis_d(name = "Year", option = "C", end = 0.7) +
#   # ylab("DO") +
#   # scale_x_reverse(limits = c(500,20)) +
#   # coord_flip(ylim = c(0, max(p$Biomass))) +
#   # scale_x_continuous(limits = c(18,210)) +
#   coord_cartesian(xlim = c(20, 700), ylim=c(0, quantile(p$Biomass, 1))) +
#   gfplot::theme_pbs()

# ggsave("halibut-tv-depth.png", width = 5, height = 4)
# ggsave("ye-tv-depth.png", width = 5, height = 4)

# rocky plot
m <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn.rds")
# check the quadratic version gives identical predictions to gam
# m <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky2-muddy2-400kn.rds")
m <- readRDS(file = "models/yelloweye-hybrid-hbll-mw-rocky-muddy-400kn.rds")

pd <- expand.grid(
  rocky = seq(min(m$data$rocky),
    max(m$data$rocky), length.out = 20),
  survey = unique(m$data$survey)
  # year = unique(m$data$year)
)
# pd$survey <- "HBLL"
pd$year <- 2020

pd$depth_scaled <- -1
# pd$depth_scaled2 <- pd$depth_scaled^2
pd$muddy <- 0
# pd$rocky2 <- pd$rocky^2
pd$mixed <- 0 #mean(m$data$mixed)


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  Biomass = exp(est) ,
  lowCI = exp(est - (1.96 * est_se)),
  highCI = exp(est + (1.96 * est_se)))


# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-2020-ye-peak-depth.rds")))
saveRDS(p, here::here(paste0("data-generated/ye-rocky-2020-ye-peak-depth.rds")))
p <- readRDS(here::here(paste0("data-generated/halibut-rocky-2020-ye-peak-depth.rds")))
p <- readRDS(here::here(paste0("data-generated/ye-rocky-2020-ye-peak-depth.rds")))

ggplot(p,
  aes(rocky, Biomass,
    ymin = lowCI,
    ymax = highCI,
    group = survey, fill= survey, colour = survey
    )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.75, alpha =0.95) +
  scale_fill_viridis_d(name = "Survey", option = "D", end = 0.8) +
  scale_colour_viridis_d(name = "Survey", option = "D", end = 0.9) +
  xlab("Proportion rocky") +
  gfplot::theme_pbs() + theme(legend.position = c(0.8, 0.8))

ggsave("figs/halibut-rocky.png", width = 4, height = 4)
ggsave("figs/ye-rocky.png", width = 4, height = 4)


# muddy plot
m <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky-muddy-400kn.rds")
# check the quadratic version gives identical predictions to gam
# m <- readRDS(file = "models/halibut-hybrid-keepable-model-rocky2-muddy2-400kn.rds")
m <- readRDS(file = "models/yelloweye-hybrid-hbll-mw-rocky-muddy-400kn.rds")

pd <- expand.grid(
  muddy = seq(min(m$data$muddy),
    max(m$data$muddy), length.out = 20),
  survey = unique(m$data$survey)
  # year = unique(m$data$year)
)
# pd$survey <- "HBLL"
pd$year <- 2020

pd$depth_scaled <- -1
# pd$muddy <- 0
pd$rocky <- 0
pd$mixed <- 0 #mean(m$data$mixed)


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  Biomass = exp(est) ,
  lowCI = exp(est - (1.96 * est_se)),
  highCI = exp(est + (1.96 * est_se)))


# saveRDS(p, here::here(paste0("data-generated/halibut-muddy.rds")))
# saveRDS(p, here::here(paste0("data-generated/ye-muddy.rds")))

p <- readRDS(here::here(paste0("data-generated/halibut-muddy.rds")))
p <- readRDS(here::here(paste0("data-generated/ye-muddy.rds")))

ggplot(p,
  aes(muddy, Biomass,
    ymin = lowCI,
    ymax = highCI,
    # group = as.factor(year), fill=as.factor(year), colour = as.factor(year)
    group = survey, fill= survey, colour = survey
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.75, alpha =0.95) +
  scale_fill_viridis_d(name = "Survey", option = "D", end = 0.8) +
  scale_colour_viridis_d(name = "Survey", option = "D", end = 0.9) +
  xlab("Proportion muddy") +
  gfplot::theme_pbs()+ theme(legend.position = c(0.8, 0.8))


ggsave("figs/halibut-muddy.png", width = 4, height = 4)
ggsave("figs/ye-muddy.png", width = 4, height = 4)


# mixed plot
m <- readRDS(file = "models/yelloweye-hybrid-RW-w-rocky-mixed-400kn.rds")

pd <- expand.grid(
  mixed = seq(min(m$data$mixed),
    max(m$data$mixed), length.out = 20),
  survey = unique(m$data$survey)
  # year = unique(m$data$year)
)
# pd$survey <- "HBLL"
pd$year <- 2008

pd$depth_scaled <- min(m$data$depth_scaled)
pd$depth_scaled2 <- pd$depth_scaled^2
pd$muddy <- 0
pd$rocky <- 0
# pd$mixed <- mean(m$data$mixed)


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  Biomass = exp(est) ,
  lowCI = exp(est - (1.96 * est_se)),
  highCI = exp(est + (1.96 * est_se)))


# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-ind-sptemp-fields.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-mixed-ind-sptemp-fields.rds")))
# p <- readRDS(here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))

ggplot(p,
  aes(mixed, Biomass,
    ymin = lowCI,
    ymax = highCI,
    # group = as.factor(year), fill=as.factor(year), colour = as.factor(year)
    group = survey, fill= survey, colour = survey
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.5, alpha =0.85) +
  # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
  # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
  scale_fill_viridis_d(name = "Survey", option = "C") +
  scale_colour_viridis_d(name = "Survey", option = "C") +
  # ylab("DO") +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  # coord_cartesian(#xlim = c(20, 700),
  #   ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs()

# ggsave("halibut-mixed-w-ind-sptemp-fields.png", width = 5, height = 4)
# ggsave("halibut-mixed-w-ind-sptemp-fields.png", width = 5, height = 4)
# ggsave("halibut-allsub-mixed.png", width = 5, height = 4)

ggsave("ye-mixed-RW.png", width = 5, height = 4)
