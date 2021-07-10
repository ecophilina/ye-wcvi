
m <- readRDS(file = "models/halibut-hybrid-model-RW-w-tv-400kn.rds")
m <- readRDS(file = "models/halibut-hybrid-model-allsub-tv-depth-400kn.rds")
m <- readRDS(file = "models/halibut-hybrid-model-RW-w-all-400kn.rds")


# depth plots
pd <- expand.grid(
  depth_scaled = seq(min(m$data$depth_scaled),
    max(m$data$depth_scaled), length.out = 100),
  year = unique(m$data$year)
)

pd$depth_scaled2 <- pd$depth_scaled^2
pd$muddy <- 0
pd$rocky <- 0
pd$mixed <- mean(m$data$mixed)
pd$survey <- "HBLL"


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
    Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
    Biomass = exp(est) ,
    lowCI = exp(est - 1.96 * est_se),
    highCI = exp(est + 1.96 * est_se))

# saveRDS(p, here::here(paste0("data-generated/halibut-tv-depth.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-tv-depth-ind-sptemp-fields.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-depth-w-RW.rds")))

p <- readRDS(here::here(paste0("data-generated/halibut-tv-depth-ind-sptemp-fields.rds")))

ggplot(p,
  aes(Depth, Biomass,
    ymin = lowCI,
    ymax = highCI,
    group = as.factor(year), fill=as.factor(year), colour = as.factor(year))) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.5, alpha =0.85) +
  # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
  # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
  scale_fill_viridis_d(name = "Year", option = "C") +
  scale_colour_viridis_d(name = "Year", option = "C") +
  # ylab("DO") +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  coord_cartesian(xlim = c(20, 700), ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs()

ggsave("halibut-tv-depth.png", width = 5, height = 4)


# rocky plot
m <- readRDS(file = "models/halibut-hybrid-model-RW-w-rocky-muddy-400kn.rds")
pd <- expand.grid(
  rocky = seq(min(m$data$rocky),
    max(m$data$rocky), length.out = 20),
  survey = unique(m$data$survey)
  # year = unique(m$data$year)
)
# pd$survey <- "HBLL"
pd$year <- 2008

pd$depth_scaled <- min(m$data$depth_scaled)
pd$depth_scaled2 <- pd$depth_scaled^2
pd$muddy <- 0
# pd$rocky <- 0
pd$mixed <- mean(m$data$mixed)


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  Biomass = exp(est) ,
  lowCI = exp(est - (1.96 * est_se)),
  highCI = exp(est + (1.96 * est_se)))


# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-ind-sptemp-fields.rds")))

# p <- readRDS(here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))

ggplot(p,
  aes(rocky, Biomass,
    ymin = lowCI,
    ymax = highCI,
    # group = as.factor(year), fill=as.factor(year), colour = as.factor(year)
    group = survey, fill= survey, colour = survey
    )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.5, alpha =0.85) +
  # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
  # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
  scale_fill_viridis_d(name = "Year", option = "C") +
  scale_colour_viridis_d(name = "Year", option = "C") +
  # ylab("DO") +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  coord_cartesian(#xlim = c(20, 700),
    ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs()
# ggsave("halibut-rocky-w-ind-sptemp-fields.png", width = 5, height = 4)
ggsave("halibut-RW-rocky.png", width = 5, height = 4)

# mixed plot
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
  scale_fill_viridis_d(name = "Year", option = "C") +
  scale_colour_viridis_d(name = "Year", option = "C") +
  # ylab("DO") +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  # coord_cartesian(#xlim = c(20, 700),
  #   ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs()

# ggsave("halibut-rocky-w-ind-sptemp-fields.png", width = 5, height = 4)
ggsave("halibut-mixed-w-ind-sptemp-fields.png", width = 5, height = 4)

# muddy plot
pd <- expand.grid(
  muddy = seq(min(m$data$muddy),
    max(m$data$muddy), length.out = 20),
  survey = unique(m$data$survey)
  # year = unique(m$data$year)
)
# pd$survey <- "HBLL"
pd$year <- 2008

pd$depth_scaled <- min(m$data$depth_scaled)
pd$depth_scaled2 <- pd$depth_scaled^2
# pd$muddy <- 0
pd$rocky <- 0
pd$mixed <- mean(m$data$mixed)


p <- predict(m, newdata = pd, se_fit = TRUE, re_form = NA)
p <- p %>% mutate(
  Depth = exp(depth_scaled * m$data$depth_sd[1] + m$data$depth_mean[1]),
  Biomass = exp(est) ,
  lowCI = exp(est - (1.96 * est_se)),
  highCI = exp(est + (1.96 * est_se)))


# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-rocky-ind-sptemp-fields.rds")))
# saveRDS(p, here::here(paste0("data-generated/halibut-mixed-ind-sptemp-fields.rds")))

# saveRDS(p, here::here(paste0("data-generated/halibut-muddy-ind-sptemp-fields.rds")))
saveRDS(p, here::here(paste0("data-generated/halibut-muddy-w-RW.rds")))
# p <- readRDS(here::here(paste0("data-generated/halibut-rocky-w-RW.rds")))

ggplot(p,
  aes(muddy, Biomass,
    ymin = lowCI,
    ymax = highCI,
    # group = as.factor(year), fill=as.factor(year), colour = as.factor(year)
    group = survey, fill= survey, colour = survey
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 0.5, alpha =0.85) +
  # geom_smooth(method= "loess", size = 0.4, alpha =0.85, se =F ) +
  # geom_smooth(span = 0.5, size = 0.4, alpha =0.85, se =F ) +
  scale_fill_viridis_d(name = "Year", option = "C") +
  scale_colour_viridis_d(name = "Year", option = "C") +
  # ylab("DO") +
  # scale_x_reverse(limits = c(500,20)) +
  # coord_flip(ylim = c(0, max(p$Biomass))) +
  # scale_x_continuous(limits = c(18,210)) +
  coord_cartesian(#xlim = c(20, 700),
    ylim=c(0, quantile(p$Biomass, 1))) +
  gfplot::theme_pbs()

# ggsave("halibut-rocky-w-ind-sptemp-fields.png", width = 5, height = 4)
# ggsave("halibut-muddy-w-ind-sptemp-fields.png", width = 5, height = 4)
ggsave("halibut-muddy-w-RW.png", width = 5, height = 4)



