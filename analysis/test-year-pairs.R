# see how well the use of year pair improves upon a RW with a spatial field

# if models not run yet
substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

years <- sort(unique(d_ye$year))

d_hal <- d_hal %>% filter(year %in% years)
d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
d_hal$year_f <- as.factor(d_hal$year)

d_ye <- d_ye %>% filter(year %in% years)
d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
d_ye$year_f <- as.factor(d_ye$year)

# Make relatively coarse mesh to allow years with fewer data points
mesh300kn <- make_mesh(d_hal, c("X", "Y"), n_knots = 300)
plot(mesh300kn$mesh, asp = 1, main = "");points(d_hal$X, d_hal$Y, pch = ".")

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-yr.rds")
if (!file.exists(f)) {
  m1 <- sdmTMB(
    formula = density ~
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh300kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year_true",
    silent = FALSE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m1, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-noyr.rds")
if (!file.exists(f)) {
  m2 <- sdmTMB(
    formula = density ~
      as.factor(survey) +
      rocky + I(rocky^2) +
      muddy + I(muddy^2) +
      depth_scaled + I(depth_scaled^2),
    data = d_hal,
    mesh = mesh300kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    reml = T, # F is simpler to explain.
    family = tweedie(link = "log")
  )
  saveRDS(m2, file = f)
}

# model with year_true
m1 <- readRDS("~/github/dfo/ye-wcvi/models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-yr.rds")
# model with year_pair
m2 <- readRDS("~/github/dfo/ye-wcvi/models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-noyr.rds")

AIC(m1, m2)
#   df      AIC
# m1  5 4246.570
# m2  5 4248.577

# > m1
# Spatiotemporal model fit by REML ['sdmTMB']
# Formula: density ~ 1 + as.factor(survey) + rocky + I(rocky^2) + muddy +
#   Formula:     I(muddy^2) + depth_scaled + I(depth_scaled^2)
# Mesh: mesh300kn
# Data: d_hal
# Family: tweedie(link = 'log')
#
# coef.est coef.se
# (Intercept)               -1.36    0.18
# as.factor(survey)TRAWL    -0.70    0.09
# rocky                     -0.84    0.55
# I(rocky^2)                -0.14    0.65
# muddy                      0.68    0.59
# I(muddy^2)                -1.30    0.67
# depth_scaled              -0.77    0.09
# I(depth_scaled^2)         -0.22    0.04
#
# Dispersion parameter: 0.75
# Matern range: 0.39
# Spatial SD: 0.64
# Spatiotemporal SD: 0.47
# REML criterion at convergence: 2118.285

# > m2
# Spatiotemporal model fit by REML ['sdmTMB']
# Formula: density ~ 1 + as.factor(survey) + rocky + I(rocky^2) + muddy +
#   Formula:     I(muddy^2) + depth_scaled + I(depth_scaled^2)
# Mesh: mesh300kn
# Data: d_hal
# Family: tweedie(link = 'log')
#
# coef.est coef.se
# (Intercept)               -1.32    0.18
# as.factor(survey)TRAWL    -0.71    0.09
# rocky                     -0.86    0.56
# I(rocky^2)                -0.11    0.65
# muddy                      0.65    0.59
# I(muddy^2)                -1.26    0.67
# depth_scaled              -0.75    0.09
# I(depth_scaled^2)         -0.22    0.04
#
# Dispersion parameter: 0.75
# Matern range: 0.36
# Spatial SD: 0.54
# Spatiotemporal SD: 0.65
# REML criterion at convergence: 2119.289


grid <- readRDS(file = "report-data/full_filled_grid_paired.rds") %>% filter(year == 2020)
original_time <- sort(unique(d_hal$year_true))
nd <- do.call(
  "rbind",
  replicate(length(original_time), grid, simplify = FALSE)
)
nd[["year_true"]] <- rep(original_time, each = nrow(grid))
nd[["survey"]] <- "HBLL"
nd

original_time2 <- sort(unique(d_hal$year_pair))
nd2 <- do.call(
  "rbind",
  replicate(length(original_time2), grid, simplify = FALSE)
)
nd2[["year"]] <- rep(original_time2, each = nrow(grid))
nd2[["survey"]] <- "HBLL"
nd2



p1 <- predict(m1, newdata = nd, return_tmb_object = T)
i1 <- get_index(p1, area = 4)
i1$year <- i1$year_true

p2 <- predict(m2, newdata = nd2, return_tmb_object = T)
i2 <- get_index(p2, area = 4)

i1 <- itw3
i2 <- itw4

ggplot(i1, aes(year, est)) + geom_line() + geom_line(data = i2, colour = "red")
