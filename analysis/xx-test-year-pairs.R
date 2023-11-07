# see how well the use of year pair improves upon a RW with a spatial field
library(tidyverse)
library(sdmTMB)

# if models not run yet
substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 52.15507) %>% left_join(select(substrate, -X, -Y))

years <- sort(unique(d_ye$year))

d_hal <- d_hal %>% filter(year %in% years)
# d_hal$present <- ifelse(d_hal$density > 0, 1, 0)
d_hal$year_f <- as.factor(d_hal$year)

d_ye <- d_ye %>% filter(year %in% years)
# d_ye$present <- ifelse(d_ye$density > 0, 1, 0)
d_ye$year_f <- as.factor(d_ye$year)

# Make relatively coarse mesh to allow years with fewer data points
mesh300kn <- make_mesh(d_hal, c("X", "Y"), n_knots = 300)
plot(mesh300kn$mesh, asp = 1, main = "");points(d_hal$X, d_hal$Y, pch = ".")

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-yr.rds")
if (!file.exists(f)) {
  m1 <- sdmTMB(
    formula = density ~
      as.factor(survey) +
      rocky +
      muddy +
      poly(depth_scaled, 2),
    data = d_hal,
    mesh = mesh300kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year_true",
    silent = FALSE,
    reml = T,
    family = tweedie(link = "log")
  )
  saveRDS(m1, file = f)
}

f <- paste0("models/halibut-stitch-keepable-model-rocky-muddy-300kn-tw-noyr.rds")
if (!file.exists(f)) {
  m2 <- sdmTMB(
    formula = density ~
      as.factor(survey) +
      rocky +
      muddy +
      poly(depth_scaled, 2),
    data = d_hal,
    mesh = mesh300kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year",
    silent = FALSE,
    reml = T,
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


grid <- readRDS(file = "data-generated/full_filled_grid_paired.rds") %>% filter(year == 2020)
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
