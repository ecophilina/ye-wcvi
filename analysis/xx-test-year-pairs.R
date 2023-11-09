# see how well the use of year pair improves upon a RW with a spatial field
library(tidyverse)
library(sdmTMB)

# if models not run yet
substrate <- readRDS(file = "data-generated/events_w_substrate_1km_buffer.rds") %>%
  select(X, Y, fishing_event_id, any_rock, rocky, mixed, muddy)

d_hal <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(survey != "NON-SURVEY") %>% # using survey data only
  filter(survey != "NON-SURVEY" | (dist_km_fished < 3 & dist_km_fished > 0.2)) %>%
  # filter(survey != "NON-SURVEY" | (dist_km_fished > 0.2)) %>% # test with all valid data (this filters 0s)
  # filter(latitude < 52.15507) %>%
  filter(latitude < 51) %>%
  left_join(select(substrate, -X, -Y))

d_ye <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(survey != "NON-SURVEY") %>% # using survey data only
  filter(survey != "NON-SURVEY" | (dist_km_fished < 3 & dist_km_fished > 0.2)) %>%
  # filter(survey != "NON-SURVEY" | (dist_km_fished > 0.2)) %>%  # test with all valid data (this filters 0s)
  # filter(latitude < 52.15507) %>%
  filter(latitude < 51) %>% mutate(
    survey = ifelse(survey == "NON-SURVEY" & year > 2015, "RESTRICTED", survey)) %>%
  left_join(select(substrate, -X, -Y))

years <- sort(unique(d_ye$year))

d <- d_hal %>% filter(year %in% years)
d <- d_ye %>% filter(year %in% years)
d$fyear <- as.factor(d$year)

# d_ye <- d_ye %>% filter(year %in% years)
# d_ye$fyear <- as.factor(d_ye$year)

# Make relatively coarse mesh to allow years with fewer data points
mesh300kn <- make_mesh(d, c("X", "Y"), n_knots = 300)
plot(mesh300kn$mesh, asp = 1, main = "");points(d$X, d$Y, pch = ".")



# # model with year_true and RW st fields

# f <- paste0("models/halibut-w-cctrim-300kn-tw-yr.rds")
# f <- paste0("models/halibut-w-cc-300kn-tw-yr.rds")
# f <- paste0("models/halibut-300kn-tw-yr.rds")
# f <- paste0("models/halibut-w-cc-300kn-dg-yr.rds")
# f <- paste0("models/halibut-w-all-cc-300kn-dg-yr.rds")
# f <- paste0("models/halibut-300kn-dg-yr.rds")

# f <- paste0("models/ye-w-cc-300kn-dg-yr.rds")
f <- paste0("models/ye-300kn-dg-yr.rds")

if (!file.exists(f)) {
  m1 <- sdmTMB(
    formula = density ~
      as.factor(survey) +
      rocky +
      muddy +
      poly(depth_scaled, 2),
    data = d,
    mesh = mesh300kn,
    spatial = "on",
    spatiotemporal = "RW",
    time = "year_true",
    silent = FALSE,
    reml = T,
    # family = tweedie(link = "log")
    family = delta_gamma()
  )
  saveRDS(m1, file = f)
} else {
  m1 <- readRDS(f)
}

# # model with year_pair

# f <- paste0("models/halibut-w-cctrim-300kn-tw-yrpair.rds")
# f <- paste0("models/halibut-w-cc-300kn-tw-yrpair.rds")
# f <- paste0("models/halibut-300kn-tw-yrpair.rds")
# f <- paste0("models/halibut-w-cc-300kn-dg-yrpair.rds")
# f <- paste0("models/halibut-w-all-cc-300kn-dg-yrpair.rds")
# f <- paste0("models/halibut-300kn-dg-yrpair.rds")

# f <- paste0("models/ye-w-cc-300kn-dg-yrpair.rds")
# f <- paste0("models/ye-w-cc-300kn-dg-yrpair-iid.rds")
f <- paste0("models/ye-300kn-dg-yrpair-iid.rds")

if (!file.exists(f)) {
  m2 <- sdmTMB(
    formula = density ~
      fyear +
      as.factor(survey) +
      rocky +
      muddy +
      poly(depth_scaled, 2),
    data = d,
    mesh = mesh300kn,
    spatial = "on",
    # spatiotemporal = "AR1",
    spatiotemporal = "IID",
    time = "year", # actually a year pair
    silent = FALSE,
    reml = T,
    # family = tweedie(link = "log")
    family = delta_gamma()
  )
  saveRDS(m3, file = f)
} else {
  m2 <- readRDS(f)
}

AIC(m1, m2)

## without cc
# df      AIC
# m1 11 13300.52
# m2 18 13269.80

## tw with all cc
# df      AIC
# m1 12 114521.9
# m2 19 113884.5

## with cc trim effort
# df      AIC
# m1 12 99417.16
# m2 19 99488.65

## with cc and dg
# df      AIC
# m1 21 97986.05
# m2 35 98017.32

## with cc and dg - trim lat 51
# df      AIC
# m1 21 76402.39
# m2 35 76422.41

## without cc - trim lat 51
# df      AIC
# m1 19 8883.452
# m2 33 8867.925

# YE with cc and dg - trim lat 51
# df      AIC
# m1 23 36407.76
# m2 35 36317.25


grid <- readRDS(file = "data-generated/full_filled_grid_paired.rds") %>% filter(year == 2020)
original_time <- sort(unique(d$year_true))
nd <- do.call(
  "rbind",
  replicate(length(original_time), grid, simplify = FALSE)
)
nd[["year_true"]] <- rep(original_time, each = nrow(grid))
nd[["survey"]] <- "HBLL"
# nd

original_time2 <- sort(unique(d$year_pair))
nd2 <- do.call(
  "rbind",
  replicate(length(original_time2), grid, simplify = FALSE)
)
nd2[["year"]] <- rep(original_time2, each = nrow(grid))
nd2[["survey"]] <- "HBLL"
# nd2
nd2$fyear <- as.factor(nd2$year)

p1 <- predict(m1, newdata = nd, return_tmb_object = T)
i1 <- get_index(p1, area = 4)
i1$year <- i1$year_true

p2 <- predict(m2, newdata = nd2, return_tmb_object = T)
i2 <- get_index(p2, area = 4)

ggplot(i1, aes(year, est
               , ymin = lwr, ymax = upr
               )) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  geom_line(data = i2, colour = "red") +
  geom_ribbon(data = i2, fill = "red", alpha = 0.2) +
  ggsidekick::theme_sleek()

# ggsave("figs/test-year-pairs-dg-all-good.png")
# ggsave("figs/test-year-pairs-tw-nocc.png")
# ggsave("figs/test-year-pairs-dg.png")
# ggsave("figs/test-year-pairs-dg-trim51.png")
# ggsave("figs/test-year-pairs-dg-trim51-nocc.png")

# ggsave("figs/test-year-pairs-dg-trim51-ye-iid.png")
ggsave("figs/test-year-pairs-dg-trim51-ye-iid-nocc.png")
