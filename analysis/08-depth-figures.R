# catch by depth figures
library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)

library(gfplot)
library(ggsidekick)

### Merge survey catches
d_hal_plotsS <- readRDS("data-generated/halibut-model-data-keepable-weight.rds") %>%
  filter(latitude < 50.5) %>%
  filter(survey != "NON-SURVEY")

d_ye_plotsS <- readRDS("data-generated/yelloweye-model-data-hbll-weights.rds") %>%
  filter(latitude < 50.5) %>%
  filter(survey != "NON-SURVEY")

ye_obs <- d_ye_plotsS %>%
  # mutate(catch_count_ye = (catch_count/hook_count)*100, catch_equiv_ye = catch_equiv) %>%
  select(X, Y,
    depth = depth_m, year_pair, year_true, survey, # catch_count_ye, catch_equiv_ye,
    density_ye = density, fishing_event_id
  )
hal_obs <- d_hal_plotsS %>%
  # mutate(catch_count_hal = (catch_count/hook_count)*100, catch_equiv_hal = catch_equiv) %>%
  select(fishing_event_id, # catch_count_hal, catch_equiv_hal,
    density_hal = density
  )
surv_dat <- left_join(ye_obs, hal_obs) # %>% filter(depth < 200)


focal_area <- sf::st_read(
  dsn = "shape-files/taaqwiihak_areaVer2.shp",
  layer = "taaqwiihak_areaVer2", quiet = TRUE
)
focal_area_proj <- sf::st_transform(focal_area, crs = 3156)

surv_dat <- surv_dat %>% mutate(X2 = X * 100000, Y2 = Y * 100000)
surv_dat_sf <- st_as_sf(surv_dat, coords = c("X2", "Y2"), crs = 3156)
keep <- st_intersects(focal_area_proj, surv_dat_sf)
surv_dat_cda <- surv_dat_sf[unlist(keep), ]
st_geometry(surv_dat_cda) <- NULL
surv_dat_cda$region <- "CDA"

focal_area2 <- sf::st_read(dsn = "shape-files/ExtendCDA1/Extend_1.shp", layer = "Extend_1")
focal_area2_proj <- sf::st_transform(focal_area2, crs = 3156)

keep2 <- st_intersects(focal_area2_proj, surv_dat_sf)
surv_dat_ext <- surv_dat_sf[unlist(keep2), ]
st_geometry(surv_dat_ext) <- NULL
surv_dat_ext$region <- "CDA adjacent"

surv_dat_ext2 <- anti_join(surv_dat_ext, surv_dat_cda, by = c("X", "Y", "fishing_event_id"))
surv_dat2 <- anti_join(surv_dat, surv_dat_ext, by = c("X", "Y", "fishing_event_id"))
surv_dat2$region <- "non-CDA 3CD"
surv_dat <- bind_rows(surv_dat2, surv_dat_cda) %>% bind_rows(., surv_dat_ext2)

# load commercial catch
f <- "data-generated/commercialcpue.rds"

if (!file.exists(f)) {
  stop("Need to run plot-commercial-cpue.R script first.")
} else {
  cc <- readRDS(f)

  # this should match whatever is done in plot-commercial-cpue.R script
  cc <- cc %>%
    filter(!(region == "CDA" & depth > 130)) %>% # removes single depth outlier in CDA
    filter(dist_km_fished < 3 & dist_km_fished > 0.2)
}

cols <- c(
  "red",
  "deeppink4",
  # "darkorchid4", # #4DAF4A", "#984EA3",
  "deepskyblue4"
)

g1 <- surv_dat %>%
  filter(depth < 1000) %>%
  ggplot() + geom_histogram(aes(depth), fill = "darkgreen") +
  geom_histogram(data = surv_dat %>%
                   filter(depth < 1000, density_ye != 0), aes(depth), alpha = 0.95, fill = "yellow") +
  xlim(0, 800) +
  ylab("Survey samples") +
  facet_wrap(~region) +
  # facet_wrap(~region, scales = "free_y") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

g2 <- cc %>%
  filter(year <= 2015) %>%
  filter(depth < 1000 & dist_km_fished < 3 & dist_km_fished > 0.2) %>%
  filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD")) %>%
  ggplot() + geom_histogram(aes(depth), fill = "darkgreen") +
  geom_histogram(data = cc %>%
                   filter(year <= 2015) %>%
                   filter(depth < 1000 & dist_km_fished < 3 & dist_km_fished > 0.2) %>%
                   filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD")) %>%
                   filter(ye_cpue != 0), aes(depth), alpha = 0.95, fill = "yellow") +
  xlim(0, 800) +
  ylab("Commercial longline\n(pre-2016)") +
  xlab("Depth") +
  facet_wrap(~region) +
  # facet_wrap(~region, scales = "free_y") +
  theme(strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

g3 <- cc %>%
  filter(year > 2015) %>%
  filter(depth < 1000 & dist_km_fished < 3 & dist_km_fished > 0.2) %>%
  filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD")) %>%
  ggplot() + geom_histogram(aes(depth), fill = "darkgreen") +
  geom_histogram(data = cc %>%
                   filter(year > 2015) %>%
                   filter(depth < 1000 & dist_km_fished < 3 & dist_km_fished > 0.2) %>%
                   filter(region %in% c("CDA", "CDA adjacent", "non-CDA 3CD")) %>%
                   filter(ye_cpue != 0), aes(depth), alpha = 0.95, fill = "yellow") +
  xlim(0, 800) +
  ylab("Commercial longline\n(2016 onwards)") +
  xlab("Depth") +
  facet_wrap(~region) +
  # facet_wrap(~region, scales = "free_y") +
  theme(strip.text.x = element_blank())


g1 + g2 + g3 + patchwork::plot_layout(nrow = 3)

ggsave("figs/ye-presence-hist.png", width = 6.5, height = 5.5)


plot_catch_by_depth <- function(data) {
  ggplot(data, aes(depth, density,
    colour = region, fill = region
  )) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "gam") +
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols) +
    # scale_y_log10(limits = c(0.4, 400)) +
    coord_cartesian(
      xlim = c(10, 550),
      ylim = c(0, 6.9),
      expand = F
    ) +
    xlab("Depth") +
    ggsidekick::theme_sleek() +
    theme(legend.position = c(0.8, 0.8))
}

p1 <- surv_dat %>%
  filter(depth < 1000) %>%
  mutate(density = log(density_ye + 1)) %>%
  plot_catch_by_depth(.) +
  ylab("Log (survey CPUE + 1)") +
  ggtitle("Yelloweye Rockfish") +
  labs(tag = "(a)") +
  theme(
    plot.tag.position = c(0.13, 0.87),
    plot.tag = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )


p2 <- surv_dat %>%
  filter(depth < 1000) %>%
  mutate(density = log(density_hal + 1)) %>%
  plot_catch_by_depth(.) +
  # ylab("Log (survey biomass density + 1)") +
  ggtitle("Landable-size Pacific Halibut") +
  labs(tag = "(b)") +
  theme(
    legend.position = "none",
    plot.tag = element_text(
      face = "bold",
      size = 11
    ),
    plot.tag.position = c(0.07, 0.87),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


p3 <- cc %>%
  filter(depth < 1000) %>%
  # filter(year <= 2015) %>%
  # filter(year > 2015) %>%
  filter(region != "3CD5A N of 50º") %>%
  filter(hal_cpue > 0) %>%
  mutate(density = log(ye_cpue + 1)) %>%
  plot_catch_by_depth(.) +
  ylab("Log (commercial CPUE + 1)") +
  # ggtitle("B. Pacific Halibut") +
  facet_grid(
    # cols=vars(region),
    rows = vars(season)
  ) +
  theme(
    legend.position = "none",
    panel.spacing.y = unit(1, "lines"),
    strip.text.y = element_blank()
  )

p4 <- cc %>%
  filter(depth < 1000) %>%
  # filter(year <= 2015) %>%
  # filter(year > 2015) %>%
  filter(region != "3CD5A N of 50º") %>%
  mutate(density = log(hal_cpue + 1)) %>%
  plot_catch_by_depth(.) +
  # ylab("Log (commercial biomass density + 1)") +
  # ggtitle("B. Pacific Halibut") +
  facet_grid(
    # cols=vars(region),
    rows = vars(season)
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing.y = unit(1, "lines"),
    legend.position = "none"
  )

library(egg)
p3b <- tag_facet(p3, tag_pool = c("c", "e"), size = 4)

p4b <- tag_facet(p4, tag_pool = c("d", "f"), size = 4)

p1 + p2 + p3b + p4b + plot_layout(ncol = 2, heights = c(1, 2))

ggsave("figs/all-catch-by-depth.png", width = 8, height = 9)

p3 + p4 +plot_layout(ncol = 2, heights = c(1))

ggsave("figs/comm-catch-by-depth.png", width = 8, height = 6)


# for slides
p1 <- surv_dat %>%
  filter(depth < 1000) %>%
  mutate(density = log(density_ye + 1)) %>%
  plot_catch_by_depth(.) +
  ylab("Log (survey CPUE + 1)") +
  ggtitle("Yelloweye Rockfish") +
  # labs(tag = "(a)") +
  theme(
    plot.tag.position = c(0.13, 0.87),
    plot.tag = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )


p2 <- surv_dat %>%
  filter(depth < 1000) %>%
  mutate(density = log(density_hal + 1)) %>%
  plot_catch_by_depth(.) +
  # ylab("Log (survey biomass density + 1)") +
  ggtitle("Landable-size Pacific Halibut") +
  # labs(tag = "(b)") +
  theme(
    legend.position = "none",
    plot.tag = element_text(
      face = "bold",
      size = 11
    ),
    plot.tag.position = c(0.07, 0.87),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


p5 <- cc %>%
  filter(depth < 1000) %>%
  filter(season == "Summer") %>%
  filter(region != "3CD5A N of 50º") %>%
  filter(hal_cpue > 0) %>%
  mutate(density = log(ye_cpue + 1)) %>%
  plot_catch_by_depth(.) +
  ylab("Log (commercial CPUE + 1)") +
  # ggtitle("B. Pacific Halibut") +
  theme(
    legend.position = "none",
    panel.spacing.y = unit(1, "lines"),
    strip.text.y = element_blank()
  )

p6 <- cc %>%
  filter(depth < 1000) %>%
  filter(season == "Summer") %>%
  filter(region != "3CD5A N of 50º") %>%
  mutate(density = log(hal_cpue + 1)) %>%
  plot_catch_by_depth(.) +
  # ylab("Log (commercial biomass density + 1)") +
  # ggtitle("B. Pacific Halibut") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing.y = unit(1, "lines"),
    legend.position = "none"
  )

p1 + p2 + p5 + p6 +plot_layout(ncol = 2, heights = c(1, 1))

ggsave("figs/summer-catch-by-depth.png", width = 8, height = 6)
