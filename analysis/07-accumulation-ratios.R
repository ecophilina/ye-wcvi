# attempt at uncertainty...
library(dplyr)
library(ggplot2)
library(tidyr)
# Do we need instructions for installing other dependencies like "pbsmapping"?
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
theme_set(ggsidekick::theme_sleek())

# # hal_model <- "w-effort-500kn-delta-AR1-aniso"
# # hal_model <- "w-good-depths-500kn-delta-AR1-aniso"
# hal_model <- "w-deeper-500kn-delta-AR1-aniso"
# # ye_model <- "w-effort-500kn-delta-spatial-aniso"
# # ye_model <- "w-good-depths-500kn-delta-iid-aniso"
# ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso"

ye_model <- "w-deeper-all-yrs-500kn-delta-iid-aniso-may23"
hal_model <- "w-deeper-500kn-delta-AR1-aniso-may23"


grid_scale <- 1000


cols <- c(
  "red",
  #"darkorchid4",
  "deeppink4",
  "deepskyblue4"
  # "cadetblue3"
)


i_hal <- readRDS(paste0("data-generated/halibut-", hal_model, "_", grid_scale, "-index-all-stan.rds"))
i_ye <- readRDS(paste0("data-generated/yelloweye-", ye_model, "_", grid_scale, "-index-all-stan.rds"))


g_1 <- i_hal$CDA[[3]] %>% filter(region %in% c("CDA")) %>% select(X, Y, year, area)
g_2 <- i_hal$`CDA adjacent`[[3]] %>% filter(region %in% c("CDA adjacent")) %>% select(X, Y, year, area)
g_3 <- i_hal$`non-CDA 3CD`[[3]] %>% filter(region %in% c("non-CDA 3CD")) %>% select(X, Y, year, area)

# area in these grids is in hectares
g1 <- g_1 %>% filter(area == grid_scale/10) %>% select(-area)
g2 <- g_2 %>% filter(area == grid_scale/10) %>% select(-area)
g3 <- g_3 %>% filter(area == grid_scale/10) %>% select(-area)
# g4 <- g_4 %>% filter(area == grid_scale/10) %>% select(-area)

g1_2020 <- filter(g1, year == 2020)
g2_2020 <- filter(g2, year == 2020)
g3_2020 <- filter(g3, year == 2020)
# g4_2020 <- filter(g4, year == 2020)

.file <- paste0(hal_model, "-100.rds")

if (!file.exists(paste0("data-generated/ye_sims_for_ratios_by_area", .file))) {

s_hal_1 <- i_hal$CDA[[4]][,1:100]
s_hal_2 <- i_hal$`CDA adjacent`[[4]][,1:100]
s_hal_3 <- i_hal$`non-CDA 3CD`[[4]][,1:100]

s_ye_1 <- i_ye$CDA[[4]][,1:100]
s_ye_2 <- i_ye$`CDA adjacent`[[4]][,1:100]
s_ye_3 <- i_ye$`non-CDA 3CD`[[4]][,1:100]


s_hal_1 <- bind_cols(g_1, as.data.frame(s_hal_1)) %>% filter(area == grid_scale/10) %>% select(-area)
s_hal_2 <- bind_cols(g_2, as.data.frame(s_hal_2))%>% filter(area == grid_scale/10) %>% select(-area)
s_hal_3 <- bind_cols(g_3, as.data.frame(s_hal_3))%>% filter(area == grid_scale/10) %>% select(-area)
# s_hal_4 <- bind_cols(g_4, as.data.frame(s_hal_4))%>% filter(area == grid_scale/10) %>% select(-area)

s_ye_1 <- bind_cols(g_1, as.data.frame(s_ye_1)) %>% filter(area == grid_scale/10) %>% select(-area)
s_ye_2 <- bind_cols(g_2, as.data.frame(s_ye_2))%>% filter(area == grid_scale/10) %>% select(-area)
s_ye_3 <- bind_cols(g_3, as.data.frame(s_ye_3))%>% filter(area == grid_scale/10) %>% select(-area)
# s_ye_4 <- bind_cols(g_4, as.data.frame(s_ye_4))%>% filter(area == grid_scale/10) %>% select(-area)

.s_ye <- bind_rows(
  mutate(s_ye_1, Area = "CDA"),
  mutate(s_ye_2, Area = "CDA adjacent"),
  mutate(s_ye_1, Area = "CDA expanded"),
  mutate(s_ye_2, Area = "CDA expanded"),
  mutate(s_ye_3, Area = "non-CDA 3CD")
  # ,mutate(s_ye_4, Area = "5B")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "ye_est")

.s_hal <- bind_rows(
  mutate(s_hal_1, Area = "CDA"),
  mutate(s_hal_2, Area = "CDA adjacent"),
  mutate(s_hal_1, Area = "CDA expanded"),
  mutate(s_hal_2, Area = "CDA expanded"),
  mutate(s_hal_3, Area = "non-CDA 3CD")
  # ,mutate(s_hal_4, Area = "5B")
) %>% pivot_longer(cols = starts_with("V"),
  names_to = ".iteration", values_to = "hal_est")

saveRDS(.s_hal, paste0("data-generated/hal_sims_for_ratios_by_region", .file))
saveRDS(.s_ye, paste0("data-generated/ye_sims_for_ratios_by_region", .file))
}




if (!file.exists(paste0("data-generated/maximize_hal_regions", .file))) {

.s_hal <- readRDS( paste0("data-generated/hal_sims_for_ratios_by_region", .file))
.s_ye <- readRDS( paste0("data-generated/ye_sims_for_ratios_by_region", .file))

glimpse(.s_hal)
glimpse(.s_ye)

# .dat <- left_join(.s_ye, .s_hal)
# left_join was crashing R so trying this
.s_ye <- .s_ye %>% select(ye_est)
.dat <- cbind(.s_hal, .s_ye)
.dat <- .dat %>% mutate(
  # so 0.0024384 * 0.009144 * 2 converts kg/km2 to kg/hook but mine are not yet in km2 they are in kg/ha
  halibut = hal_est * 100 * (500 * 0.0024384 * 0.009144 * 2),
  yelloweye = ye_est * 100 * (500 * 0.0024384 * 0.009144 * 2),
  ye_per_hal = yelloweye / (halibut), # not correcting for <1
  hal_per_ye = (halibut) / yelloweye # not correcting for <1
)

# .dat %>% View()

avoiding_ye <- .dat[order(.dat$yelloweye, decreasing = F), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    hal_cumsum2 = ifelse(cumsum(halibut) < 0.01, 0.01, cumsum(halibut)),
    ye_cumsum = cumsum(yelloweye),
    ye_cumsum2 = ifelse(cumsum(yelloweye) < 0.01, 0.01, cumsum(yelloweye)),
    cumsum_ye_hal = (ye_cumsum*100)/(hal_cumsum2*100),
    cumsum_hal_ye = (hal_cumsum*100)/(ye_cumsum2*100),
    hal_mean2 = ifelse((hal_cumsum*100)/ordered < 1, 1, (hal_cumsum*100)/ordered),
    mean_ratio = (ye_cumsum*100/ordered)/hal_mean2,
    ye_mean2 = ifelse((ye_cumsum*100)/ordered < 1, 1, (ye_cumsum*100)/ordered),
    mean_hal_ratio = (hal_cumsum*100/ordered)/ye_mean2,
    # proportion = ordered/nrow(cda_2020),
    pair_name = case_when(
      year %in% c(2007, 2008) ~ "2007-2008",
      year %in% c(2009, 2010) ~ "2009-2010",
      year %in% c(2011, 2012) ~ "2011-2012",
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020"
    )) %>% ungroup()

# check that denominator was successfully truncated to 1
# before
range(avoiding_ye$ye_cumsum/avoiding_ye$ordered)
# after
range(avoiding_ye$ye_mean2)
# before
range(avoiding_ye$hal_cumsum/avoiding_ye$ordered)
# after
range(avoiding_ye$hal_mean2)

avoiding_ye_sum <- avoiding_ye %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye = #ifelse(
      quantile(mean_hal_ratio, 0.025#) <1, 1, quantile(mean_hal_ratio, 0.025)
      ),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    mean_hal_per_ye = mean(mean_hal_ratio),
    halibut = mean(halibut),
    total_hal = mean(hal_cumsum),
    lwr_hal = quantile(hal_cumsum, 0.025),
    upr_hal = quantile(hal_cumsum, 0.975),
    yelloweye = mean(yelloweye),
    total_ye = mean(ye_cumsum),
    lwr_ye = quantile(ye_cumsum, 0.025),
    upr_ye = quantile(ye_cumsum, 0.975)
  ) %>% ungroup()


#### maximizing halibut strategy ###
maximize_hal <- .dat[order(.dat$halibut, decreasing = T), ] %>% group_by(Area, year, .iteration) %>%
  mutate(ordered = 1:n(),
    hal_cumsum = cumsum(halibut),
    hal_cumsum2 = ifelse(cumsum(halibut) < 0.01, 0.01, cumsum(halibut)),
    ye_cumsum = cumsum(yelloweye),
    ye_cumsum2 = ifelse(cumsum(yelloweye) < 0.01, 0.01, cumsum(yelloweye)),
    cumsum_ye_hal = (ye_cumsum*100)/(hal_cumsum2*100),
    cumsum_hal_ye = (hal_cumsum*100)/(ye_cumsum2*100),
    hal_mean2 = ifelse((hal_cumsum*100)/ordered < 1, 1, (hal_cumsum*100)/ordered),
    mean_ratio = (ye_cumsum*100/ordered)/hal_mean2,
    ye_mean2 = ifelse((ye_cumsum*100)/ordered < 1, 1, (ye_cumsum*100)/ordered),
    mean_hal_ratio = (hal_cumsum*100/ordered)/ye_mean2,
    # proportion = ordered/nrow(cda_2020),
    pair_name = case_when(
      year %in% c(2007, 2008) ~ "2007-2008",
      year %in% c(2009, 2010) ~ "2009-2010",
      year %in% c(2011, 2012) ~ "2011-2012",
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020"
    )) %>% ungroup()

# check that denominator was successfully truncated to 1
# before
range(maximize_hal$ye_cumsum/maximize_hal$ordered)
# after
range(maximize_hal$ye_mean2)
# before -- not needed
range(maximize_hal$hal_cumsum/maximize_hal$ordered)

maximize_hal_sum <- maximize_hal %>% ungroup() %>% group_by(ordered, year, pair_name, Area) %>%
  summarise(
    lwr_mean_ye_hal = quantile(mean_ratio, 0.025),
    upr_mean_ye_hal = quantile(mean_ratio, 0.975),
    mean_ye_per_hal = mean(mean_ratio),
    lwr_mean_hal_ye =
      # ifelse(
        quantile(mean_hal_ratio, 0.025#)<1, 1, quantile(mean_hal_ratio, 0.025)
      ),
    upr_mean_hal_ye = quantile(mean_hal_ratio, 0.975),
    halibut = mean(halibut),
    total_hal = mean(hal_cumsum),
    lwr_hal = quantile(hal_cumsum, 0.025),
    upr_hal = quantile(hal_cumsum, 0.975),
    yelloweye = mean(yelloweye),
    total_ye = mean(ye_cumsum),
    lwr_ye = quantile(ye_cumsum, 0.025),
    upr_ye = quantile(ye_cumsum, 0.975),
    mean_hal_per_ye = mean(mean_hal_ratio)
  ) %>% ungroup()

# range(maximize_hal_sum$lwr_mean_hal_ye)
#
# maximize_hal_sum_cda <- maximize_hal_sum %>% filter(Area == "CDA")
# range(maximize_hal_sum_cda$mean_ye_per_hal)
#


saveRDS(maximize_hal_sum, paste0("data-generated/maximize_hal_regions", .file))
saveRDS(avoiding_ye_sum, paste0("data-generated/avoiding_ye_regions", .file))
}
#

# chose which areas to plot

# areas_to_plot <- c("CDA","CDA adjacent","non-CDA 3CD")
areas_to_plot <- c("CDA","CDA expanded","non-CDA 3CD")

# #### make plots ####
maximize_hal_sum <- readRDS( paste0("data-generated/maximize_hal_regions", .file)) %>% mutate(Scenario = Area)
avoiding_ye_sum <- readRDS( paste0("data-generated/avoiding_ye_regions", .file)) %>% mutate(Scenario = Area)

# Area


# chose which areas to plot
chosen_increments <- c(
  round((nrow(g1_2020))*.001),
  round((nrow(g1_2020))*.002),
  round((nrow(g1_2020))*.005),
  round((nrow(g1_2020))*.01)
  , round((nrow(g1_2020))*.03)
  , round((nrow(g1_2020))*.04)
  , round((nrow(g1_2020))*.05)
  , round((nrow(g1_2020))*.1)
  , round((nrow(g1_2020))*.13)
  , round((nrow(g1_2020))*.16)
  , round((nrow(g1_2020))*.2)
  , round((nrow(g1_2020))*.23)
  , round((nrow(g1_2020))*.26)
  , round((nrow(g1_2020))*.3)
  , round((nrow(g1_2020))*.32)
  , round((nrow(g1_2020))*.35)
  , round((nrow(g1_2020))*.37)
  , round((nrow(g1_2020))*.4)
  , round((nrow(g1_2020))*.41)
  , round((nrow(g1_2020))*.43)
  , round((nrow(g1_2020))*.46)
  , round((nrow(g1_2020))*.5)
  , round((nrow(g1_2020))*.55)
  , round((nrow(g1_2020))*.6)
  , round((nrow(g1_2020))*.7)
  , round((nrow(g1_2020))*.85)
  , round((nrow(g1_2020))*.9)
  , round((nrow(g1_2020))*.95)
  , round((nrow(g1_2020))*.99)
  , round((nrow(g1_2020)))
  # if using cda adjacent on it's own
  , round((nrow(g2_2020))*.75)
  , round((nrow(g2_2020))*.85)
  , round((nrow(g2_2020))*.9)
  , round((nrow(g2_2020))*.95)
  , round((nrow(g2_2020))*.99)
  , round((nrow(g2_2020)))
  # combined area cda and adjacent
  , round((nrow(g1_2020) + nrow(g2_2020))*0.7)
  , round((nrow(g1_2020) + nrow(g2_2020))*0.8)
  , round((nrow(g1_2020) + nrow(g2_2020))*0.85)
  , round((nrow(g1_2020) + nrow(g2_2020))*0.9)
  , round((nrow(g1_2020) + nrow(g2_2020))*0.95)
  , round((nrow(g1_2020) + nrow(g2_2020))*0.99)
  , round((nrow(g1_2020) + nrow(g2_2020)))
  # how far to extend beyond combined area
  , round((nrow(g1_2020) + nrow(g2_2020))*1.1)
  , round((nrow(g1_2020) + nrow(g2_2020))*1.2)
  , round((nrow(g1_2020) + nrow(g2_2020))*1.3)
  , round((nrow(g1_2020) + nrow(g2_2020))*1.4)
  , round((nrow(g1_2020) + nrow(g2_2020))*1.5)
)

(p1 <- ggplot(avoiding_ye_sum %>%
    filter(Scenario %in% areas_to_plot)%>%
    filter(pair_name =="2019-2020")%>%
    filter(ordered %in% chosen_increments)) +
  geom_line(
    aes(x = ordered*grid_scale/1000,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Scenario), size = 1) +
  geom_ribbon(aes(x = ordered*grid_scale/1000,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Scenario), alpha=0.2) +
  # scale_y_log10(
  #   limits = c(1e-10, 1.7),
  #   breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  # scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = c(10, 100, 1000, "10000", "100000")) +
    coord_cartesian(expand = F, ylim = c(0, 0.55)) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  ylab("Mean ratio of YE to halibut") +
  xlab(expression("Total area of cells ("~km^2~")")) +
  ggtitle("B. Minimizing YE ") +
  theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    legend.position = c(0.2, 0.85)
    # legend.position = "none"
    )
)

(p2 <- ggplot(maximize_hal_sum %>%
    filter(Scenario %in% areas_to_plot)%>%
    filter(pair_name =="2019-2020")%>%
    filter(ordered %in% chosen_increments)) +
  geom_line(
    aes(x = ordered*grid_scale/1000,
      y = mean_ye_per_hal, #lty = "solid",
      colour = Scenario), size = 1) +
  geom_ribbon(aes(x = ordered*grid_scale/1000,
    ymin = lwr_mean_ye_hal,
    ymax = upr_mean_ye_hal,
    fill = Scenario), alpha=0.2) +
  # scale_y_log10(
  #   limits = c(1e-10, 1.7),
  #   breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
  # scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = c(10, 100, 1000, "10000", "100000")) +
  coord_cartesian(expand = F, ylim = c(0, 0.55)) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  ylab("Mean ratio of YE to halibut") +
  xlab(expression("Total area of cells ("~km^2~")")) +
  ggtitle("A. Maximizing halibut") +
  theme(
    # axis.title.y = element_blank(), axis.text.y = element_blank(),
    # legend.position = c(0.2, 0.85)
    legend.position = "none"
    )
)

p3 <- ggplot(data.frame(l = "lab", x = 1, y = 1)) +
  geom_text(aes(x, y), label = expression("Total area of cells ("~km^2~")"), colour="grey30") +
  theme_void() +
  coord_cartesian(clip = "off")

p1$labels$x <- p2$labels$x <- " "
(p2 | p1)/p3 + patchwork::plot_layout(heights = c(15,0.25))

ggsave(paste0("figs/expected_ye_to_hal", hal_model, "_both_scenarios_regions2.png"), width = 7, height = 4)

# JUST AVOIDING
(p4 <- ggplot(avoiding_ye_sum %>%
                filter(Scenario %in% areas_to_plot)%>%
                filter(pair_name =="2019-2020")%>%
                filter(ordered %in% chosen_increments)) +
    geom_line(
      aes(x = ordered*grid_scale/1000,
          y = mean_ye_per_hal, #lty = "solid",
          colour = Scenario), size = 1) +
    geom_ribbon(aes(x = ordered*grid_scale/1000,
                    ymin = lwr_mean_ye_hal,
                    ymax = upr_mean_ye_hal,
                    fill = Scenario), alpha=0.2) +
    coord_cartesian(
      ylim = c(0, 0.55),
      expand = F) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    ylab("Yelloweye to halibut ratio") +
    xlab(expression("Total area of cells ("~km^2~")")) +
    labs(tag="B.") +
    theme(
      plot.tag.position = c(0.1, 0.95),
      # axis.title.y = element_blank(), axis.text.y = element_blank(),
      # legend.position = c(0.9, 0.8)
      legend.position = "none"
    )
)


# # flipped ratio
(p5 <- ggplot(avoiding_ye_sum %>%
                filter(Scenario %in% areas_to_plot)%>%
                filter(pair_name =="2019-2020")%>%
                filter(ordered %in% chosen_increments)) +
    geom_line(
      aes(x = ordered*grid_scale/1000,
          y = mean_hal_per_ye, #lty = "solid",
          colour = Scenario), size = 1) +
    geom_ribbon(aes(x = ordered*grid_scale/1000,
                    ymin = lwr_mean_hal_ye,
                    ymax = upr_mean_hal_ye,
                    fill = Scenario), alpha=0.2) +
    # scale_y_continuous(limits = c(0, 750)) +
    coord_cartesian(expand = F) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    ylab("Halibut to yelloweye ratio") +
    xlab(expression("Total area of cells ("~km^2~")")) +
    # ggtitle("B. Minimizing YE ") +
    theme(
      plot.tag.position = c(0.15, 0.9),
      # axis.title.y = element_blank(), axis.text.y = element_blank(),
      # legend.position = c(0.85, 0.75)
      legend.position = "none"
    )
)
#
# p4 <- p4 + labs(tag="A.") + theme(
#   plot.tag.position = c(0.15, 0.9),
#   axis.title.x = element_blank(), axis.text.x = element_blank(),
#   legend.position = c(0.85, 0.75)
#   # legend.position = "none"
# )
#
#
# p5 <- p5 + labs(tag="B.") + theme(
#   plot.tag.position = c(0.15, 0.9),
#   # axis.title.x = element_blank(), axis.text.x = element_blank(),
#   # legend.position = c(0.85, 0.75)
#   legend.position = "none"
# )
#
# # p4$labels$x <- " "
# # p4$theme$axis.text.x <- p1$theme$axis.text.y
# (p4/p5) + patchwork::plot_layout()
#
# ggsave(paste0("figs/expected_ye_to_hal", hal_model, "_both_ratios_regions2.png"), width = 5, height = 7)


(p6 <- ggplot(avoiding_ye_sum %>%
                filter(Scenario %in% areas_to_plot)%>%
                filter(pair_name =="2019-2020")%>%
                filter(ordered %in% chosen_increments)) +
    geom_line(
      aes(x = ordered*grid_scale/1000,
          y = total_hal/1000, #lty = "solid", # /1000 to convert to tonnes, x10 for 1000 hooks per km^2
          colour = Scenario), size = 1) +
    geom_ribbon(aes(x = ordered*grid_scale/1000,
                    ymin = lwr_hal/1000,
                    ymax = upr_hal/1000,
                    fill = Scenario), alpha=0.2) +
    # scale_y_log10(limits = c(1e-10, )) +
    # scale_y_log10(
    #   breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, "1000")
    #   ) +
    coord_cartesian(expand = F, ylim = c(0, 120)) +
    labs(tag="C.")+
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    ylab("Cumulative halibut") +
    xlab(expression("Total area of cells ("~km^2~")")) +
    # ggtitle("B. Minimizing YE ") +
    theme(
      plot.tag.position = c(0.15, 0.9),
      # axis.title.y = element_blank(), axis.text.y = element_blank(),
      legend.position = c(0.2, 0.8)
      # legend.position = "none"
    )
)

(p7 <- ggplot(avoiding_ye_sum %>%
                filter(Scenario %in% areas_to_plot)%>%
                filter(pair_name =="2019-2020")%>%
                filter(ordered %in% chosen_increments)) +
    geom_line(
      aes(x = ordered*grid_scale/1000,
          y = total_ye/1000, #lty = "solid", # /1000 to convert to tonnes, x10 for 1000 hooks per km^2
          colour = Scenario), size = 1) +
    geom_ribbon(aes(x = ordered*grid_scale/1000,
                    ymin = lwr_ye/1000,
                    ymax = upr_ye/1000,
                    fill = Scenario), alpha=0.2) +
    # scale_y_log10(limits = c(1e-10, )) +
    # scale_y_log10(
    #   breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, "1000")
    #   ) +
    coord_cartesian(expand = F, ylim = c(0, 30)) +
    # labs(tag="D.")+
    labs(tag="(d)")+
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    ylab("Cumulative yelloweye") +
    xlab(expression("Total area of cells ("~km^2~")")) +
    # ggtitle("B. Minimizing YE ") +
    theme(
      plot.tag.position = c(0.15, 0.9),
      # axis.title.y = element_blank(), axis.text.y = element_blank(),
      # legend.position = c(0.2, 0.8)
      legend.position = "none"
    )
)


# p4 <- p4 + labs(tag="C.") + theme(
p4 <- p4 + labs(tag="(c)") + theme(
  plot.tag.position = c(0.15, 0.9),
  axis.title.x = element_blank(), axis.text.x = element_blank(),
  # legend.position = c(0.85, 0.75)
  legend.position = "none"
)

# p5 <- p5 + labs(tag="D.") + theme(
p5 <- p5 + labs(tag="(d)", colour = "Fishing scenario", fill = "Fishing scenario") + theme(
  # axis.title.x = element_blank(), axis.text.x = element_blank()
  legend.position = c(0.85, 0.75)
  # legend.position = "none"
)

# p6 <- p6 + labs(tag="B.") + theme(
p6 <- p6 + labs(tag="(b)") + theme(
  axis.title.x = element_blank(), axis.text.x = element_blank(),
  # legend.position = c(0.85, 0.3)
  legend.position = "none"
)

# p7 <- p7 + labs(tag="A.") + theme(
p7 <- p7 + labs(tag="(a)") + theme(
  axis.title.x = element_blank(), axis.text.x = element_blank()
  )

(p7/p6/p4/p5) + patchwork::plot_layout()

ggsave(paste0("figs/expected_ye_to_hal", hal_model, "_w_cumulative_totals_500hooks_smalltags.png"), width = 5.5, height = 10)


#
# # mean_ye_hal
# ggplot(avoiding_ye_sum %>%
#          filter(Scenario %in% areas_to_plot)%>%
#          filter(ordered %in% chosen_increments)) +
#   geom_line(
#     aes(x = ordered*grid_scale/1000,
#         y = mean_ye_per_hal, #lty = "solid",
#         colour = Scenario), size = 1) +
#   geom_ribbon(aes(x = ordered*grid_scale/1000,
#                   ymin = lwr_mean_ye_hal,
#                   ymax = upr_mean_ye_hal,
#                   fill = Scenario), alpha=0.2) +
#   scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
#   # scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
#   # scale_x_continuous(breaks = c(0, 500, 1000)) +
#   # coord_cartesian(ylim = c(0.00000001, 0.15)) +
#   # scale_fill_brewer(palette = "Set1", direction = 1) + scale_colour_brewer(palette = "Set1", direction = 1) +
#   scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#   ylab("Mean ratio of YE to halibut") +
#   xlab(expression("Total area of cells selected to minimize YE ("~km^2~")")) +
#   facet_wrap(~pair_name, ncol = 4)+
#   theme(legend.position = c(0.85, 0.2))
#
# ggsave(paste0("figs/expected_YE_when_avoiding_YE_CI", ye_model, "_regions.png"), width = 6.5, height = 3.5)
#
# # flipped ratio
# ggplot(avoiding_ye_sum %>%
#          filter(Scenario %in% areas_to_plot)%>%
#          filter(ordered %in% chosen_increments)) +
#   geom_line(
#     aes(x = ordered*grid_scale/1000,
#         y = mean_hal_per_ye, #lty = "solid",
#         colour = Scenario), size = 1) +
#   geom_ribbon(aes(x = ordered*grid_scale/1000,
#                   ymin = lwr_mean_hal_ye,
#                   ymax = upr_mean_hal_ye,
#                   fill = Scenario), alpha=0.2) +
#   scale_y_log10(
#     breaks = c(0.5, 1, 10, 100, 1000, 10000, 100000), labels = c(0.5, 1, 10, 100, 1000, "10000", "100000")
#   ) +
#   # scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
#   # scale_fill_brewer(palette = "Set1", direction = 1) + scale_colour_brewer(palette = "Set1", direction = 1) +
#   scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#   ylab("Mean ratio of halibut to YE (ratio + 1)") +
#   xlab(expression("Total area of cells selected to minimize YE ("~km^2~")")) +
#   # facet_wrap(~pair_name, ncol = 2)+
#   # theme(legend.position = c(0.65, 0.1))
#   facet_wrap(~pair_name, ncol = 4)+
#   theme(legend.position = c(0.85, 0.2))
#
# ggsave(paste0("figs/expected_hal_when_avoiding_YE_CI", ye_model, "_regions.png"), width = 6.5, height = 3.5)
#
# ggplot(maximize_hal_sum %>%
#          filter(Scenario %in% areas_to_plot)%>%
#          filter(ordered %in% chosen_increments)) +
#   geom_line(
#     aes(x = ordered*grid_scale/1000,
#         y = mean_ye_per_hal, #lty = "solid",
#         colour = Scenario), size = 1) +
#   geom_ribbon(aes(x = ordered*grid_scale/1000,
#                   ymin = lwr_mean_ye_hal,
#                   ymax = upr_mean_ye_hal,
#                   fill = Scenario), alpha=0.2) +
#   scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
#   scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
#   # scale_fill_brewer(palette = "Set1", direction = 1) + scale_colour_brewer(palette = "Set1", direction = 1) +
#   scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#   ylab("Mean ratio of YE to halibut") +
#   xlab("Total area of cells selected to maximize halibut (km2)") +
#   facet_wrap(~pair_name, ncol = 4)+
#   theme(legend.position = c(0.85, 0.2))
#
# ggsave(paste0("figs/expected_YE_when_maximize_hal_CI", ye_model, "_regions.png"), width = 6.5, height = 3.5)
#
# # mean_ye_hal
# ggplot(maximize_hal_sum %>%
#          filter(Scenario %in% areas_to_plot)%>%
#          filter(ordered %in% chosen_increments)) +
#   geom_line(
#     aes(x = ordered*grid_scale/1000,
#         y = mean_hal_per_ye, #lty = "solid",
#         colour = Scenario), size = 1) +
#   geom_ribbon(aes(x = ordered*grid_scale/1000,
#                   ymin = lwr_mean_hal_ye,
#                   ymax = upr_mean_hal_ye,
#                   fill = Scenario), alpha=0.2) +
#   scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5), labels = c("0.0001", 0.001, 0.01, 0.1, 0.5)) +
#   scale_x_log10(breaks = c( 10, 100, 1000, 10000, 100000), labels = c( 10, 100, 1000, "10000", "100000")) +
#   # scale_fill_brewer(palette = "Set1", direction = 1) + scale_colour_brewer(palette = "Set1", direction = 1) +
#   scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#   ylab("Mean ratio of halibut to YE") +
#   xlab("Total area of cells selected to maximize halibut (km2)") +
#   facet_wrap(~pair_name, ncol = 4)+
#   theme(legend.position = c(0.85, 0.2))
#
# ggsave(paste0("figs/expected_hal_when_maximize_hal_CI", ye_model, "_regions.png"), width = 6.5, height = 3.5)
#
