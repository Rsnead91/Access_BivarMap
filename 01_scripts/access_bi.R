# begin r script ----

# 1. libraries ----

library(tidyverse)
library(biscale)
library(cowplot)
library(tigris)
library(rgdal)
library(sf)


# 2. data ----

options(tigris_use_cache = TRUE)

pa_border <- filter(states(cb = TRUE), STATEFP == "42") # used for map

pa_counties <- counties(state = "42", cb = TRUE, class = "sf") # used for map

pa_bg <- block_groups(state="PA", cb=TRUE, year=2010) %>% mutate(geoid = as.numeric(str_sub(GEO_ID, 10))) # unit of observation

## removing block groups with no population (e.g., area covered by water)
nopopgeo <- c(420179800001, 421019806001, 420459800001, 421019805001, 420490124000, 420039808001, 421019804001, 420039812001)

pa_bg <- pa_bg[ ! pa_bg$geoid %in% nopopgeo, ]

## Due to the data use agreement with the state, I can not provide the cancer data. I am loading the residuals
## and fitted RR (averaged across the ten year period) from the model below. Relative risk is estimated from
## a hierarchical Bayesian regression model that uses Besag-York-Mollie conditional autoregressive priors
## and a first order autoregressive term to account for temporal autocorrelation during the ten year period

load("00_data/inla.residuals.Rdata")

# model <- inla(formula = crc.x ~ f(idarea, model = "bym", graph = nb.b) + f(idarea1, idtime, model = "iid") + idtime + mm2sfca_45_spar + ruca_3_mvu + ruca_3_rvu + crcsadh + age_ge65i + sqrt(blacki) + sqrt(nohsgrad_25plusi) + sqrt(unemployedi) + sqrt(householder45plus_povertyi),
#               family = "poisson", data = ds,
#               control.predictor = list(compute = TRUE),
#               control.compute=list(config=TRUE, dic=TRUE, cpo=TRUE, return.marginals.predictor=TRUE)
# )

pa_joined <- left_join(x=pa_bg, y=inla.residuals, by='geoid') # joining model results with the spatial attributes

bi.resid_v_rr <- bi_class(pa_joined, x = residuals, y = inla.rr.avg, style = "quantile", dim = 3) # assigning bivariate classes to perform the visualization


# 3. Figure ----

## Creating a bivariate choropleth map of the relative risk of colorectal cancer by the spatial residual
## of the model used to estimate risk. This tells us which areas in the state are best and least explained 
## by our predictors.

pa <- ggplot() +
  geom_sf(data = bi.resid_v_rr, mapping = aes(fill = bi_class, color = bi_class), size = 0.1, show.legend = FALSE) +
  geom_sf(data = pa_border, fill = NA, color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  bi_scale_color(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  labs(caption = "Risk of Colorectal Cancer by Spatial Residual in Pennsylvania at the Block Group-level,\nfrom 2008 to 2017") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    title = element_text(family="Arial"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=10.5,hjust=0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )  

philly <- ggplot(filter(bi.resid_v_rr, COUNTY == "101")) +
  geom_sf(mapping = aes(fill = bi_class, color = bi_class), size = 0.1, show.legend = FALSE) +
  geom_sf(data = filter(pa_counties, COUNTYFP == "101"), fill = NA, color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  bi_scale_color(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="none"
  )

pitt <- ggplot(filter(bi.resid_v_rr, COUNTY == "003")) +
  geom_sf(mapping = aes(fill = bi_class, color = bi_class), size = 0.1, show.legend = FALSE) +
  geom_sf(data = filter(pa_counties, COUNTYFP == "003"), fill = NA, color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  bi_scale_color(pal = "DkBlue", dim = 3, flip_axes = TRUE) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="none"
  )

pa <- pa + 
  geom_rect(
    xmin = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "101"))[1],
    ymin = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "101"))[2],
    xmax = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "101"))[3],
    ymax = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "101"))[4],
    fill = NA, 
    colour = "black",
    size = 0.6
  ) + 
  geom_rect(
    xmin = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "003"))[1],
    ymin = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "003"))[2],
    xmax = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "003"))[3],
    ymax = st_bbox(dplyr::filter(bi.resid_v_rr, COUNTY == "003"))[4],
    fill = NA, 
    colour = "black",
    size = 0.6
  )

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher Risk of CRC ",
                    ylab = "Spatial Residual ",
                    size = 6.8, 
                    flip_axes = TRUE)

ggdraw() +
  draw_plot(
    pa, 
    x = 0, y = 0, 
    width = .6, height = 1) +
  draw_plot(
    philly,
    x = 0.43, y = .25,
    width = 0.5, height = 0.5) +
  draw_plot(
    pitt,
    x = 0.5, y = 0.6,
    width = 0.25, height = 0.25) +
  draw_plot(
    legend + 
      theme(
        plot.background = element_rect(fill="transparent", color = NA), 
        panel.background = element_rect(fill="transparent", color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()), 
      .75, .4, 0.3, 0.3) + 
  theme(plot.background = element_rect(fill="transparent", color = NA)) +
  geom_text() +
  annotate(
    "text",
    label = "Philadelphia",
    x = 0.75, y = 0.35,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pittsburgh",
    x = 0.75, y = 0.8,
    family = "Arial", size = 3
  ) + theme()#plot.background = element_rect(fill = "white"))

ggsave("02_output/access_bivarmap.png", height=4, width=6, units="in")

