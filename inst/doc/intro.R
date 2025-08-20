## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  eval = nzchar(Sys.getenv("VIGNETTES")), # Only compile locally
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  out.width = "100%"
)

# Okabe-Ito colours for discrete scales
options(
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)

## ----packages, message = FALSE, eval = TRUE-----------------------------------
library(vital)
library(tsibble)
library(dplyr)
library(ggplot2)

## ----example, eval = TRUE-----------------------------------------------------
nor <- norway_mortality |>
  filter(Sex != "Total") |>
  collapse_ages(max_age = 100)
nor

## ----vars---------------------------------------------------------------------
index_var(nor)
key_vars(nor)
vital_vars(nor)

## ----autoplot, warning = FALSE, fig.cap="", fig.alt="Rainbow plot of mortality rates in Norway."----
nor |>
  autoplot(Mortality) +
  scale_y_log10()

## ----pyramid, fig.cap="", fig.alt="Population pyramids for Norway."-----------
nor |>
  mutate(Population = if_else(Sex == "Female", -Population, Population)) |>
  autoplot(Population) +
  coord_flip() +
  facet_grid(. ~ Sex, scales = "free_x")

## ----lifetable----------------------------------------------------------------
# Life tables for males and females in Norway in 2000
nor |>
  filter(Year == 2000) |>
  life_table()

## ----e0, fig.cap="", fig.alt="Life expectancy at birth in Norway."------------
# Life expectancy for males and females in Norway
nor |>
  life_expectancy() |>
  ggplot(aes(x = Year, y = ex, color = Sex)) +
  geom_line()

## ----smoothing, fig.cap="", fig.alt="Smoothed mortality rates in Norway for 1967."----
# Smoothed data
nor |>
  filter(Year == 1967) |>
  smooth_mortality(Mortality) |>
  autoplot(Mortality) +
  geom_line(aes(y = .smooth), col = "#0072B2") +
  ylab("Mortality rate") +
  scale_y_log10()

## ----lc-----------------------------------------------------------------------
# Lee-Carter model
lc <- nor |>
  model(lee_carter = LC(log(Mortality)))
lc

## ----lc2----------------------------------------------------------------------
lc |>
  filter(Sex == "Female") |>
  report()

## ----lc3, fig.cap="", fig.alt="Components from Lee-Carter model."-------------
autoplot(lc)

## ----lccomponents-------------------------------------------------------------
age_components(lc)
time_components(lc)

## ----lc5----------------------------------------------------------------------
# Forecasts from Lee-Carter model
lc |>
  forecast(h = 20)

## ----fdm, fig.cap="", fig.alt="First few components from functional data model for mortality in Norway."----
# FDM model
fdm <- nor |>
  smooth_mortality(Mortality) |>
  model(hu = FDM(log(.smooth)))
fc_fdm <- fdm |>
  forecast(h = 20)
autoplot(fc_fdm) +
  scale_y_log10()

## ----fdmplot, fig.cap="", fig.alt="First three components from functional data model for mortality in Norway."----
fdm |>
  autoplot(show_order = 3)

## ----fdmcomponents------------------------------------------------------------
age_components(fdm)
time_components(fdm)

## ----coherent-----------------------------------------------------------------
fdm_coherent <- nor |>
  smooth_mortality(Mortality) |>
  make_pr(.smooth) |>
  model(hby = FDM(log(.smooth), coherent = TRUE))
fc_coherent <- fdm_coherent |>
  forecast(h = 20) |>
  undo_pr(.smooth)
fc_coherent

