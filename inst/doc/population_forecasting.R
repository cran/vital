## ----include = FALSE----------------------------------------------------------
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

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(vital)
library(fable)
library(dplyr)
library(ggplot2)
set.seed(2025)

## ----mortality, fig.cap="", fig.alt="First two components of the functional data model for mortality rates."----
fit_mortality <- norway_mortality |>
  filter(Sex != "Total") |>
  smooth_mortality(Mortality) |>
  make_pr(.smooth) |>
  model(fdm = FDM(log(.smooth), coherent = TRUE))
autoplot(fit_mortality, 2)

## ----fertility, fig.cap="", fig.alt = "Fitted values of the functional mean model for fertility rates."----
fit_fertility <- norway_fertility |>
  filter(Year > 2010) |>
  smooth_fertility(Fertility) |>
  model(fmean = FMEAN(sqrt(.smooth)))
autoplot(fit_fertility)

## ----migration, fig.cap="", fig.alt="First two components of the functional data model for net migration."----
netmig <- net_migration(
  norway_mortality |> filter(Sex != "Total"),
  norway_births
) |>
  make_sd(NetMigration)
fit_migration <- netmig |>
  model(fdm = FDM(NetMigration, coherent = TRUE))
autoplot(fit_migration)

## ----simulation---------------------------------------------------------------
pop <- norway_mortality |>
  filter(Sex != "Total", Year == max(Year))
future <- generate_population(
  starting_population = pop,
  mortality_model = fit_mortality,
  fertility_model = fit_fertility,
  migration_model = fit_migration,
  h = 10,
  n_reps = 500
)

## ----population_plot, fig.cap="", fig.alt="Simulated population for the first replicate."----
future |>
  filter(.rep == "100") |>
  ggplot(aes(x = Age, y = Population, group = Year, color = Year)) +
  geom_line(
    data = norway_mortality |> filter(Year > 2010, Sex != "Total"),
    color = "grey",
    mapping = aes(group = Year)
  )  +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10)[1:9]) +
  facet_grid(. ~ Sex)

## ----mean_age-----------------------------------------------------------------
future |>
  group_by(Sex, .rep) |>
  summarise(mean_age = sum(Population * (Age + 0.5)) / sum(Population)) |>
  group_by(Sex) |>
  summarise(mean_age = mean(mean_age))

## ----population_pyramid, fig.cap="", fig.alt="Population pyramid for 2032 with 95% prediction intervals."----
pyramid_2032 <- future |>
  filter(Year == 2032) |>
  mutate(Population = if_else(Sex == "Female", -Population, Population)) |>
  group_by(Age, Sex) |>
  summarise(
    lo = quantile(Population, 0.025),
    med = quantile(Population, 0.5),
    hi = quantile(Population, 0.975)
  )
pyramid_2032 |>
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, colour = NULL),
    fill = "#c14b14", alpha = 0.2
  ) +
  geom_line(aes(y = med), color = "#c14b14") +
  facet_grid(. ~ Sex, scales = "free_x") +
  labs(y = "Population") +
  coord_flip() +
  guides(fill = "none", alpha = "none")

