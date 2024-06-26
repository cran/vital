% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norway.R
\name{norway_births}
\alias{norway_births}
\alias{norway_fertility}
\alias{norway_mortality}
\title{Norwegian mortality and births data}
\format{
Time series of class \code{vital}
}
\source{
Human Mortality Database \url{https://mortality.org}

Human Fertility Database \url{https://www.humanfertility.org}
}
\description{
\code{norway_births} is an annual \code{vital} object covering the years 1900-2022, as provided
by the Human Mortality Database on 21 April 2024.

\code{norway_fertality} is an annual \code{vital} covering the years 1967-2022, as provided
by the Human Fertility Database on 21 April 2024.

\code{norway_mortality} is an annual \code{vital} covering the years 1900-2022, as provided
by the Human Mortality Database on 21 April 2024.
}
\examples{
library(ggplot2)
# Births
norway_births
norway_births |>
  autoplot(Births)
# Deaths
norway_mortality
norway_mortality |>
  dplyr::filter(Age < 85, Year < 1950, Sex != "Total") |>
  autoplot(Mortality) +
  scale_y_log10()
# Fertility
norway_fertility
norway_fertility |>
  autoplot(Fertility)

}
\keyword{datasets}
