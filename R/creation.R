# SOC/CS&SS/CSDE 533 A Winter Quarter 2022
# Assignments: Exercise 2
# Data-generating script

## Setup
library(dplyr)
library(tidycensus)

## Question 1

### Generate King County, WA population estimate data ----

get_king_acs1_totpop <- function(year) {
  tidycensus::get_acs(
    year = year,
    geography = "county",
    variables = "B01003_001",
    state = "Washington",
    county = "King County",
    survey = "acs1"
  )
}
years <- 2006:2019
king_acs1_totpop_05_19 <- years %>%
  purrr::map(function(y) get_king_acs1_totpop(y)$estimate) %>%
  as.numeric() %>%
  tibble::tibble(
    year = years,
    population = .
  ) %>%
  readr::write_csv(
    "assignments/exercises/02_exercise/data/king_acs1_totpop.csv"
  )
