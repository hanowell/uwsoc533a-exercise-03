# SOC/CS&SS/CSDE 533 A Winter Quarter 2022
# Assignments: Exercise 2
# Data-generating script

## Setup ----
library(dplyr)
library(HMDHFDplus)
library(readr)
library(tibble)

## Question 1 ----
n0 <- 2666089
nt <- c(n0)
i <- 0
while (i < 15) {
  n <- nt[length(nt)] * exp(abs(rnorm(1, 0.02, 0.01)))
  nt <- c(nt, n)
  i <- i + 1
}
nt <- nt[-1]
ntable <- tibble::tibble(year = 2018:2022,
                         start = nt[seq(1, length(nt), 3)],
                         mid = nt[seq(2, length(nt), 3)],
                         end = nt[seq(3, length(nt), 3)])
readr::write_csv(ntable, "data/q01_data.csv")

# Question 3 ----
q03_data <- tibble::tribble(
  ~area,             ~births_k, ~cbr_k,
  "Africa",          140575,    38.0, 
  "Asia",            389765,    21.9,
  "Europe",          37465,     10.3,
  paste(
    "Latin America",
    "and the",
    "Caribbean"
  ),                 57770,     23.1,
  paste(
    "Northern",
    "America"
  ),                 20860,     13.8,
  "Oceania",         2635,      17.9
) %>%
  readr::write_csv("data/q03_data.csv")

## Question 4 ----
promo1_daily <- 0.5
promo2_daily <- promo1_daily / 2
hire_year_dist <- c(5:1) %>% magrittr::divide_by(sum(.))
hire_month_dist <-
  c(rep(1, 3), rep(2, 3), rep(3, 3), rep(1, 3)) %>%
  magrittr::divide_by(sum(.))
get_date <- function(year, month, day) {
  paste(year, month, day, sep = "-")
}
random_day <- function(year, month) {
  if (month < 12) {
    next_month <- month + 1
    next_month_year <- year
  } else {
    next_month <- 1
    next_month_year <- year + 1
  }
  seq(
    from = as.Date(get_date(year, month, "01")),
    to = as.Date(get_date(next_month_year, next_month, "01")) - 1,
    by = "day"
  ) %>%
    sample(size = 1)
}
source_url <- paste0("https://raw.githubusercontent.com/",
                     "teuschb/hr_data/master/datasets/",
                     "turnover_babushkin.csv")
employees <- url(source_url) %>%
  readr::read_csv(col_types = cols()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    hire_year = sample(1988:1992, size = 1, prob = hire_year_dist),
    hire_month = sample(1:12, size = 1, prob = hire_month_dist),
    hire_date = random_day(hire_year, hire_month),
    promo1_years = rexp(1, promo1_daily),
    promo1_months = promo1_years * 12,
    promo1_days = promo1_years * 365.25,
    promo1_yet = promo1_months < tenure,
    promo1_date = dplyr::if_else(
      promo1_yet,
      as.character(hire_date + floor(promo1_days)),
      NA_character_
    ) %>% as.Date(),
    promo2_years = dplyr::case_when(
      promo1_yet ~ rexp(1, promo2_daily),
      !promo1_yet ~ NA_real_
    ),
    promo2_months = promo2_years * 12,
    promo2_days = promo2_years * 365.25,
    promo2_yet = promo2_months < (tenure - promo1_months),
    promo2_date = dplyr::if_else(
      promo2_yet,
      as.character(promo1_date + floor(promo2_days)),
      NA_character_
    ) %>% as.Date()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    tenure_years = tenure / 12,
    tenure_days = tenure_years * 365.25,
    termination_date = if_else(
      left_company == 1,
      as.character(hire_date + ceiling(tenure_days) + 1),
      NA_character_
    ) %>% as.Date(),
    id = dplyr::row_number()
  ) %>%
  dplyr::select(id, ends_with("_date")) %>%
  readr::write_csv("data/q04_data.csv")

## Question 6 ----
read_hmd_country <- function(CNTRY, item) {
  HMDHFDplus::readHMDweb(
    CNTRY = CNTRY,
    item = item,
    username = keyring::key_list("human-mortality-database")$username,
    password = keyring::key_get(
      service = "human-mortality-database",
      username = keyring::key_list("human-mortality-database")$username
    )
  )
}
q06_data <- read_hmd_country("SVN", "Deaths_5x1") %>%
  dplyr::select(Year, Age, Total) %>%
  setNames(nm = c("year", "age", "deaths")) %>%
  dplyr::filter(year == 1988) %>%
  dplyr::left_join(
    read_hmd_country("SVN", "Exposures_5x1") %>%
      dplyr::select(Year, Age, Total) %>%
      setNames(nm = c("year", "age", "exposure")) %>%
      dplyr::filter(year == 1988)
  ) %>%
  dplyr::select(age, deaths, exposure) %>%
  dplyr::mutate(age = if_else(age < 80L, age, 80L)) %>%
  dplyr::group_by(age) %>%
  dplyr::summarize_at(vars(deaths, exposure), sum) %>%
  dplyr::ungroup() %>%
  readr::write_csv("data/q06_data.csv")
