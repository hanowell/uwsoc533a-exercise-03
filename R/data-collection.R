# SOC/CS&SS/CSDE 533 A Winter Quarter 2022
# Assignments: Exercise 2
# Data-generating script

## Setup ----
library(dplyr)
library(readxl)
library(tibble)
library(tidyr)

## Question 1 ----

### Age-specific population counts
popage0_by_income <-
  "raw/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx" %>%
  readxl::read_excel(
    sheet = "ESTIMATES",
    skip = 17,
    na = "..."
  ) %>%
  dplyr::select(3, 8, 9) %>%
  setNames(nm = c("location", "period", "size_age0")) %>%
  dplyr::filter(
    location %in% c("Low-income countries", "High-income countries"),
    period %>% dplyr::between(2015,2020)
  ) %>%
  dplyr::rename(income_level = location)
pop_by_income_age5yr <-
  "raw/WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx" %>%
  readxl::read_excel(
    sheet = "ESTIMATES",
    skip = 17,
    na = "..."
  ) %>%
  setNames(nm = c(
    "index",
    "variant",
    "location",
    "notes",
    "country_code",
    "type",
    "parent_code",
    "period",
    "x0_4",
    "x5_9",
    "x10_14",
    "x15_19",
    "x20_24",
    "x25_29",
    "x30_34",
    "x35_39",
    "x40_44",
    "x45_49",
    "x50_54",
    "x55_59",
    "x60_64",
    "x65_69",
    "x70_74",
    "x75_79",
    "x80_84",
    "x85_89",
    "x90_94",
    "x95_99",
    "x100"
  )) %>%
  dplyr::filter(
    location %in% c("Low-income countries", "High-income countries"),
    period %>% dplyr::between(2015,2020)
  ) %>%
  dplyr::rename(income_level = location) %>%
  dplyr::select(income_level, period, starts_with("x")) %>%
  tidyr::pivot_longer(
    starts_with("x"),
    names_to = c("x", "xpn"),
    names_prefix = "x",
    names_sep = "_",
    values_to = "size"
  ) %>%
  dplyr::mutate(x = as.numeric(x)) %>%
  dplyr::select(income_level, period, x, size)
popage1_by_income <- pop_by_income_age5yr %>%
  dplyr::filter(x == 0) %>%
  dplyr::left_join(popage0_by_income) %>%
  dplyr::mutate(size_age1 = size - size_age0, x = 1) %>%
  dplyr::select(income_level, period, x, size_age1)
popstruct_by_income <- dplyr::bind_rows(
  popage0_by_income %>%
    dplyr::rename(size = size_age0) %>%
    dplyr::mutate(x = 0),
  popage1_by_income %>%
    dplyr::rename(size = size_age1) %>%
    dplyr::mutate(x = 1),
  pop_by_income_age5yr %>%
    dplyr::filter(x > 1)
) %>%
  dplyr::group_by(income_level, period) %>%
  dplyr::mutate(cx = size / sum(size)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(income_level, x) %>%
  dplyr::summarize(cx = mean(cx)) %>%
  dplyr::ungroup()

### Life tables
asmr_by_income <-
  "raw/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx" %>%
  readxl::read_excel(
    sheet = "ESTIMATES",
    col_names = FALSE,
    skip = 17,
    na = "..."
  ) %>%
  setNames(nm = c(
    "index",
    "variant",
    "location",
    "notes",
    "country_code",
    "type",
    "parent_code",
    "period",
    "x",
    "n",
    "mx",
    "qx",
    "px",
    "lx",
    "dx",
    "Lx",
    "Sx",
    "Tx",
    "ex",
    "ax"
  )) %>%
  dplyr::filter(
    location %in% c("Low-income countries", "High-income countries"),
    period == "2015-2020"
  ) %>%
  dplyr::rename(income_level = location) %>%
  dplyr::select(income_level, x, mx)

q01_data <- asmr_by_income %>%
  dplyr::left_join(popstruct_by_income)
saveRDS(q01_data, "data/q01_data.rds")

## Question 2 ----

base_rate <- 0.3
log_base_rate <- log(base_rate)
website_effects <- tibble::tribble(
  ~website,  ~website_effect,
  "main",    1.0,
  "partner", 0.5
) %>%
  dplyr::mutate(website_effect = log(website_effect))
channel_effects <- tibble::tribble(
  ~channel,         ~channel_effect,
  "TV",             1.00,
  "radio",          0.10,
  "podcast",        1.05,
  "paid search",    0.07,    
  "organic search", 1.5
) %>%
  dplyr::mutate(channel_effect = log(channel_effect))
q02_data <- tidyr::expand_grid(
  website = website_effects[["website"]],
  channel = channel_effects[["channel"]]
) %>%
  dplyr::left_join(website_effects) %>%
  dplyr::left_join(channel_effects) %>%
  dplyr::mutate(rate = exp(log_base_rate + website_effect + channel_effect)) %>%
  dplyr::mutate(
    cx = c(1.0, 1.1, 0.7, 1.1, 1.2, # main website
           1.0, 1.1, 0.7, 1.1, 0.3),
    rate = dplyr::case_when(
      website == "partner" & channel == "organic search" ~ rate * 2.3,
      TRUE ~ rate
    )
  ) %>%
  dplyr::group_by(website) %>%
  dplyr::mutate(cx = cx / sum(cx)) %>%
  dplyr::ungroup() %>%
  dplyr::select(website, channel, rate, cx)
saveRDS(q02_data, "data/q02_data.rds")

## Question 5 ----

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
q05_data <- url(source_url) %>%
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
  dplyr::select(id, ends_with("_date"))
saveRDS(q05_data, "data/q05_data.rds")

# Question 6 ----
q06_data <- tibble::tribble(
  ~year, ~manufactured, ~failed,
  2018,  320276,              115,
  2019,  540782,              209,
  2020,  316001,              103
)
saveRDS(q06_data, "data/q06_data.rds")
