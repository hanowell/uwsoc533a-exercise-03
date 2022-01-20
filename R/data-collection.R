# SOC/CS&SS/CSDE 533 A Winter Quarter 2022
# Assignments: Exercise 2
# Data-generating script

## Setup ----
library(dplyr)
library(readxl)
library(tidyr)

## UN World Poplation Prospects 2019 ----

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
