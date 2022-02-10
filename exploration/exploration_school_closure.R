
# libraries --------------------------------------------------------------------
library(janitor)
library(tidyverse)

# variables --------------------------------------------------------------------
period_closure_to_keep <- c(
  #"Partially open",
  #"Academic break",
  "Closed due to COVID-19"
)

# data -------------------------------------------------------------------------
covid_impact_education <- 
  read_csv(
    "https://en.unesco.org/sites/default/files/covid_impact_education.csv",
    col_types = list(.default = col_character(), Date = col_date(format = "%d/%m/%Y"))) |> 
  clean_names() |>
  select(date, country, status) |> 
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",  
    country == "Central African republic" ~ "Central African Republic",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Republic of Korea" ~ "South Korea",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "United States of America" ~ "USA",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
  )
  )

covid_impact_education_full <- read_csv(
  "https://en.unesco.org/sites/default/files/covid_impact_education_full.csv",
  col_types = list(.default = col_character(), Date = col_date(format = "%d/%m/%Y"))) |> 
  clean_names() |>
  select(date, country, status_test = status) |> 
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",  
    country == "Central African republic" ~ "Central African Republic",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Republic of Korea" ~ "South Korea",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "United States of America" ~ "USA",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
  )
  )

# analysis ---------------------------------------------------------------------
## for some reasons covid_impact_education is shorter than covid_impact_education_full
comparison_closure <- 
  full_join(
    covid_impact_education, 
    covid_impact_education_full, 
    by = c("date", "country")
  ) |> 
  mutate(comp = if_else(status_test == status, 1, 0)) |> 
  filter(is.na(comp))
