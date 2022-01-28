
# libraries --------------------------------------------------------------------
library(janitor)
library(tidyverse)

# options ----------------------------------------------------------------------
options(scipen = 999)

# data -------------------------------------------------------------------------
df <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv")

df_clean <- df |> 
  clean_names() |> 
  select(date, age, cases)

# visual exploration -----------------------------------------------------------
df |> 
  group_by(date) |> 
  summarise(total_cases = sum(cases)) |> 
  ggplot(aes(total_cases)) +
  geom_density(na.rm = TRUE) +
  labs(
    title = "Distribution of COVID-19 Daily Cases in Ireland",
    subtitle = "",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Covid Cases", 
    y = "Density"
  ) +
  theme_bw()

df |> 
  group_by(date) |> 
  summarise(total_cases = sum(cases)) |> 
  ggplot(aes(date, total_cases)) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Evolution of COVID-19 Cases in England",
    subtitle = "Raw data",
    caption = "Source: UK Department of Health",
    x = "Date", 
    y = "Covid Cases"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
