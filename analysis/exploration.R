################################################################################
#            Influence of School Closure on Covid-19 Contamination 
#                     and Repercussion across Age Groups  
################################################################################

# references -------------------------------------------------------------------
"https://towardsdatascience.com/inferring-causality-in-time-series-data-b8b75fe52c46"
"https://jofam.github.io/covidBE_analysis/Overdispersion.html"

# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)

# data -------------------------------------------------------------------------
df <- 
  read_csv(
    here("data/age_groups.csv"), 
    col_types = list(
      .default = col_double(), 
      Date = col_date(format = "%d/%m/%Y")
    )
  ) |> 
  clean_names()

# visual exploration -----------------------------------------------------------

df |> 
  ggplot(aes(date, total)) +
  geom_line() +
  theme_bw()

df |> 
  select(c(date, starts_with("aged"))) |> 
  pivot_longer(-date) |> 
  ggplot(aes(date, value, color = name)) +
  geom_line() +
  theme_bw()

  
         