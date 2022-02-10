# libraries --------------------------------------------------------------------
library(broom)
library(glue)
library(gratia)
library(here)
library(scales)
library(tidyverse)

# # libraries --------------------------------------------------------------------
# library(broom)
# library(future)
# library(glue)
# library(gratia)
# library(here)
# library(mgcv)
# library(parallel)
# library(scales)
# library(tidyverse)
# 
# # options ----------------------------------------------------------------------
# options(scipen = 999)
# plan("future::multisession")
# 
# # data -------------------------------------------------------------------------
# df_school_closure_wave <- df_global |> 
#   select(date, country, schools_closed) |> 
#   group_by(country) |> 
#   mutate(schools_closure_day = schools_closed - lag(schools_closed)) |> 
#   filter(schools_closure_day == 1) |> 
#   mutate(schools_closure_wave = row_number())
# 
# df_global_school <- df_school_closure_wave |> 
#   full_join(df_global, by = c("date", "country", "schools_closed")) |> 
#   group_by(country) |> 
#   arrange(date) |> 
#   fill(schools_closure_wave, .direction = "down") |> 
#   drop_na(schools_closure_wave) |> 
#   group_by(country, schools_closure_wave) |> 
#   mutate(schools_time_since_closure = row_number()) |> 
#   ungroup()
# 
# df_global_school_age <- df_global_school |> 
#   select(date, country, schools_closed, schools_closure_day, schools_closure_wave, schools_time_since_closure, who_region) |> 
#   full_join(df_global_age, by = c("date", "country"))
# 
# df_global_school_length <- df_global_school |> 
#   filter(schools_closed == 1) |> 
#   count(country, schools_closure_wave) |> 
#   rename(schools_closure_wave_n = n)
# 
# df_global_school_age_long <- df_global_school_length |> 
#   right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |> 
#   filter(
#     schools_closure_wave_n >= days_min_closure,
#     schools_time_since_closure <= days_after_closure,
#     age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")
#   )
# 
# negative_daily_case_wave <- df_global_school_age |> 
#   filter(daily_cases < 0) |> 
#   distinct(country, schools_closure_wave)
# 
# missing_daily_case_wave <- df_global_school_age_long |> 
#   group_by(country, age_grp, schools_closure_wave) |> 
#   summarise(n_missing = sum(is.na(daily_cases))) |> 
#   filter(n_missing != 0) |> 
#   distinct(country, schools_closure_wave)
# 
# df_global_school_age_long_clean <- df_global_school_age_long |> 
#   anti_join(negative_daily_case_wave, by = c("country", "schools_closure_wave")) |> 
#   anti_join(missing_daily_case_wave, by = c("country", "schools_closure_wave")) 
# 
# df_global_school_age_long_euro <- df_global_school_age_long_clean |> 
#   filter(who_region == "EURO")
# 
# df_gam <- df_global_school_age_long_euro |>  
#   mutate(
#     age_grp = factor(age_grp),
#     country = factor(country)
#   )
# 
# # model ------------------------------------------------------------------------
# system.time(
#   gam_model <-
#     bam(
#       daily_cases ~ age_grp * country +
#         s(schools_time_since_closure, by = age_grp) +
#         s(schools_time_since_closure, by = country) +
#         s(schools_time_since_closure, schools_closure_wave, country, bs = "fs"),
#       data = df_gam,
#       family = nb(),
#       method = "fREML",
#       control = list(nthreads = detectCores())
#     )
# ); summary(gam_model)
# 
# saveRDS(gam_model, here("results/gam_model.rds"))

gam_model <- readRDS(here("results/gam_model.rds"))

gam_df <- gam_model |> 
  smooth_estimates() |> 
  add_confint()
# statistics -------------------------------------------------------------------
gam_summary <- summary(gam_model)

gam_results <- 
  tidy(gam_model) |> 
  mutate(
    term = term |> 
      str_remove("s\\(schools_time_since_closure\\):age_grpaged") |> 
      str_remove("s\\(schools_time_since_closure\\):country"),
    p.value = pvalue(
      p.value, 
      accuracy = 0.001, 
      add_p = FALSE, 
      prefix = c("< ", "= ", "> ")
      ),
    across(where(is.numeric), round, 2),
    print = glue("$\\chi^2({edf}) = {statistic}$, $p {p.value}$")
  ) |> 
  select(term, print) |> 
  deframe()

# visualisation ----------------------------------------------------------------
gam_plot_age <- gam_df |>
  filter(by == "age_grp") |>
  mutate(
    smooth = smooth |>
      str_remove("s\\(schools_time_since_closure\\):age_grpaged") |>
      factor(levels = c("0to4", "5to9", "10to14", "15to19", "20to24"))
  ) |>
  ggplot(aes(x = schools_time_since_closure, color = smooth)) +
  geom_line(aes(y = est)) +
  scale_x_continuous(
    "Day Since Closure",
    breaks = seq(0, 28, by = 7)
  ) +
  scale_y_continuous(
    "Standardized Effect"
  ) +
  scale_color_manual(
    "Age Group",
    values = c("#a6611a", "#dfc27d", "#bababa", "#80cdc1", "#018571"),
    labels = c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10, family = "serif")
  )