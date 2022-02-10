# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)

# # variables --------------------------------------------------------------------
age_order <- c(
  "aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24",
  "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65+"
)
age_name <- c(
  "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 34",
  "35 to 44",   "45 to 54",   "55 to 64",   "65 and up"
)

countries_of_interest <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Estonia", "France",
  "Germany", "Greece", "Netherlands", "Portugal", "Slovakia", "Spain"
)

# # libraries --------------------------------------------------------------------
# library(future)
# library(here)
# library(tidyverse)
# library(RTransferEntropy)
# 
# # options ----------------------------------------------------------------------
# options(scipen = 999)
# plan("future::multisession")
# 
# # data -------------------------------------------------------------------------
# df_wide_changes <- df_global_age |>
#   select(date, country, age_grp, daily_changes) |>
#   pivot_wider(names_from = age_grp, values_from = daily_changes)
# 
# # transfer entropy -------------------------------------------------------------
# te_agegroup <- function(var_x, var_y, country_select){
#   df <- df_wide_changes |>
#     filter(country == country_select)
#   var_x <- select(df, all_of(var_x))
#   var_y <- select(df, all_of(var_y))
#   transfer_entropy(var_x, var_y, quiet = TRUE)[["coef"]] |>
#     as_tibble() |>
#     mutate(direction = c("X->Y", "Y->X"))
# }
# 
# te_results <-
#   combinations(
#     n = length(age_order),
#     r = 2,
#     v = age_order,
#     repeats.allowed = FALSE
#   ) |>
#   as_tibble(.name_repair = ~ c("X", "Y")) |>
#   expand_grid(country = countries_of_interest) |>
#   group_by(X, Y, country) |>
#   summarise(te_agegroup(X, Y, country)) |>
#   ungroup()
# 
# te_results_XY <- te_results |>
#   filter(direction == "X->Y")
# 
# te_results_YX <- te_results |>
#   filter(direction == "Y->X") |>
#   rename(X = Y, Y = X)
# 
# te_results <- bind_rows(te_results_XY,te_results_YX)
# 
# write_csv(te_results, here("results/te_results.csv"))

te_results <- read_csv(here("results/te_results.csv"))

te_plot <- te_results |> 
  mutate(
    X = factor(X, levels = age_order),
    Y = factor(Y, levels = age_order),
    p_stars = unclass(symnum(`p-value`, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", " ")))
  ) |>
  arrange(X, Y) |>
  ggplot(aes(X, Y)) + 
  geom_tile(aes(fill = te)) + 
  geom_vline(xintercept = 5.5, linetype = "dotted") +
  geom_hline(yintercept = 5.5, linetype = "dotted") +
  #geom_text(aes(label = p_stars)) +
  scale_x_discrete("Influencing Age Groups", labels = age_name) +
  scale_y_discrete("Influenced Age Groups", labels = age_name) +
  scale_fill_gradient("Transfer Entropy", low = "white", high = "red") +
  facet_wrap(~ country, nrow = 4) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.background = element_rect(colour = "black", size = 0.1)
  )
