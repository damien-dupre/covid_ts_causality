
# libraries --------------------------------------------------------------------
library(gtools)
library(tidyverse)
library(RTransferEntropy)

# variables --------------------------------------------------------------------
age_order <- c("aged1to4", "aged5to14", "aged15to24", "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65to74", "aged75to84", "aged85up")

# solution 1 -------------------------------------------------------------------
# te_agegroup <- function(var_x, var_y){
#   
#   var_x <- select(df_wide_changes, all_of(var_x))
#   var_y <- select(df_wide_changes, all_of(var_y))
#   
#   calc_te(var_x, var_y)
# }
# 
# te_results <-
#   expand_grid(
#     X = age_order, 
#     Y = age_order
#   ) |> 
#   as_tibble() |> 
#   group_by(X, Y) |> 
#   summarise(te = te_agegroup(X, Y))

# te_matrix <- te_results |> 
#   mutate(
#     X = factor(X, levels = age_order),
#     Y = factor(Y, levels = age_order),
#   ) |> 
#   arrange(X, Y) |> 
#   pivot_wider(names_from = Y, values_from = te) |> 
#   column_to_rownames("X") |> 
#   as_tibble()
# 
# ggplot(te_results, aes(X, Y)) + 
#   geom_raster(aes(fill=te)) + 
#   scale_fill_gradient(low="grey90", high="red") +
#   theme_bw()

# solution 2 -------------------------------------------------------------------
library(future) # enable parallel processing for all future transfer_entropy calls
plan(multisession)

te_agegroup <- function(var_x, var_y){
  
  var_x <- select(df_wide_changes, all_of(var_x))
  var_y <- select(df_wide_changes, all_of(var_y))
  
  transfer_entropy(var_x, var_y, quiet = TRUE)[["coef"]] |> 
    as_tibble() |> 
    mutate(direction = c("X->Y", "Y->X"))
}

te_results <-
  combinations(
    n = length(age_order), 
    r = 2, 
    v = age_order, 
    repeats.allowed = FALSE
  ) |> 
  as_tibble(.name_repair = ~ c("X", "Y")) |> 
  group_by(X, Y) |> 
  summarise(te_agegroup(X, Y)) |> 
  ungroup()

te_results_XY <- te_results |>
  filter(direction == "X->Y")

te_results_YX <- te_results |>
  filter(direction == "Y->X") |> 
  rename(X = Y, Y = X)

te_plot <- bind_rows(te_results_XY,te_results_YX) |> 
  mutate(
    X = factor(X, levels = age_order),
    Y = factor(Y, levels = age_order),
    p_stars = stars.pval(`p-value`)
  ) |>
  arrange(X, Y) |>
  ggplot(aes(X, Y)) + 
  geom_tile(aes(fill = te)) + 
  geom_text(aes(label = p_stars)) +
  scale_x_discrete("Influencing Age Groups", labels = age_name) +
  scale_y_discrete("Influenced Age Groups", labels = age_name) +
  scale_fill_gradient("Transfer Entropy", low = "grey90", high = "red") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(colour = "black", size = 0.1)
  )
