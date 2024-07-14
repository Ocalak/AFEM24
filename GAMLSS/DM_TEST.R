#GET_PB Score
read_csv("~/Desktop/PB_SCORE_TEST.csv")#PB_TEST


# DM test function
dm_test <- function(error_a, error_b, hmax = 1, power = 1) {
  ## as dm_test with alternative == "less"
  loss_a <- apply(abs(as.matrix(error_a))^power, 1, sum)^(1 / power)
  loss_b <- apply(abs(as.matrix(error_b))^power, 1, sum)^(1 / power)
  delta <- loss_a - loss_b
  delta_var <- var(delta) / length(delta) ## estimation of the variance
  statistic <- mean(delta, na.rm = TRUE) / sqrt(delta_var)
  delta_length <- length(delta)
  k <- ((delta_length + 1 - 2 * hmax +
           (hmax / delta_length) * (hmax - 1)) / delta_length)^(1 / 2)
  statistic <- statistic * k
  p_value <- pt(statistic, df = delta_length - 1)
  list(stat = statistic, p.val = p_value)
}

errors_wo_true <- PB_SCORE_TEST
model_names <- names(PB_SCORE_TEST)
model_names_wo_true <- paste(seq_along(model_names),
                             model_names,
                             sep = "_"
)

dm_results_df <- data.frame(
  matrix(
    ncol = length(model_names_wo_true),
    nrow = length(model_names_wo_true)
  )
)

dimnames(dm_results_df) <- list(model_names_wo_true, model_names_wo_true)

dm_results <- list(
  "p_val" = dm_results_df,
  "t_stat" = dm_results_df
)

dm_results$p_val[, "type"] <- "p_val"
dm_results$t_stat[, "type"] <- "t_stat"

for (mod_a in seq_along(model_names_wo_true)) {
  for (mod_b in seq_along(model_names_wo_true)) {
    if (mod_a == mod_b) {
      dm_results[["p_val"]][mod_a, mod_b] <- NA
      dm_results[["t_stat"]][mod_a, mod_b] <- NA
    } else {
      dm <- dm_test(
        errors_wo_true[, mod_a],
        errors_wo_true[, mod_b]
      )
      dm_results[["p_val"]][mod_a, mod_b] <- dm$p.val
      dm_results[["t_stat"]][mod_a, mod_b] <- dm$stat
    }
  }
}

dm_results_tible <- dm_results %>%
  # We rbind both dataframes together
  reduce(.f = rbind) %>%
  # Convert them to a tibble. Its basically a nice dataframe
  as_tibble() %>%
  # Add a columns mod_a
  mutate(mod_a = c(
    model_names_wo_true,
    model_names_wo_true
  )) %>%
  # And bring the tibble to into a longer format
  pivot_longer(!c(type, mod_a),
               names_to = "mod_b", values_to = "val"
  ) %>%
  # Finally bring the tibble to into a wider format wrt type
  pivot_wider(names_from = type, values_from = val)

# Now we are ready to plot our results using ggplot2

# P-values
dm_results_tible %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = p_val)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "Statistic",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 10,
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  )+labs(x=" Models",y=" ",)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
