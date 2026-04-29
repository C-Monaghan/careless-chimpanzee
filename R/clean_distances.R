clean_distances <- function(distances) {
  distances |>
    pivot_wider(names_from = metric, values_from = value) |>
    pivot_longer(cols = c(aic:KL), names_to = "metric", values_to = "value") |>
    mutate(
      # Cleaning column names
      wave = str_replace(wave, "-", " to "),
      wave = str_glue("Wave {wave}"),

      parent_block = str_to_sentence(parent_block),
      parent_block = str_glue("{parent_block} models"),

      sub_block = case_when(
        sub_block == "null" ~ "Null Model",
        sub_block == "red1" ~ "Reduced Model 1",
        sub_block == "red2" ~ "Reduced Model 2",
        sub_block == "true" ~ "True Model",
        sub_block == "of" ~ "Overfit Model",
      ),

      sample_size = str_glue("n = {sample_size}"),

      metric = case_when(
        metric == "Frobenius" ~ "Frobenius Distance",
        metric == "Manhattan" ~ "Manhattan Distance",
        metric == "Max" ~ "Max Difference",
        metric == "MeanAbs" ~ "Mean Absolute Difference",
        metric == "RMSE" ~ "Root Mean Square Error",
        metric == "Correlation" ~ "Correlation Distance",
        metric == "KL" ~ "Kullback-Leibler Divergence",
        metric == "aic" ~ "AIC",
        metric == "bic" ~ "BIC",
      ),

      # Factorising
      parent_block = factor(
        parent_block,
        levels = c("Base models", "Additive models", "Multiplicative models")
      ),

      sub_block = factor(
        sub_block,
        levels = c(
          "Null Model",
          "Reduced Model 1",
          "Reduced Model 2",
          "True Model",
          "Overfit Model"
        )
      ),

      sample_size = factor(
        sample_size,
        levels = c("n = 100", "n = 250", "n = 500", "n = 1000", "n = 5000")
      ),

      metric = factor(
        metric,
        levels = c(
          "Frobenius Distance",
          "Manhattan Distance",
          "Max Difference",
          "Mean Absolute Difference",
          "Root Mean Square Error",
          "Correlation Distance",
          "Kullback-Leibler Divergence",
          "Determinent",
          "AIC",
          "BIC"
        )
      )
    )
}
