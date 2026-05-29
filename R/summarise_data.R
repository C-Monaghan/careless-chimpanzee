summarise_data <- function(data, states) {
    data |>
      group_by(
        scenario, 
        n_states, 
        parent_block, 
        sub_block, 
        sample_size, 
        rep, 
        metric
        ) |>
      summarise(value = mean(value), .groups = "drop") |>
      filter(n_states == states) |>
      collect() |>
      group_by(parent_block, sample_size, rep, metric) |>
      mutate(
        winning = rank(value),
        lowest = ifelse(winning == 1, TRUE, FALSE)
      ) |>
      group_by(parent_block, sub_block, sample_size, metric) |>
      summarise(n_lowest = sum(lowest), .groups = "drop") |>
      mutate(prop = n_lowest / 100)
}