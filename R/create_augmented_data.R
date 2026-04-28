create_augmented_data <- function(data, n_states) {
  augmented_data <- map(1:n_states, function(n) {
    data |> mutate(y_prev = factor(n))
  }) |>
    bind_rows() |>
    arrange(ID, w)

  return(augmented_data)
}
