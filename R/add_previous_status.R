add_previous_status <- function(data, id = "ID", w = "w", y = "y") {
  # Make sure data is correctly arranged
  data <- data |>
    dplyr::arrange(!!rlang::sym(id), !!rlang::sym(w))

  # Create a t-1 column
  data <- data |>
    dplyr::group_by(!!rlang::sym(id)) |>
    dplyr::mutate(y_prev = dplyr::lag(!!rlang::sym(y), n = 1)) |>
    dplyr::ungroup() |>
    dplyr::relocate(y_prev, .after = y)

  # Convert to factor
  # If y is a factor, then logically prev_y should also be a factor
  if (is.factor(data$y)) {
    data$y_prev <- factor(data$y_prev, levels = levels(data$y))
  }

  # Removing wave 1 (w = 1) from data as there is no prev_y column
  data <- data |>
    dplyr::filter(!is.na(y_prev))

  return(data)
}
