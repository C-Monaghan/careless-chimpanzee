calculate_predicted_probs <- function(model) {
  # The test data is already stored within each target branch so all we need to
  # do is extract it
  test_data <- model |> pluck("test_data")

  # Now I need to figure out how many states is within this dataset
  n_states <- test_data |> pull(y) |> unique() |> length()

  # Now we can create the augmented dataset with the correct amount of states
  augmented_data <- create_augmented_data(data = test_data, n_states = n_states)

  # Little helper function to predict from a model -----------------------------
  make_predictions <- function(mod) {
    predict(mod, augmented_data, type = "probs")
  }

  # Apply these predictions over all the models within one target --------------
  model |>
    pluck("models") |>
    imap(function(model_group, parent_block) {
      imap(model_group, function(betas, sub_block) {
        # Calculate probabilities ----------------------------------------------
        probs <- make_predictions(mod = betas)

        # TODO: need to make this fully generalisable --------------------------
        # Now we need to reshape these into nice little matrices
        ids <- unique(augmented_data$ID)

        # This is the number of possible states we could have
        # → should range from 3 - 5
        n_states <- ncol(probs)

        # This is the number of possible waves in the data
        # → ideally this should always be 2 but maybe I'll end up doing more
        # longitudinal data
        n_waves <- augmented_data$w |> unique() |> length()

        # Split these probabilities into blocks
        split_rows <- split(
          seq_len(nrow(probs)),
          ceiling(seq_along(seq_len(nrow(probs))) / n_states)
        )

        # Now we need to better name these blocks
        id_wave_names <- rep(ids, each = n_waves)

        wave_labels <- rep(
          paste0(seq_len(n_waves), "-", seq_len(n_waves) + 1),
          times = length(ids)
        )

        matrix_names <- paste0("ID_", id_wave_names, "_", wave_labels)

        # Now we can build the matrics
        matrices <- setNames(
          lapply(split_rows, function(rows) {
            matrix(
              probs[rows, ],
              nrow = n_states,
              ncol = n_states,
              byrow = FALSE
            )
          }),
          matrix_names
        )

        return(matrices)
      })
    })
}
