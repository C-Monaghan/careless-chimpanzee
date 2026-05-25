calculate_predicted_probs <- function(model) {
  # The test data is already stored within each target branch so all we need to
  # do is extract it
  test_data <- model |> pluck("test_data")

  # Additionally I need the meta data from each model too
  meta <- model |> pluck("meta")

  # Now I need to figure out how many states is within this dataset
  states <- meta |> pluck("n_states") |> seq_len() |> as.character()

  # Now we can create the augmented dataset with the correct amount of states
  augmented_data <- create_augmented_data(
    data = test_data, 
    n_states = meta$n_states
    )

  # Little helper function to predict from a model -----------------------------
  make_predictions <- function(mod) {
    predict(mod, augmented_data, type = "probs")
  }

  # Apply these predictions over all the models within one target --------------
  model |>
    pluck("models") |>
    imap_dfr(function(model_group, parent_block) {
      imap_dfr(model_group, function(betas, sub_block) {
        # Calculate probabilities ----------------------------------------------
        probs <- make_predictions(mod = betas)
        
        # At times not all transitions are in the test datasets
        # This is mostly the case in the smaller datasets (100 / 250)
        # So we will fill in the blank spaces with zeros to ensure the 
        # later matrix calculations can work
        
        # First, what transitions are in the data
        existing <- colnames(probs)
        
        # Create a full matrix with zeros
        full_probs <- matrix(
          0,
          nrow = nrow(probs),
          ncol = length(states),
          dimnames = list(NULL, states)
        )
        
        # Now we will fill in what exists and leave the remaining zeros
        common <- intersect(states, colnames(probs))
        
        full_probs[, common] <- probs[, common, drop = FALSE]
        
        # Phew - now we have what we need!
        probs <- full_probs

        # Build the individual matrices ----------------------------------------
        matrices_df <- build_matrices(probs, augmented_data, meta$n_states)

        # Now I can turn this into a nice little tibble
        tibble(
          ID = matrices_df$ID,
          wave = matrices_df$wave,
          parent_block = parent_block,
          sub_block = sub_block,
          scenario = meta$scenario,
          n_states = meta$n_states,
          sample_size = meta$sample_size,
          rep = meta$rep,
          sim_mat = matrices_df$sim_mat
        )
      }) # End of inner imap
    }) # End of outer imap
}
