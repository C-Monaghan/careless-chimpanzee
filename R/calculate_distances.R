calculate_distances <- function(transitions) {
  # How many transitions are we dealing with
  max_transitions <- nrow(transitions)

  results_list <- vector("list", length = max_transitions)

  # Now I want to apply to the C++ function to each individual matrix
  for (i in seq_len(max_transitions)) {
    obs <- transitions$obs_mat[[i]]
    sim <- transitions$sim_mat[[i]]

    # Apply C++ function
    results_list[[i]] <- tryCatch(
      compare_matrices_rcpp(Obs = obs, Sim = sim),
      error = function(e) {
        message(sprintf("Error in row %d: %s", i, e$message))
        NULL
      }
    )
  }

  results_list |> rbindlist(fill = TRUE, idcol = "row_id")
}
