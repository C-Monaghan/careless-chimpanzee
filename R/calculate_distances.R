calculate_distances <- function(transitions) {
  # How many transitions are we dealing with
  max_transitions <- nrow(transitions)
  
  results_list <- vector("list", length = max_transitions)
  
  # Now I want to apply to the C++ function to each individual matrix
  for (i in seq_len(max_transitions)) {
    obs <- transitions$obs_mat[[i]]
    sim <- transitions$sim_mat[[i]]
    
    # Apply C++ function
    res <- tryCatch(
      compare_matrices_rcpp(Obs = obs, Sim = sim),
      error = function(e) {
        message(sprintf("Error in row %d: %s", i, e$message))
        
        # Return an empty set of values
        data.table(metric = NA_character_, value = NA_real_)
      }
    )
    
    # Convert to a datatable
    res <- res |> as.data.table()
    
    # Make sure to add back in the meta data
    if (!is.null(res)) {
      res[, `:=`(
        ID = transitions$ID[i],
        wave = transitions$wave[i],
        parent_block = transitions$parent_block[i],
        sub_block = transitions$sub_block[i],
        scenario = transitions$scenario[i],
        n_states = transitions$n_states[i],
        sample_size = transitions$sample_size[i],
        rep = transitions$rep[i]
      )]
    }
    
    results_list[[i]] <- res
  }
  
  out <- results_list |> rbindlist(fill = TRUE)
  
  # When getting this to run with parallel processing I am running into issues
  # some workers are failing
  # Ensuring all columns are present in the data to prevent some random 
  # joining error
  required_cols <- c("metric", "value")
  
  for (col in required_cols) {
    if (!col %in% names(out)) {
      out[, (col) := NA]
    }
  }
  
  out
}
