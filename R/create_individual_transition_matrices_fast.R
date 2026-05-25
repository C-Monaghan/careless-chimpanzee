create_individual_transition_matrices_fast <- function(data) {
  # Convert to data.table for speed
  data <- as.data.table(data)
  
  # Remove NA transitions first
  data <- data[!is.na(y) & !is.na(y_prev)]
  
  # Get states info
  states <- sort(unique(c(data$y, data$y_prev)))
  n_states <- length(states)
  
  
  
  # Create transition index as a single integer instead of matrix
  # For a state-space of n_states, each transition becomes: (from-1)*n_states + to
  data[, transition_id := (as.numeric(y_prev) - 1) * n_states + as.numeric(y)]
  
  # Group by ID and wave and create sparse representation
  transitions <- data[, .(
    transition_ids = list(transition_id),
    waves = list(paste0("w_", as.numeric(w)-1, "_", w))
  ), by = ID]
  
  # Convert to list format matching original structure
  result <- list()
  for(i in 1:nrow(transitions)) {
    id <- transitions$ID[i]
    trans_list <- list()
    for(j in seq_along(transitions$transition_ids[[i]])) {
      # Create sparse matrix representation (only store non-zero)
      trans_matrix <- matrix(0, n_states, n_states)
      from_state <- ((transitions$transition_ids[[i]][j] - 1) %/% n_states) + 1
      to_state <- ((transitions$transition_ids[[i]][j] - 1) %% n_states) + 1
      trans_matrix[from_state, to_state] <- 1
      
      trans_list[[transitions$waves[[i]][j]]] <- trans_matrix
    }
    result[[paste0("p_", id)]] <- trans_list
  }
  
  return(result)
}