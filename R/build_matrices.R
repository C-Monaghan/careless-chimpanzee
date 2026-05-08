build_matrices <- function(probs, augmented_data, n_states) {
  
  # Convert probs to data.frame and bind with augmented data
  prob_df <- as.data.frame(probs)
  
  # Ensure column names exist
  if (is.null(colnames(prob_df))) {
    colnames(prob_df) <- paste0("state_", seq_len(ncol(prob_df)))
  }
  
  prob_df <- bind_cols(augmented_data, prob_df)
  
  # Identify probability columns
  prob_cols <- colnames(prob_df)[
    !(colnames(prob_df) %in% colnames(augmented_data))
  ]
  
  # Build matrices by grouping ---------------------------------------------
  matrices_df <- prob_df |>
    group_by(ID, w) |>
    arrange(ID, w) |>
    summarise(
      sim_mat = list(
        matrix(
          as.matrix(across(all_of(prob_cols))),
          nrow = n_states,
          byrow = FALSE
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      w = as.numeric(w),
      wave = paste0(w - 1, "-", w)
    )
  
  matrices_df
}