extract_fit_metrics <- function(model) {
  # Extract the model data
  fits <- model |> pluck("models")

  # Extract the meta data
  meta <- model |> pluck("meta")

  imap_dfr(fits, function(by_sub_model, parent) {
    imap_dfr(by_sub_model, function(betas, sub_model) {
      tibble(
        parent_block = parent,
        sub_block = sub_model,
        scenario = meta$scenario,
        n_states = meta$n_states,
        sample_size = meta$sample_size,
        rep = meta$rep,
        aic = AIC(betas),
        bic = BIC(betas)
      )
    }) # End of inner imap
  }) # End of outer imap
}
