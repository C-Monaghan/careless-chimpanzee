flatten_obs_transitions <- function(model) {
  # The individual transitions are all stored in the models as is the meta data
  indv_trans <- model |> pluck("indv_trans")
  meta <- model |> pluck("meta")

  indv_trans |>
    imap_dfr(function(by_wave, id) {
      imap_dfr(by_wave, function(transition, wave) {
        tibble(
          ID = str_remove(id, "^p_") |> as.numeric(),
          wave = str_replace(wave, "^w_", "") |> str_replace_all("_", "-"),
          scenario = meta$scenario,
          n_states = meta$n_states,
          sample_size = meta$sample_size,
          rep = meta$rep,
          obs_mat = list(transition)
        )
      })
    })
}
