run_markov_pipeline <- function(
  data,
  scenario,
  n_states,
  sample_size,
  rep,
  seed
) {
  #* Fit the Markov models
  models <- run_markov_model(
    data = data,
    sample_size = sample_size,
    rep = rep,
    scenario = scenario,
    n_states = n_states,
    seed = 123
  )

  #* Extract the following information
  # 1. Predicted probabilities of transition
  # 2. Actual observed transitions
  # 3. Fit metrics
  preds <- models |> calculate_predicted_probs()
  obs <- models |> flatten_obs_transitions()
  fits <- models |> extract_fit_metrics()

  #* Join the predicted and observed transitions
  trans <- left_join(
    preds,
    obs,
    by = c("ID", "wave", "scenario", "n_states", "sample_size", "rep")
  )

  #* Calculate distances
  distances <- trans |> calculate_distances()

  #* Combine everything together
  combine_distances(trans, distances, fits) |>
    clean_distances()
}
