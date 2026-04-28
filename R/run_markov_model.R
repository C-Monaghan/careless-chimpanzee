run_markov_model <- function(data, sample_size, rep, seed = 1) {
  # Setting unique seed for each target
  set.seed(seed + rep)

  # Split the data into train / test and create sample datasets
  sample_data <- prepare_markov_dataset(
    data = data,
    sample_size = sample_size,
    seed = seed + rep
  )

  # Extract training sample data
  train_sample <- sample_data |> pluck("train_sample")

  # Extract test data (we need it for later)
  test_data <- sample_data |> pluck("test_data")

  # Fit all the markov models
  models <- fit_markov(train_sample)

  # Calculate the idividual transition matrices on the test data
  indv_trans <- create_individual_transition_matrices(test_data)

  list(
    models = models,
    test_data = test_data,
    indv_trans = indv_trans
  )
}
