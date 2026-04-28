library(targets)
library(tarchetypes)

# Functions to be used ---------------------------------------------------------
purrr::walk(list.files("R/", recursive = TRUE, full.names = TRUE), source)

# Packages to be throughout pipeline -------------------------------------------
tar_option_set(
  packages = c(
    # The usual gang
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "stringr",

    # Modelling
    "nnet",

    # Some extras
    "data.table",
    "rlang",
    "future"
  )
)

list(
  #? Setting up ----------------------------------------------------------------
  # These are all the scenarios and number of states I want to do for the data
  # simulation
  tar_target(
    params,
    tidyr::crossing(scenario = 1:3, n_states = 3:5)
  ),

  # This is the design for the modelling
  tar_target(
    model_design,
    tidyr::crossing(sample_size = c(100, 250), rep = seq_len(5))
  ),

  #* Apply simulation function over each row of params -------------------------
  tar_target(
    sim_data,
    simulate_data(
      n_subjects = 1000,
      n_waves = 3,
      scenario = params$scenario,
      n_states = params$n_states,
      seed = 7140 + params$scenario * 10 + params$n_states
    ),
    pattern = map(params),
    iteration = "list"
  ),

  #* Create a previous y column in the data ------------------------------------
  tar_target(
    sim_data_prev,
    sim_data |> pluck("data") |> add_previous_status(),
    pattern = map(sim_data),
    iteration = "list"
  ),

  tar_target(
    analysis_data,
    {
      list(data = sim_data_prev, ids = unique(sim_data_prev$ID))
    },
    pattern = map(sim_data_prev)
  ),

  #* Fit Markov models ---------------------------------------------------------
  tar_target(
    models,
    run_markov_model(
      data = sim_data_prev,
      sample_size = model_design$sample_size,
      rep = model_design$rep,
      seed = 123
    ),
    pattern = cross(sim_data_prev, model_design),
    iteration = "list"
  ),

  #* Calculate predicted probabilities -----------------------------------------
  tar_target(
    predicted_probs,
    calculate_predicted_probs(models),
    pattern = map(models),
    iteration = "list"
  ),

  #* Now I wanted to flatten each of these predictions -------------------------
  tar_target(
    flatten_preds,
    flatten_predictions(predicted_probs),
    pattern = map(predicted_probs),
    iteration = "list"
  )
)
