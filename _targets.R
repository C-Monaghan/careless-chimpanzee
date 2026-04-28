library(targets)
library(tarchetypes)

# Functions to be used ---------------------------------------------------------
purrr::walk(list.files("R/", recursive = TRUE, full.names = TRUE), source)

# Packages to be throughout pipeline -------------------------------------------
tar_option_set(
  packages = c(
    "dplyr",
    "data.table",
    "tibble",
    "purrr",
    "rlang",
    "future"
  )
)

list(
  #? Setting up ----------------------------------------------------------------
  # These are all the scenarios and number of states I want to do
  tar_target(
    params,
    tidyr::crossing(
      scenario = 1:3,
      n_states = 3:5
    )
  ),

  # How many sample sizes to do in the modelling
  tar_target(sample_size, c(100, 250, 1000, 5000)),

  # How many repititions to do in the modelling
  tar_target(rep, seq_len(50)),

  #* Apply simulation function over each row of params -------------------------
  tar_target(
    sim_data,
    simulate_data(
      n_subjects = 10000,
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

  #* Extract IDs from simulated data -------------------------------------------
  tar_target(
    sim_ids,
    unique(sim_data_prev$ID),
    pattern = map(sim_data_prev),
    iteration = "list"
  ),

  #* Split datasets into training and test (80 / 20 split) ---------------------
  tar_target(
    data_split,
    sim_ids |> unique() |> split_ids(prop = 0.2),
    pattern = map(sim_ids),
    iteration = "list"
  ),

  #* Replicate each of the datasets --------------------------------------------
  tar_target(
    sampled_data,
    {
      # Get the training IDS
      train_ids <- data_split$train

      # Sample a sequence of them based off the overall sample size
      sampled_ids <- sample(train_ids, size = sample_size, replace = FALSE)

      # Get the associated data
      sim_data_prev[sim_data_prev$ID %in% sampled_ids, ]
    },
    pattern = cross(sim_data_prev, data_split, sample_size, rep),
    iteration = "list"
  )

  #* Step 3: Fit the markov models over all datasets ---------------------------
  # tar_target(
  #   models,
  #   fit_markov_model(
  #     data = sim_data_prev,
  #     sample_sizes = c(100, 250, 1000, 5000),
  #     n_reps = 5,
  #     parallel = FALSE,
  #     seed = 7140 + params$scenario * 10 + params$n_states
  #   ),
  #   pattern = map(sim_data_prev),
  #   iteration = "list"
  # )
)
