# Packages ---------------------------------------------------------------------
library(targets)
library(tarchetypes)

# Functions to be used ---------------------------------------------------------
targets::tar_source(files = "R")

# We'll be using a C ++ function to handle the matrix calculations
Rcpp::sourceCpp(here::here("C/compare_matrices.cpp"))

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
    "rlang"
  ),
  memory = "transient",
  garbage_collection = TRUE,
  format = "qs",
  )

#* Beginning of targets pipeline
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
    tidyr::crossing(
      sample_size = c(100, 250, 500, 1000),
      rep = seq_len(100)
    )
  ),

  #* Apply simulation function over each row of params -------------------------
  tar_target(
    sim_data,
    simulate_data(
      n_subjects = 5000,
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
    {
      data <- sim_data |> pluck("data") |> add_previous_status()

      list(
        data = data,
        meta = list(
          scenario = sim_data$scenario,
          n_states = sim_data$n_states
        )
      )
    },
    pattern = map(sim_data),
    iteration = "list"
  ),

  #* Run the Markov pipeline ---------------------------------------------------
  tar_target(
    distances,
    {
      # Run the pipeline
      results <- run_markov_pipeline(
        data = sim_data_prev$data,
        scenario = sim_data_prev$meta$scenario,
        n_states = sim_data_prev$meta$n_states,
        sample_size = model_design$sample_size,
        rep = model_design$rep,
        seed = 123
      )
      
      # I need to remove the n in the sample size column to get correct 
      # partitioning of the results
      results <- results |>
        mutate(sample_size_label = stringr::str_remove(sample_size, "n = "))
      
      # Write the data to a set of files
      arrow::write_dataset(
        results,
        path = "arrow_distances",
        format = "parquet",
        partitioning = c("scenario", "n_states", "sample_size_label", "rep"),
        existing_data_behavior = "overwrite"
      )
      
      return("arrow_distances")
    },
    pattern = cross(sim_data_prev, model_design),
    iteration = "list",
    format = "file",
  )
  
  # Everything above takes 2 days 9hrs and 22mins to run
  # I cannot run the bottom code without using all the server memory
  # Need to think about that one ... 

  #* Finally I want to combine these together in a nice scenario × state format
  # tar_target(
  #   final_data,
  #   distances |> bind_rows() |> group_by(scenario, n_states) |> nest()
  # )
)
