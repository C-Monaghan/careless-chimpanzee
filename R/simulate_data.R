simulate_data <- function(
  n_subjects = 100,
  n_waves = 3,
  scenario = 1,
  n_states = 3,
  seed = NULL
) {
  set.seed(seed)
  
  # Generate beta values used in probability calculations
  # TODO: I'm not sure this works properly right now (investigate)
  betas <- generate_betas(n_states, scenario)

  #* General subject-level data ------------------------------------------------
  subject_data <- data.frame(
    ID = 1:n_subjects,
    x1 = sample(x = 0:1, size = n_subjects, replace = TRUE),
    x2 = rnorm(n = n_subjects, mean = 5, sd = 2),
    x3 = rnorm(n = n_subjects, mean = 0, sd = 1),
    x4 = runif(n = n_subjects, min = 0, max = 1),
    x5 = runif(n = n_subjects, min = 0, max = 1)
  )

  # TODO: Do we need to store this?
  panel <- vector("list", n_subjects * n_waves)
  probs_store <- vector("list", n_subjects * n_waves)

  idx <- 1

  #* Longitudinal simulation ---------------------------------------------------
  for (i in 1:n_subjects) {
    # Get the data on the first subject
    subj <- subject_data[i, ]

    # What are their current values
    x1_t <- subj$x1
    x2_t <- subj$x2
    x3_t <- subj$x3
    x4_t <- subj$x4
    x5_t <- subj$x5

    y_prev <- NULL

    for (t in 1:n_waves) {
      # Evolve covariates AFTER first wave
      if (t > 1) {
        #? x2 follows a linear trend with some slight noise
        #? x3 follows a random walk process
        #? x4 AND x5 follow a bounded drift
        x2_t <- x2_t + rnorm(1, mean = 1.5, sd = 0.5)
        x3_t <- 0.7 * x3_t + rnorm(1, mean = 0, sd = 1)
        x4_t <- pmin(1, pmax(0, x4_t + runif(1, -0.1, 0.1)))
        x5_t <- pmin(1, pmax(0, x5_t + runif(1, -0.1, 0.1)))
      }

      #* Now we can compute the overall probabilities --------------------------
      probs <- compute_probabilities(
        x1 = x1_t,
        x2 = x2_t,
        x3 = x3_t,
        y_prev = y_prev,
        betas = betas,
        scenario = scenario,
        n_states = n_states
      )

      # Based off these probs what will y be
      y <- sample(1:n_states, size = 1, prob = probs)

      # Store this data
      panel[[idx]] <- data.frame(
        ID = i,
        w = t,
        y = y,
        x1 = x1_t,
        x2 = x2_t,
        x3 = x3_t,
        x4 = x4_t,
        x5 = x5_t
      )

      probs_store[[idx]] <- probs

      # update lag
      y_prev <- y
      idx <- idx + 1
    }
  }

  data <- dplyr::bind_rows(panel)

  # Factorise
  data$y <- factor(data$y, levels = 1:n_states)
  data$w <- factor(data$w)
  data$x1 <- factor(data$x1)

  # Probabilities
  pi_mat <- do.call(rbind, probs_store)
  colnames(pi_mat) <- paste0("pi_", 1:n_states)

  pi_df <- cbind(data[, c("ID", "w")], pi_mat)

  list(
    data = data,
    betas = betas,
    pi_values = pi_df,
    scenario = scenario,
    n_states = n_states
  )
}
