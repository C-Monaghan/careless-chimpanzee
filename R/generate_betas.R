generate_betas <- function(n_states, scenario) {
  K <- n_states
  Km1 <- K - 1

  make_mat <- function(scale = 0.1) {
    matrix(rnorm(Km1, 0, scale), nrow = Km1)
  }

  betas <- list(
    alpha = make_mat(1),
    beta_x1 = make_mat(),
    beta_x2 = make_mat(),
    beta_x3 = make_mat()
  )

  if (scenario >= 2) {
    betas$beta_prev <- matrix(rnorm(Km1 * Km1, 0, 0.5), Km1, Km1)
  }

  if (scenario == 3) {
    betas$beta_int_x1 <- matrix(rnorm(Km1 * Km1, 0, 0.2), Km1, Km1)
    betas$beta_int_x2 <- matrix(rnorm(Km1 * Km1, 0, 0.2), Km1, Km1)
    betas$beta_int_x3 <- matrix(rnorm(Km1 * Km1, 0, 0.2), Km1, Km1)
  }

  betas
}
