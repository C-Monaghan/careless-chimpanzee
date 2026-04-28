compute_probabilities <- function(
  x1,
  x2,
  x3,
  y_prev,
  betas,
  scenario,
  n_states
) {
  Km1 <- n_states - 1

  eta <- betas$alpha +
    betas$beta_x1 * x1 +
    betas$beta_x2 * x2 +
    betas$beta_x3 * x3

  if (scenario >= 2 && !is.null(y_prev)) {
    prev_vec <- rep(0, Km1)
    if (y_prev > 1) {
      prev_vec[y_prev - 1] <- 1
    }

    eta <- eta + betas$beta_prev %*% prev_vec

    if (scenario == 3) {
      eta <- eta +
        betas$beta_int_x1 %*% (x1 * prev_vec) +
        betas$beta_int_x2 %*% (x2 * prev_vec) +
        betas$beta_int_x3 %*% (x3 * prev_vec)
    }
  }

  softmax(as.vector(eta))
}
