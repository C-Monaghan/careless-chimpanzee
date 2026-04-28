fit_markov <- function(train_data) {
  base_formulas <- list(
    null = y ~ 1,
    red1 = y ~ x1,
    red2 = y ~ x1 + x2,
    true = y ~ x1 + x2 + x3,
    of = y ~ x1 + x2 + x3 + x4 + x5
  )

  additive_formulas <- list(
    null = y ~ y_prev,
    red1 = y ~ x1 + y_prev,
    red2 = y ~ x1 + x2 + y_prev,
    true = y ~ x1 + x2 + x3 + y_prev,
    of = y ~ x1 + x2 + x3 + x4 + x5 + y_prev
  )

  multiplicative_formulas <- list(
    null = y ~ y_prev,
    red1 = y ~ x1 * y_prev,
    red2 = y ~ (x1 + x2) * y_prev,
    true = y ~ (x1 + x2 + x3) * y_prev,
    of = y ~ (x1 + x2 + x3 + x4 + x5) * y_prev
  )

  fit_one <- function(formula) {
    nnet::multinom(formula, data = train_data, trace = FALSE)
  }

  list(
    base = purrr::map(base_formulas, fit_one),
    additive = purrr::map(additive_formulas, fit_one),
    multiplicative = purrr::map(multiplicative_formulas, fit_one)
  )
}
