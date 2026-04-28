softmax <- function(eta) {
  exp_eta <- exp(c(0, eta))
  exp_eta / sum(exp_eta)
}
