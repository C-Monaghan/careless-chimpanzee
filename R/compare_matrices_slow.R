# Converting a C++ function into R because C++ doesn't work with parallel
# processing → for some reason ... 
compare_matrices_slow <- function(Obs, Sim) {
  
  # Make sure the observations are in matrix form
  # They should be but I run into so many errors I wanna make 100% sure
  Obs <- as.matrix(Obs)
  Sim <- as.matrix(Sim)
  
  # Calculate the difference matrix
  D <- Obs - Sim
  
  # Flatten into a vector
  d <- as.vector(D)
  
  n <- length(d)
  
  # Doing different calculations -----------------------------------------------
  # Manhattan norm (L1)
  sum_abs <- sum(abs(d))
  
  # Squared error
  sum_sq <- sum(d^2)
  
  # Max absolute error
  max_abs <- max(abs(d))
  
  # Vectorised originals
  x <- as.vector(Obs)
  y <- as.vector(Sim)
  
  # Sums for correlation
  sum_x  <- sum(x)
  sum_y  <- sum(y)
  sum_xy <- sum(x * y)
  sum_x2 <- sum(x^2)
  sum_y2 <- sum(y^2)
  
  # Frobenius norm
  frob <- sqrt(sum_sq)
  
  # RMSE
  rmse <- sqrt(sum_sq / n)
  
  # Pearson correlation
  denom_corr <- sqrt((n * sum_x2 - sum_x^2) *
                       (n * sum_y2 - sum_y^2))
  
  corr <- NA_real_
  
  if (!is.na(denom_corr) && denom_corr != 0 && is.finite(denom_corr)) {
    corr <- (n * sum_xy - sum_x * sum_y) / denom_corr
  }
  
  # KL divergence
  eps <- 1e-10
  kl <- sum((x + eps) * log((x + eps) / (y + eps)))
  
  data.frame(
    metric = c("Frobenius", "Manhattan", "Max", "MeanAbs",
               "RMSE", "Correlation", "KL"),
    value = c(
      frob,
      sum_abs,
      max_abs,
      sum_abs / n,
      rmse,
      ifelse(is.na(corr), NA_real_, 1 - corr),
      kl
    ),
    stringsAsFactors = FALSE
  )
}