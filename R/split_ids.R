split_ids <- function(ids, prop = 0.2) {
  test <- sample(ids, ceiling(prop * length(ids)))

  list(train = setdiff(ids, test), test = test)
}
