prepare_markov_dataset <- function(data, sample_size, seed = NULL) {
  # Perform an 80/20 split on the data
  split <- split_ids(unique(data$ID), prop = 0.2)

  # Split the data into train and test
  train_data <- data[data$ID %in% split$train, ]
  test_data <- data[data$ID %in% split$test, ]

  # From the training data sample based off the sample sized
  #   - i.e., from 8,000 training cases sample 1,000 of them
  sampled_ids <- sample(split$train, sample_size)

  # Save that as an object
  train_sample <- train_data[train_data$ID %in% sampled_ids, ]

  list(
    train_data = train_data,
    test_data = test_data,
    train_sample = train_sample
  )
}
