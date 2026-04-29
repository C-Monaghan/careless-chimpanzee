combine_distances <- function(transitions, distances, metrics) {
  transitions |>
    select(-c(sim_mat, obs_mat)) |>
    mutate(row_id = row_number()) |>
    left_join(distances, by = "row_id") |>
    left_join(
      metrics,
      by = c("parent_block", "sub_block", "scenario", "n_states", "sample_size", "rep")
    ) |>
      select(-row_id)
}
