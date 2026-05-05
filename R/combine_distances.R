combine_distances <- function(transitions, distances, metrics) {
  transitions |>
    select(-c(sim_mat, obs_mat)) |>
    left_join(
      distances,
      by = c(
        "ID",
        "wave",
        "parent_block",
        "sub_block",
        "scenario",
        "n_states",
        "sample_size",
        "rep"
      )
    ) |>
    left_join(
      metrics,
      by = c(
        "parent_block",
        "sub_block",
        "scenario",
        "n_states",
        "sample_size",
        "rep"
      )
    )
}
