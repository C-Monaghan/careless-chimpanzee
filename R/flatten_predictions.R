flatten_predictions <- function(model) {
  imap_dfr(predictions, function(by_sub_block, parent) {
    imap_dfr(by_sub_block, function(mat, sub_block) {
      mat_names <- names(mat)

      tibble(
        ID = str_extract(mat_names, "(?<=ID_)\\d+"),
        wave = str_extract(mat_names, "\\d+-\\d+"),
        parent_block = parent,
        sub_block = sub_block,
        sim_mat = unname(mat)
      )
    })
  })
}
