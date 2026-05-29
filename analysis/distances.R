library(dplyr)
library(purrr)
library(ggplot2)

# Theme ------------------------------------------------------------------------
theme_simulation <- function() {
  theme_minimal() +
    theme(
      axis.text.y = element_text(face = "bold", size = rel(0.7)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.7)),
      axis.title = element_text(size = rel(0.8)),
      strip.background = element_rect(fill = "#F0F0F0", colour = NA),
      strip.text = element_text(face = "bold", size = rel(0.45)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent"),
      legend.text = element_text(size = rel(0.5)),
      legend.title = element_text(size = rel(0.6)),
      legend.key.size = unit(0.5, 'cm')
    )
}

# Load database of results
final_data <- arrow::open_dataset("arrow_distances")

# Map over the number of states ------------------------------------------------
best_models <- map(c(3, 4, 5), function(states) {
  final_data |> summarise_data(states = states)
}) |>
  set_names(nm = c("3_states", "4_states", "5_states"))

# Plotting the results ---------------------------------------------------------
best_plots <- best_models |>
  map(function(model) {
    model |>
      ggplot(aes(x = parent_block, y = prop, fill = sub_block)) +
      geom_col(colour = "black", linewidth = 0.25) + 
      geom_text(
        aes(label = ifelse(prop >= 0.04, scales::percent(prop, accuracy = 1), NA)),
        position = position_stack(vjust = 0.5),
        size = rel(1.5)
      ) +
      scale_fill_manual(
        values = c(
          "Null Model" = "#E69F00", 
          "Reduced Model 1" = "#56B4E9", 
          "Reduced Model 2" = "#009e73", 
          "True Model" = "#F0E442",
          "Overfit Model" = "#0072B2")
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Model Type",
        y = "Proportion of Repetitions as best", 
        fill = "Sub Model") +
      facet_grid(
        sample_size ~ metric, space = "free",
        labeller = labeller(metric = function(x) stringr::str_wrap(x, 15))
      ) +
      theme_simulation() +
      ggview::canvas(width = 6.75, height = 7)
  })

# Exporting --------------------------------------------------------------------
map2(
  best_plots, 
  c("3_states.png", "4_states.png", "5_states.png"), 
  function(fig, name) {
   ggview::save_ggplot(fig, here::here("analysis/results/", name)) 
  })


