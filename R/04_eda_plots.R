# ggplot helpers for EDA sections (matches 134FinalProjectWriteUp.Rmd styling).

plot_movies_per_year <- function(movies_clean) {
  movies_clean %>%
    count(release_year) %>%
    ggplot(aes(x = release_year, y = n)) +
    geom_line(
      color = "#4C78A8",
      linewidth = 1.2
    ) +
    geom_point(
      color = "#4C78A8",
      size = 1.5
    ) +
    labs(
      title = "Number of Movies Released Per Year",
      x = "Release Year",
      y = "Number of Movies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      axis.title = element_text(face = "bold")
    )
}
