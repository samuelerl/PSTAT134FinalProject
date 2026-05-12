# Data cleaning and merging helpers for the movie recommender write-up.
# Pipelines match 134FinalProjectWriteUp.Rmd (behavioral parity).

clean_ratings <- function(ratings) {
  ratings %>%
    select(-timestamp)
}

trim_movies_metadata <- function(movies) {
  movies %>%
    select(
      -homepage,
      -original_title,
      -poster_path,
      -video,
      -status,
      -original_title
    )
}

merge_movies_keywords_credits <- function(movies_clean, keywords, credits) {
  movies_clean %>%
    left_join(keywords, by = "id") %>%
    left_join(credits, by = "id")
}

map_ratings_to_tmdb_summaries <- function(ratings_clean, links) {
  ratings_mapped <- ratings_clean %>%
    left_join(links, by = "movieId")

  ratings_summary <- ratings_mapped %>%
    group_by(tmdbId) %>%
    summarise(
      avg_user_rating = mean(rating, na.rm = TRUE),
      num_user_ratings = n(),
      .groups = "drop"
    )

  list(
    ratings_mapped = ratings_mapped,
    ratings_summary = ratings_summary
  )
}

join_rating_summaries <- function(movies_full, ratings_summary) {
  movies_full %>%
    left_join(
      ratings_summary,
      by = c("id" = "tmdbId")
    )
}

map_original_language_to_names <- function(movies_final) {
  movies_final %>%
    mutate(
      original_language =
        ISO_639_2$Name[
          match(original_language, ISO_639_2$Alpha_2)
        ]
    )
}

rename_tmdb_vote_columns <- function(movies_final) {
  movies_final %>%
    rename(
      runtime_minutes = runtime,
      tmdb_vote_average = vote_average,
      tmdb_vote_count = vote_count,
      tmdb_popularity = popularity
    ) %>%
    mutate(release_year = year(release_date))
}

dedupe_movies_final_with_summary <- function(movies_final) {
  dataset_summary <- tibble(
    observations = nrow(movies_final),
    variables = ncol(movies_final),
    duplicate_rows = sum(duplicated(movies_final)),
    unique_movies = n_distinct(movies_final$id)
  )

  movies_final_distinct <- movies_final %>%
    distinct(id, .keep_all = TRUE)

  list(
    dataset_summary = dataset_summary,
    movies_final = movies_final_distinct
  )
}

compute_missingness_long <- function(movies_final) {
  movies_final %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "column",
      values_to = "missing_pct"
    ) %>%
    arrange(desc(missing_pct))
}

impute_and_drop_for_modeling <- function(movies_final) {
  movies_final %>%
    select(-belongs_to_collection) %>%
    mutate(
      tagline = replace_na(tagline, ""),
      overview = replace_na(overview, "")
    ) %>%
    mutate(
      avg_user_rating = replace_na(avg_user_rating, median(avg_user_rating, na.rm = TRUE)),
      num_user_ratings = replace_na(num_user_ratings, median(num_user_ratings, na.rm = TRUE)),
      runtime_minutes = replace_na(runtime_minutes, median(runtime_minutes, na.rm = TRUE))
    ) %>%
    drop_na(
      release_date,
      imdb_id,
      original_language,
      tmdb_popularity,
      revenue,
      spoken_languages,
      title,
      tmdb_vote_average,
      tmdb_vote_count,
      production_companies,
      production_countries,
      cast,
      crew,
      adult,
      budget,
      id,
      genres
    )
}

filter_movies_released_through_year <- function(movies_clean, year) {
  movies_clean %>%
    filter(release_year <= year)
}
