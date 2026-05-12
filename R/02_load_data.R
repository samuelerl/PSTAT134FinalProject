# Load raw Kaggle / MovieLens CSVs into the working environment.
# Summaries below preserve the original row order for dataset vs nrow() (historical parity).

load_movie_raw_csvs <- function() {
  list(
    keywords = read_csv("data/movies/keywords.csv"),
    links = read_csv("data/movies/links.csv"),
    ratings = read_csv("data/movies/ratings.csv"),
    movies = read_csv("data/movies/movies_metadata.csv"),
    credits = read_csv("data/movies/credits.csv")
  )
}

raw <- load_movie_raw_csvs()
keywords <- raw$keywords
links <- raw$links
ratings <- raw$ratings
movies <- raw$movies
credits <- raw$credits
rm(raw)

dataset_summary <- tibble(
  dataset = c("credits", "keywords", "links", "ratings", "movies_metadata"),
  observations = c(
    nrow(keywords),
    nrow(links),
    nrow(ratings),
    nrow(movies),
    nrow(credits)
  ),
  variables = c(
    ncol(keywords),
    ncol(links),
    ncol(ratings),
    ncol(movies),
    ncol(credits)
  )
) %>% kable()

column_summary <- tibble(
  dataset = c("credits", "keywords", "links", "ratings", "movies_metadata"),
  columns = c(
    paste(colnames(keywords), collapse = ", "),
    paste(colnames(links), collapse = ", "),
    paste(colnames(ratings), collapse = ", "),
    paste(colnames(movies), collapse = ", "),
    paste(colnames(credits), collapse = ", ")
  )
)

column_summary %>% kable()
