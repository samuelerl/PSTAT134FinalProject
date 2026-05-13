# NLP process for modularization

make_movies_text <- function(movies_clean) {
  movies_clean %>%
    mutate(
      across(
        c(title, genres, keywords, tagline, overview),
        ~ iconv(as.character(.x), from = "", to = "UTF-8", sub = " ")
      ),
      movie_text = paste(title, genres, keywords, tagline, overview, sep = " ")
    ) %>%
    mutate(
      movie_text = iconv(movie_text, from = "", to = "UTF-8", sub = " "),
      movie_text = str_replace_all(movie_text, "[^[:alnum:] ]", " "),
      movie_text = str_squish(movie_text)
    )
}

make_movie_tokens <- function(movies_text) {
  movies_text %>%
    select(id, title, movie_text) %>%
    unnest_tokens(word, movie_text) %>%
    anti_join(stop_words, by = "word")
}

clean_movie_tokens <- function(tokens) {
  tokens %>%
    filter(word!= "id") %>%
    filter(!is.na(word), word != "")
}

make_word_tf_idf <- function(tokens) {
  tokens %>%
    count(id, word, sort = T) %>%
    bind_tf_idf(term = word,
                document = id,
                n = n) %>%
    arrange(desc(tf_idf))
}

make_bigram_tokens <- function(movies_text) {
  movies_text %>%
    select(id, title, movie_text) %>%
    unnest_tokens(bigram, movie_text, token = "ngrams", n = 2) %>%
    filter(!str_detect(bigram, "\\bid\\b"))
}

clean_bigram_tokens <- function(tokens) {
  tokens %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ")
}

make_bigram_tf_idf <- function(tokens) {
  tokens %>%
    count(id, title, bigram, sort = T) %>%
    bind_tf_idf(term = bigram,
                document = id,
                n = n) %>%
    arrange(desc(tf_idf))
}

clean_bigram_tf_idf <- function(tf_idf) {
  tf_idf %>%
    group_by(bigram) %>%
    summarize(
      avg_tfidf = mean(tf_idf),
      movie_count = n_distinct(id),
    ) %>%
    filter(movie_count >= 3) 
}

create_feature_matrix <- function(tf_idf) {
  tf_idf %>%
    filter(!is.na(id), !is.na(bigram), !is.na(tf_idf)) %>%
    filter(bigram != "") %>%
    group_by(id, bigram) %>%
    summarize(tf_idf = sum(tf_idf), .groups = "drop") %>%
    cast_sparse(
      row = id,
      column = bigram,
      value = tf_idf
    )
}