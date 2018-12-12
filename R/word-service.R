# This file contains the functions for the word serviceof the the Leipzig
# Corpora Collection
# Function names correspond to the API names
# The German corpus will be used as a Default





word <- function(word, corpus = "deu_news_2012_1M", ...){

    assertthat::assert_that(all(is.character(word)),
                            msg = "word must be a string or a vector of strings")
    word_list <- glue::glue("http://api.corpora.uni-leipzig.de/ws/words/{corpus}/word/{word}")
    purrr::map_dfr(word_list, catch_error_json, ...)
}

