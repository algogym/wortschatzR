# This file contains the functions for the word serviceof the the Leipzig
# Corpora Collection
# Function names correspond to the API names
# The German corpus will be used as a Default

#' Queries a lists of information about a word in a corpus.
#'
#' @param word A string or vector of strings containing words, for which the information should be queried
#' @param corpus Name of the corpus to query. Default is the German deu_news_2012_1M
#' @param ... Additional arguments passed to catch_error_json. For now quietly = FALSE enables the display of warnings,
#'  whenever a word has not entry in the corpus
#'
#' @return A tibble with the information about the queried words. That is Frequency, FrequencyClass and WordRank
#' @export
#'
#' @examples
#'
#' word(c("Hund", "Katze", "Maus"))
#'
word <- function(word, corpus = "deu_news_2012_1M", ...){

    assertthat::assert_that(all(is.character(word)),
                            msg = "word must be a string or a vector of strings")
    word_list <- glue::glue("http://api.corpora.uni-leipzig.de/ws/words/{corpus}/word/{word}")
    purrr::map_dfr(word_list, catch_error_json, ...)
}

