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
    purrr::map_dfr(word_list, query_word, ...)
}
# TODO Add documentation for this function
query_word <- function(url, ...){
    q_res <- catch_error_json(url)
    word_string <- stringr::str_extract(url, "\\w+$")
    if (is.null(q_res$error)) {
        return(q_res$result)
    } else {
        return(
            list(id = NA,
                 word = word_string,
                 freq = NA,
                 wordRank = NA,
                 frequencyClass = NA)
        )
    }
}


#' Query a random selection of words with their frequency
#'
#' lists randomly choosen words (of medium frequency) of a corpus.
#' Can be used to provide the user with a preselection of words.
#'
#' @param n integer: Number of words to query. Should be between 1 and 1000
#' @param corpus Name of the corpus to query. Default is the German deu_news_2012_1M
#' @param force logical. Set to TRUE, if you want to query more than 1000 words
#'
#' @return A tibble with random words, their id and their word frequency
#' @export
#'
#' @examples
#' randomword(5)
#'
randomword <- function(n, corpus = "deu_news_2012_1M",force = FALSE){
    assertthat::assert_that(assertthat::is.count(n))
    assertthat::assert_that(assertthat::is.flag(force))
    if (n > 1000 & force == FALSE) stop("Your request exceeds the imho reasonable size of 1000.
                                        Use force = TRUE to do it anyway at your own risk")

      tibble::as_tibble(
          jsonlite::fromJSON(
              glue::glue("http://api.corpora.uni-leipzig.de/ws/words/{corpus}/randomword/?limit={n}")
              )
          )
}
