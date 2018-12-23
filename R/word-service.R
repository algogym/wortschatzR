# This file contains the functions for the word service of the the Leipzig
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

#' Wrapper around catch_error_json to create the right output for querys with errors
#'
#' This function takes the result of the error catching json function as returns either results
#' if the request was successful or a list with the same dimensions as a successful request but with NAs
#'
#' @param url URL for the API request
#' @param ... Additional arguments passed to catch_error_json. For now quietly = FALSE enables the display of warnings,
#'  whenever a word has not entry in the corpus
#'
#' @return A list with the query results or a list with NAs

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


#' Query words starting with a specific letter combination
#'
#' lists words with a specific prefix with a minimum length of 4 characters.
#' Can be used for autocompletion functionality or similar.
#'
#' @param prefix A string or vector of strings containing the prefixes, for which words should be queried.
#' The prefixes must be at least 4 letters long.
#' @param max_num integer. Maximal number of retrieved words. Should be between 1 and 1000.
#' @param min_freq interger. Minimum frequency of retrieved words. Default is 2.
#' @param corpus string. Name of the corpus to query. Default is the German deu_news_2012_1M
#' @param force logical. Set to TRUE, if you want to query more than 1000 words
#'
#' @return A tibble with words starting with the prefix and their Frequency.
#' @export
#'
#' @examples
#' prefixword("fort")
prefixword <- function(prefix, max_num=10, min_freq=2, corpus = "deu_news_2012_1M", force = FALSE){

  assertthat::assert_that(assertthat::is.count(min_freq))
  assertthat::assert_that(assertthat::is.count(max_num))
  assertthat::assert_that(assertthat::is.flag(force))
  if (max_num > 1000 & force == FALSE) stop("Your request exceeds the imho reasonable size of 1000.
                                        Use force = TRUE to do it anyway at your own risk")
  if (any(stringr::str_length(prefix) < 4)) warning("All prefixes must be at least 4 characters long. Expect some NAs")

  word_list <- glue::glue("http://api.corpora.uni-leipzig.de/ws/words/{corpus}/prefixword/{prefix}?minFreq={min_freq}&limit={max_num}")

  purrr::map_dfr(word_list, query_prefix)
}

#' Wrapper around catch_error_json to create the right output for querys with errors
#'
#' This function is used when querying prefixes and takes the result of the error catching json function as returns either results
#' if the request was successful or a list with the same dimensions as a successful request but with NAs
#' @param url URL for the API request
#'
#' @return list with the query results or a list with NAs


query_prefix <- function(url){
  q_res <- catch_error_json(url)
  word_string <- stringr::str_extract(url, "\\w+(?=\\?minFreq)")
  if (is.null(q_res$error) & length(q_res$result > 0)) {
    return(dplyr::bind_cols(query = rep(word_string, nrow(q_res$result)), q_res$result))
  } else {
    return(
      list(query = word_string,
           id = NA_integer_,
           word = NA_character_,
           freq = NA_integer_)
    )
  }
}
