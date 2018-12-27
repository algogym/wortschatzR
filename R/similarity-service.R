# This file contains the functions for the similarity service of the Leipzig
# Corpora Collection
# Function names correspond to the API names
# The German corpus will be used as a Default

#' Lists words that are similar to the input word
#'
#' The results are based on the sentence and neighbour cooccurrences profiles of the words in descending order based on similarity value.
#'
#' @param word A string or vector of strings containing words, for which the information should be queried
#' @param corpus Name of the corpus to query. Default is the German deu_news_2012_1M
#' @param min_sim Minimal similarity of the possible candidate words to the queried word
#' @param max_num integer. Maximal number of retrieved similar words.
#'
#' @return A tibble with words similar to the queried_word
#' @export
#'
#' @examples
#' coocsim("mein")
#'
#' coocsim(c("mein", "ist"), min_sim=0.3, max_num=4)

coocsim <- function(word, corpus = "deu_news_2012_1M", min_sim = 0.25, max_num = 20 ){
    assertthat::assert_that(min_sim >= 0 & min_sim <=1 ,
                            msg = "min_sim must be between 0 and 1")
    assertthat::assert_that((corpus %in% list_available_corpora(TRUE)),
                            msg = "No valid corpus name provived.
                            Use list_available_corpora() to see the complete list of possible corpora.")
    assertthat::assert_that(all(is.character(word)),
                            msg = "word must be a string or a vector of strings")
    assertthat::assert_that(assertthat::is.count(max_num))

    word_list <- glue::glue("http://api.corpora.uni-leipzig.de/ws/similarity/{corpus}/coocsim/{word}?minSim={min_sim}&limit={max_num}")
    purrr::map_dfr(word_list,query_similarity)
}

#' Wrapper around catch_error_json to create the right output for querys
#' to the similarity services with errors
#'
#' This function is used when querying to the similarity service and takes the result of the error catching json function and returns either results
#' if the request was successful or a list with the same dimensions as a successful request but with NAs
#'
#' @param url URL for the API request
#' @return list with the query results or a list with NAs

query_similarity <- function(url){
    q_res <- catch_error_json(url)
    word_string <- stringr::str_extract(url, "\\w+(?=\\?minSim)")
    if (is.null(q_res$error) & length(q_res$result) > 0) {
        q_res$result %>%
            jsonlite::flatten() %>%
            dplyr::rename_all(stringr::str_remove, pattern = "word.") %>%
            dplyr::mutate(queried_word= word_string) %>%
            dplyr::select( .data[["id"]], .data[["word"]], similarity = .data[["sim"]], .data[["freq"]]) %>%
            return()
    } else {
        return(
            list(queried_word = word_string,
                 id = NA,
                 word = NA,
                 freq = NA,
                 similarity = NA,
                 freq = NA)
        )
    }
}
