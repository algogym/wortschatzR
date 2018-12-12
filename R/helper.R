# This file contains helper functions used by other functions of the package

#' Helper function which catches (404) errors, when requesting a JSON from a remote source
#'
#' @param txt String: URL to the JSON file
#' @param quietly If FALSE, a warning message is returned, whenever an error is encountered. Default: TRUE
#'
#' @return A list with the query results or a list with NAs for each variable that was requested

catch_error_json <- function(txt, quietly = TRUE){
    safe_json <- purrr::safely(jsonlite::fromJSON)
    query_result <- safe_json(txt)

    if (is.null(query_result$error))
    {
        return(query_result$result)
    } else {
        word_string <- stringr::str_extract(txt, "\\w+$")
        if (!quietly) warning(glue::glue("There is no record of the word {word_string}"))
        return(list(id = NA,
                    word = word_string,
                    freq = NA,
                    wordRank = NA,
                    frequencyClass = NA))
    }
}
