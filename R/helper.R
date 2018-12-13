# This file contains helper functions used by other functions of the package

#' Helper function which catches (404) errors, when requesting a JSON from a remote source
#'
#' @param txt String: URL to the JSON file
#' @param quietly If FALSE, a warning message is returned, whenever an error is encountered. Default: TRUE
#'
#' @return A list with the query results and the potential error message

catch_error_json <- function(txt, quietly = TRUE){
    safe_json <- purrr::safely(jsonlite::fromJSON)
    query_result <- safe_json(txt)
        if (!quietly & !is.null(query_result$error)) warning("An error occured during the query. Most likely the api request did not yield a result (404)")
    return(query_result)
    }
