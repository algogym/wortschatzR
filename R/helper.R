# This file contains helper functions used by other functions of the package

catch_error_json <- function(txt, quietly = TRUE){
    safe_json <- purrr::safely(jsonlite::fromJSON)
    query_result <- safe_json(txt)

    if (is.null(query_result$error))
    {
        return(query_result$result)
    } else {
        word_string <- str_extract(txt, "\\w+$")
        if (!quietly) warning(glue::glue("There is no record of the word {word_string}"))
        return(list(id = NA,
                    word = word_string,
                    freq = NA,
                    wordRank = NA,
                    frequencyClass = NA))
    }
}
