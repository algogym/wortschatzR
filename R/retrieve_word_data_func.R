#' Retrieve frequency data from Leipzig Wortschatz
#'
#' @param wordlist A character vector containing the words, for which the data is requested. Other types will be coerced into characters.
#' @param corpus Name of the corpus to be used. At the moment only "deu_news_2012" possible
#'
#' @return data.frame with the requested frequency data
#' @export
#'
#' @examples
#' my_wordlist <- c("Hund", "Faultier", "Kruoz")
#' my_word_data <- retrieve_word_data(my_wordlist)
#' my_word_data
retrieve_word_data <- function(wordlist, corpus="deu_news_2012"){
    wordlist <- as.character(wordlist)
    missing_word_data <- FALSE
    if(corpus == "deu_news_2012"){
        url_le <- "http://wortschatzwebservices.informatik.uni-leipzig.de/ws/words/deu_news_2012_1M/word/"
    }
    word_data_df <- data.frame(NULL)
    for (ii in 1:length(wordlist)){
        http_le <- paste(url_le, wordlist[ii], sep="")
        retrieved_data <- try(jsonlite::fromJSON(http_le), silent = TRUE)

        if(class(retrieved_data) != "try-error"){
            word_data_df <- rbind(word_data_df, as.data.frame(retrieved_data))
        }else{
            word_data_df <- rbind(word_data_df, data.frame(id = NA,word = wordlist[ii],freq = NA, wordRank = NA,frequencyClass = NA))
            missing_word_data <- TRUE
            }

    }
if(missing_word_data == TRUE){
    warning("One or more words did not have frequency data available in the chosen corpus")
}
word_data_df
}


