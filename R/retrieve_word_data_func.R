#' Retrieve frequency data from Leipzig Wortschatz
#'
#' @param wordlist A character vector containing the words, for which the data is requested. Other types will be coerced into characters.
#' @param corpus Name of the corpus to be used. At the moment only "deu_news_2012" possible
#' @param as_dataframe logical; if True (default) a data.frame is returned, else a list
#'
#' @return Either data.frame or list with the requested frequency data
#' @export
#'
#' @examples
#' my_wordlist <- c("Hund", "Faultier", "Kruoz")
#' my_word_data <- retrieve_word_data(my_wordlist)
#' my_word_data
retrieve_word_data <- function(wordlist, corpus="deu_news_2012", as_dataframe = TRUE){
    wordlist <- as.character(wordlist)
    if(corpus == "deu_news_2012"){
        url_le <- "http://wortschatzwebservices.informatik.uni-leipzig.de/ws/words/deu_news_2012_1M/word/"
    }
    word_data_list <- vector(mode="list", length = length(wordlist))
    for (ii in 1:length(wordlist)){
        http_le <- paste(url_le, wordlist[[ii]], sep="")
        retrieved_data <- httr::content(httr::GET(http_le, httr::add_headers("Accept: application/json")))
        if (length(retrieved_data)==2){
            word_data_list[[ii]] = list(id = NA,word = wordlist[[ii]],freq = NA, wordRank = NA,frequencyClass = NA)
        }else{word_data_list[[ii]] <- retrieved_data
        }
    }
    if(as_dataframe){
        word_data_df <- do.call(rbind, lapply(word_data_list,unlist))
        word_data_df = as.data.frame(word_data_df)
    }else{
        word_data_list
    }

}




