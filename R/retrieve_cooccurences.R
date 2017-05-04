#' Retrieve left cooccurences for a specific word from Leipzig Wortschatz
#'
#' @param wordlist A character vector containing the words, for which the data is requested. Other types will be coerced into characters.
#' @param corpus Name of the corpus to be used. At the moment only "deu_news_2012" possible
#' @param limit Number of most significant left cooccurrences. Must be larger than 2
#'
#' @return data.frame with the requested left cooccurence frequencies
#' @export
#'
#' @examples
#' my_wordlist <- c("Hund", "Faultier", "Kruoz")
#' my_word_data <- retrieve_left_cooccurences(my_wordlist)
#' my_word_data
retrieve_left_cooccurences <- function(wordlist, limit=20,  corpus="deu_news_2012"){
    wordlist <- as.character(wordlist)

    if(corpus == "deu_news_2012"){
        url_le <- "http://api.corpora.uni-leipzig.de/ws/cooccurrences/deu_news_2012_1M/leftcooccurrences/"
    }
    missing_word_data <- FALSE
    word_data_df <- data.frame(NULL)
    for (ii in 1:length(wordlist)){
        http_le_b <- paste(url_le, wordlist[ii], sep="")
        http_le <- paste(http_le_b, limit, sep="?limit=")
        retrieved_data <- try(jsonlite::fromJSON(http_le, flatten = TRUE), silent = TRUE)
        if(class(retrieved_data) != "try-error"){
            word_data_df <- rbind(word_data_df, as.data.frame(retrieved_data))
        }else{
            word_data_df <- rbind(word_data_df, data.frame(w1.id = NA, w1.word = wordlist[ii], w1.freq = NA, w2.id = NA, w2.word = NA, w2.freq = NA, freq = NA, sig = NA))
            missing_word_data <- TRUE
        }

    }

    if(missing_word_data == TRUE){
        warning("One or more words did not have cooccurence data available in the chosen corpus")
    }
    word_data_df
}


#' Retrieve right cooccurences for a specific word from Leipzig Wortschatz
#'
#' @param wordlist A character vector containing the words, for which the data is requested. Other types will be coerced into characters.
#' @param corpus Name of the corpus to be used. At the moment only "deu_news_2012" possible
#' @param limit Number of most significant right cooccurrences. Must be larger than 2
#'
#' @return data.frame with the requested right cooccurence frequencies
#' @export
#'
#' @examples
#' my_wordlist <- c("Hund", "Faultier", "Kruoz")
#' my_word_data <- retrieve_right_cooccurences(my_wordlist)
#' my_word_data
retrieve_right_cooccurences <- function(wordlist, limit=20,  corpus="deu_news_2012"){
    wordlist <- as.character(wordlist)

    if(corpus == "deu_news_2012"){
        url_le <- "http://api.corpora.uni-leipzig.de/ws/cooccurrences/deu_news_2012_1M/rightcooccurrences/"
    }
    missing_word_data <- FALSE
    word_data_df <- data.frame(NULL)
    for (ii in 1:length(wordlist)){
        http_le_b <- paste(url_le, wordlist[ii], sep="")
        http_le <- paste(http_le_b, limit, sep="?limit=")
        retrieved_data <- try(jsonlite::fromJSON(http_le, flatten = TRUE), silent = TRUE)
        if(class(retrieved_data) != "try-error"){
            word_data_df <- rbind(word_data_df, as.data.frame(retrieved_data))
        }else{
            word_data_df <- rbind(word_data_df, data.frame(w1.id = NA, w1.word = wordlist[ii], w1.freq = NA, w2.id = NA, w2.word = NA, w2.freq = NA, freq = NA, sig = NA))
            missing_word_data <- TRUE
        }

    }

    if(missing_word_data == TRUE){
        warning("One or more words did not have cooccurence data available in the chosen corpus")
    }
    word_data_df
}
