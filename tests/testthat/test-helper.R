context("test-helper")

test_that("catch_error_json issues a warning if not quiet", {
  expect_warning(
      catch_error_json(
          "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit" ,
          quietly = FALSE ))
})

test_that("check that query word sets the right NAs",
          {
              expect(is.na(query_word(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
                  )$id), "id is not NA")
              expect(!is.na(query_word(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$word), "word is NA")
              expect(is.na(query_word(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$freq), "freq is not NA")
              expect(is.na(query_word(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$wordRank), "wordRank is not NA")
              expect(is.na(query_word(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$frequencyClass), "frequencyClass is not NA")
          }
          )

test_that("the format of the available corpora is right",
          {
           testthat::expect_is(list_available_corpora(), "tbl_df")
           testthat::expect_is(list_available_corpora(TRUE), "character")
          })

test_that("available corpora throws the right errors",
          {
             testthat::expect_error(list_available_corpora(1))
          })
