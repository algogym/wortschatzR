context("test-helper")

test_that("catch_error_json issues a warning if not quiet", {
  expect_warning(
      catch_error_json(
          "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit" ,
          quietly = FALSE ))
})

test_that("check that catch_error_json sets the right NAs",
          {
              expect(is.na(catch_error_json(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
                  )$id), "id is not NA")
              expect(!is.na(catch_error_json(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$word), "word is NA")
              expect(is.na(catch_error_json(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$freq), "freq is not NA")
              expect(is.na(catch_error_json(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$wordRank), "wordRank is not NA")
              expect(is.na(catch_error_json(
                  "http://api.corpora.uni-leipzig.de/ws/words/deu_news_2012_1M/word/Wookievomit"
              )$frequencyClass), "frequencyClass is not NA")
          }
          )
