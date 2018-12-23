context("test-word-service")

test_that("word accepts only strings", {
 expect_error(word(1:3))
})

test_that("word output has same length as input",
{
    expect(nrow(word("Katze")) == 1,
           failure_message = "The output has not the same length as the input" )
    expect(nrow(word(c("Katze", "Pferd", "Elch"))) == 3,
           failure_message = "The output has not the same length as the input")
})

test_that("randomwords stops, on certain conditions",{
    expect_error(randomword(1001), info = "query length")
    expect_error(randomword(1.2), info = "double as n argument")
    expect_error(randomword(-1), info = "negative n")
    expect_error(randomword(10, force=1), info = "non logical force argument")
})

test_that("randomwords hat the correct dimensions", {
    expect(nrow(randomword(10) == 10),
           failure_message = "randomword output has not the expected no of rows")
    expect(ncol(randomword(10) == 3),
           failure_message = "randomword output has not the expected no of rows")
})

test_that("prefixword stops, on certain conditions",{
    expect_error(prefixword("fort", min_freq = -2), info = "invalid min_freq")
    expect_error(prefixword("fort", max_num=0), info = "invalid max_num")
    expect_error(prefixword("fort", force=1), info = "non logical force argument")
})


