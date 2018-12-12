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
