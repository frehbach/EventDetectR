context("buildModel")

test_that("Error for string input",
          {
              x <- "SomeString"
              expect_that(buildModel(x), throws_error())
          })
