context("buildModel")

test_that("Error for string input",
          {
              ## Check for Wrong Data input
              x <- "SomeString"
              expect_error(buildEDModel(x), regexp = "x has to be a data.frame")

              x <- stationBData[1:500,]
              expect_error(buildEDModel(x), regexp = "non-numeric data")

              ## Check for wrong model or preparator names
              x <- stationBData[,-1]
              expect_error(buildEDModel(x, dataPrepators = "SomeWrongPrep")
                           , regexp = "not supported")
              expect_error(buildEDModel(x, buildModelAlgo = "SomeWrongAlgo")
                           , regexp = "not supported")
              expect_error(buildEDModel(x, buildModelAlgo = 7)
                           , regexp = "not supported")

              # Check Functionality
              expect_equal(class(buildEDModel(x)),"UnivariateForecast")
              expect_equal(length(buildEDModel(x)$modelList),ncol(x))
          })
