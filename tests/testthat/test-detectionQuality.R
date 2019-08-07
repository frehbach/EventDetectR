context("detectionQuality")


test_that("confusionMatrix works",
          {
              skip_on_cran()
              testthat::skip_on_os("windows")

              if(require("caret") & require("e1071")){
                  train <- geccoIC2018Train[16400:16600,]
                  edObject <- detectEvents(train[,-c(1,11)],windowSize = 70, nIterationsRefit = 50,verbosityLevel = 2,
                                           postProcessorControl = list(nStandardDeviationsEventThreshhold = 3))
                  stat <- qualityStatistics(edObject, train$EVENT)
                  expect_equal(typeof(stat), "list")
              }
          })
