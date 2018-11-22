context("detectionQuality")

test_that("confusionMatrix works",
          {
              train <- geccoIC2018Train[15000:18000,]
              edObject <- detectEvents(train[,-c(1,11)],windowSize = 500, nIterationsRefit = 200,verbosityLevel = 2,
                                postProcessorControl = list(nStandardDeviationsEventThreshhold = 3))
              stat <- qualityStatistics(edObject, train$EVENT)
              expect_equal(typeof(stat), "list")
          })
