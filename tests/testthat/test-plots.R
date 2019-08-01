context("plots")

test_that("wrongInputs lead to errors",
          {
              ed <- detectEvents(stationBData[1000:2000,-1],nIterationsRefit = 50,
                                                     verbosityLevel = 2,ignoreVarianceWarning = TRUE)
              expect_success(plot(ed))
          })
