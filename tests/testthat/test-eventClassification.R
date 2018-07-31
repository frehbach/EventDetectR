context("eventClassification")

test_that("wrongInputs lead to errors",
          {
              x <- stationBData[1000:2000,-1]
              m <- buildEDModel(x)

              newDataReal <- stationBData[2001:2020,-1]
              expect_error(predict(m,newDataReal[,-1]),regexp = "Predictions dimensions do not match newData")
          })

test_that("prediction works correct",
          {
              x <- stationBData[1000:2000,-1]
              m <- buildEDModel(x)

              newDataReal <- stationBData[2001:2020,-1]
              p <- predict(m)
              expect_equal(nrow(p),10)
              expect_equal(ncol(p),ncol(stationBData)-1)
              expect_equal(anyNA(p),FALSE)
              expect_equal(is.numeric(p),TRUE)

              p <- predict(m,newDataReal)
              expect_equal(nrow(p),nrow(newDataReal))
              expect_equal(ncol(p),ncol(stationBData)-1+1)
              expect_equal(typeof(p$Event),"logical")

              m1 <- buildEDModel(x,postProcessorControl = list(nStandardDeviationsEventThreshhold = 1))
              m2 <- buildEDModel(x,postProcessorControl = list(nStandardDeviationsEventThreshhold = 50))
              p1 <- predict(m1,newDataReal)
              p2 <- predict(m2,newDataReal)
              expect_true(sum(p1$Event)>sum(p2$Event))
              expect_true(!any(p2$Event))
          })
