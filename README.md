# EventDetectR
## General Info
[EventDetectR](https://frehbach.github.io/EventDetectR/) is an R-package for detecting/classifiying events in time-series data.
It aims to combine multiple well-known R-packages like the forecast package to deliver an easily configurable tool for event detection.

## Current Project Status
<a href="http://www.repostatus.org/#wip"><img src="http://www.repostatus.org/badges/latest/wip.svg" alt="Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public." /></a>
[![Build Status](https://travis-ci.org/frehbach/EventDetectR.svg?branch=master)](https://travis-ci.org/frehbach/EventDetectR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/frehbach/EventDetectR?branch=master&svg=true)](https://ci.appveyor.com/project/frehbach/EventDetectR)
[![codecov](https://codecov.io/gh/frehbach/EventDetectR/branch/master/graph/badge.svg)](https://codecov.io/gh/frehbach/EventDetectR)

## Installation
The package can be installed directly from github:

```R
require(devtools)
install_github("frehbach/EventDetectR")
```

Soon the package will also be available on CRAN.

## Usage
The main function of the EventDetectR package is:

```R
detectEvents <- function(x,
                         windowSize = 100,
                         nIterationsRefit = 50,
                         verbosityLevel = 0,
                         dataPrepators = "ImputeTSInterpolation",
                         dataPreparationControl = list(),
                         buildModelAlgo = "ForecastETS",
                         buildModelControl = list(),
                         postProcessors = "bedAlgo",
                         postProcessorControl = list(),
                         ignoreVarianceWarning = TRUE)
```

'detectEvents' is used to detect events in a multi-dimensional time-series object / data.frame. 
This is done in a three steps approach:
  - Data Preparation
  - Model Building
  - Result Postprocessing
Each of these steps can be configured to use methodologies from multiple available packages.
For example, beneath normalization it might be desired to clean the data and impute any existing NA-values.
This can be done by simply adding a dataPreparator as a string name in the function call, e.g.:

```R
dataPrepators = c("ImputeTSInterpolation")
```

Additional parameters for the function can be passed via named variables in the control lists:
```R
dataPreparationControl = list("option" = "spline", "useNormalization" = FALSE)
```

All configurable package-functions can be shown via the supportedMethodFunctions:
```R
getSupportedPreparations()
getSupportedModels()
getSupportedPostProcessors()
```

Default configurations and control lists can be found via:
```R
getDefaultPreparationControl()
getDefaultModelControl()
getDefaultPostControl()
```

The event classification itself is shown in the graph below. #TODO make this graph look nice

![Alt text](doc/windowImage.png?raw=true "Title")

At each classification iteration, a window of 'windowSize' datapoints (shown in blue) is first prepared with the specified data preparators. Next, a model is fitted and used to predict the next 'nIterationsRefit' data into the future. The real data (shown in red) is compared to the prediction in order to calculate residuals. These residuals together with the specified classification threshholds is used to decide which data is considered as event and which is considered background. After the classification, the window is moved by 'nIterationsRefit' rows in the data.frame and the procedure is repeated until the end of the data.frame / time-series is reached, and thus all elements are classified.

-------------

## Graphical User Interface
A graphical user interface (GUI) for the EventDetectR package is currently in development. 
Check out the most recent status at: [EventDetectGUI](https://github.com/frehbach/EventDetectGUI)

-------------

Parts of this work were supported by the ''Ministerium für Kultur und Wissenschaft des Landes Nordrhein-Westfalen'' (FKZ: 005-1703-0011).
