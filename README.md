# MEMTS

R package implementing Multivariate Error Measures for Time Series forecasting.

The error measures (MSE, MASE, MAPE, MASE, SMAPE, WAPE) are defined as a multivariate, aggregated version of their univariate counterpart.
The considered error measures are discussed in detail in the paper: [Hyndman, R. J., & Koehler, A. B. (2006). Another look at measures of forecast accuracy. International journal of forecasting, 22(4), 679-688.](https://www.sciencedirect.com/science/article/pii/S0169207006000239)

## How to Install

The preferred way to install this package is using devtools:

```r
devtools::install_github("jdestefani/MEMTS", upgrade_dependencies = FALSE)
```

## Overview

To see the full list of exported functions:

```{r}
library("MEMTS")
ls("package:MEMTS")
```

