# lidRmetrics
Additional point cloud metrics to use with *_metric functions in lidR. 


## Installation 

You can install the most recent version of the package by executing the code below:

```
install.packages("devtools")
devtools::install_github("ptompalski/lidRmetrics")
library(lidRmetrics)
```


## Example usage

All of the functions in lidRmetrics are designed to be used with one of the *_metrics functions in the lidR package (e.g. grid_metrics()). 

For example:

```
library(lidR)
library(lidRmetrics)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")

m1 <- cloud_metrics(las, ~metrics_basic(Z))

m2 <- grid_metrics(las, ~metrics_set1(Z))

m3 <- grid_metrics(las, ~metrics_set2(X, Y, Z), res = 20)
```