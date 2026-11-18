# Percent of points above threshold

Calculates percentage of points above specified threshold heights and
mean height.

## Usage

``` r
metrics_percabove(z, threshold = c(2, 5), zmin = NA)

.metrics_percabove
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- threshold:

  Numeric. Threshold height(s). Default = c(2, 5).

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

## Value

A list. Percent of points above mean and above threshold value(s).

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_percabove(z = Z))

m2 <- pixel_metrics(las, ~metrics_percabove(z = Z, zmin = 2), res = 20)
```
