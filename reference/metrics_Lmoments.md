# L-moments and L-moment ratios

Calculates L-moments and L-moment ratios of point cloud heihts.

## Usage

``` r
metrics_Lmoments(z, zmin = NA)

.metrics_Lmoments
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

## Value

A list. L-moments and L-moment ratios

- L-moments: `L1`, `L2`, `L3`, `L4`

- L-moment ratios: `Lskew` (`L3`/`L2`), `Lkurt` (`L4`/`L2`), and
  `Lcoefvar` (`L2`/`L1`)

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_Lmoments(z = Z))

m2 <- pixel_metrics(las, ~metrics_Lmoments(z = Z, zmin = 2), res = 20)
```
