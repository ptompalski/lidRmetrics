# Percentiles

Percentiles of point heights

## Usage

``` r
metrics_percentiles(z, zmin = NA)

.metrics_percentiles
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

A list. Set of height percentiles including: `zq1`, `zq5`, `zq10`,
`zq15`, `zq20`, `zq25`, `zq30`, `zq35`, `zq40`, `zq45`, `zq50`, `zq55`,
`zq60`, `zq65`, `zq70`, `zq75`, `zq80`, `zq85`, `zq90`, `zq95`, and
`zq99`.

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_percentiles(z = Z))

m2 <- pixel_metrics(las, ~metrics_percentiles(z = Z, zmin = 2), res = 20)
```
