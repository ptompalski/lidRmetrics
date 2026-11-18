# LAD metrics

Metrics based on the leaf area density.
[`lidR::LAD()`](https://rdrr.io/pkg/lidR/man/nstdmetrics.html) used to
calculate the leaf area density.

## Usage

``` r
metrics_lad(z, zmin = NA, dz = 1, k = 0.5, z0 = 2)

.metrics_lad
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

- dz:

  numeric. The thickness of the layers used (height bin)

- k:

  numeric. is the extinction coefficient

- z0:

  numeric. The bottom limit of the profile

## Value

A list. `lad_min`, `lad_max`, `lad_mean`, `lad_cv`, and `lad_sum`.

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_lad(z = Z))

m2 <- pixel_metrics(las, ~metrics_lad(z = Z), res = 20)
```
