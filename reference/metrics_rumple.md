# Calculate rumple index

A wrapper of the
[`lidR::rumple_index`](https://rdrr.io/pkg/lidR/man/nstdmetrics.html)
function that allows to calculate rumple index without the need for CHM,
and can be used directly in the e.g. `pixel_metrics` function. The
function combines the two required steps, i.e. creating a surface model,
and calculating rumple index, into one. Top surface is created using
highest points within each pixel.

## Usage

``` r
metrics_rumple(x, y, z, pixel_size, zmin = NA)

.metrics_rumple
```

## Format

An object of class `formula` of length 2.

## Arguments

- x, y, z:

  X, Y, Z coordinates of a point cloud

- pixel_size:

  pixel size

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

## Value

Same as
[`lidR::rumple_index`](https://rdrr.io/pkg/lidR/man/nstdmetrics.html) -
the calculated rumple index

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_rumple(x = X, y = Y, z = Z, pixel_size = 1))
#> Error: Package 'geometry' needed for this function to work. Please install it.

m2 <- pixel_metrics(las, ~metrics_rumple(x = X, y = Y, z = Z, pixel_size = 1), res = 20)
#> Error: Package 'geometry' needed for this function to work. Please install it.
```
