# Height of median energy (HOME)

The function provided here aims to mimic the HOME metric, which is a
metric typically used with full-waveform lidar. HOME is calculated by
identifying an elevation that splits the total intensity into two equal
parts. Function is based on a similar metric implemented in LAStools.

## Usage

``` r
metrics_HOME(z, i, zmin = NA)

.metrics_HOME
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- i:

  Intensity

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

## Value

Height of median energy

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_HOME(z = Z, i = Intensity))

m2 <- grid_metrics(las, ~metrics_HOME(z = Z, i = Intensity), res = 20)
```
