# Basic metrics

Most common descriptive statistics used to characterize the vertical
distribution of points in a point cloud.

## Usage

``` r
metrics_basic(z, zmin = NA)

.metrics_basic
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

A set of descriptive statistics including: total number of points,
maximum height, minimum height, mean height, variance of height,
standard deviation of height, coefficient of variation of height,
skewness and kurtosis of height.

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


#================
# CLOUD METRICS
#================

m1 <- cloud_metrics(las, ~metrics_basic(z = Z))

#================
# PIXEL METRICS
#================

m2 <- pixel_metrics(las, ~metrics_basic(z = Z), res = 20)


#================
# PLOT METRICS
#================

shpfile <- system.file("extdata", "efi_plot.shp", package="lidR")
inventory <- sf::st_read(shpfile, quiet = TRUE)

m3 <- plot_metrics(las, ~metrics_basic(z = Z,  zmin = 2), inventory, radius = 11.28)
```
