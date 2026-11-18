# Kernel density estimation (KDE) metrics

Kernel density estimation applied to the distribution of point cloud
elevation (Z). KDE allows to create a probability density function
(using a Guassian kernel). The density function is then used to detect
peaks (function maxima). Based on similar metric available in Fusion
(see references), with significant differences in the list of output
statistics as well as the default bandwidth used when estimating kernel
density.

## Usage

``` r
metrics_kde(z, bw = 2, zmin = NA, npeaks = 4, ...)

.metrics_kde
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- bw:

  Numeric. Smoothing bandwidth of the
  [`stats::density`](https://rdrr.io/r/stats/density.html) function.
  Note that the default value (`bw=2`) does not correspond to the
  default `bw` parameter in
  [`stats::density`](https://rdrr.io/r/stats/density.html).

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

- npeaks:

  Numeric. Maximum number of recorded peaks. If the number of detected
  peaks is larger than `npeaks`, only the peaks with highest density
  value are kept.

- ...:

  Other parameters of the
  [`stats::density`](https://rdrr.io/r/stats/density.html) function

## Value

A list. Number of peaks, elevation, and density value of each peak,
distance (height difference) between peaks

## References

McGaughey, R.J., 2021. FUSION/LDV: Software for LIDAR Data Analysis and
Visualization.
http://forsys.cfr.washington.edu/software/fusion/FUSION_manual.pdf

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_kde(z = Z))

m2 <- pixel_metrics(las, ~metrics_kde(z = Z), res = 20)
```
