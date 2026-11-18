# Dispersion metrics

Metrics characterizing the dispersion of point heights

## Usage

``` r
metrics_dispersion(z, dz = 1, zmin = NA, zmax = NA)

.metrics_dispersion
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud (point heights)

- dz:

  Numeric. Layer thickness to use when calculating entropy and VCI.

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

- zmax:

  Numeric. Maximum elevation for an entropy normalized to zmax.

## Value

A list. Calculated metrics include:

- `ziqr` interquartile distance

- `zMADmean` mean absolute deviation (MAD) around the mean

- `zMADmedian` mean absolute deviation (MAD) around the median

- `CRR` canopy relief ratio

- `zentropy` entropy

- `VCI` vertical complexity index. Optional - calculated only if the
  `zmax` parameter is provided.

## Details

When calculating `zentropy` and `VCI`, `z` values below 0 are removed.

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_dispersion(z = Z))

m2 <- pixel_metrics(las, ~metrics_dispersion(z = Z, dz = 2, zmax = 30), res = 20)
```
