# Canopy texture metrics

Generates a suite of GLCM (Grey-Level Co-Occurence Matrix) metrics of a
canopy height model (CHM). CHM is calculated on the fly to allow easy
integration with e.g.
[`lidR::pixel_metrics`](https://rdrr.io/pkg/lidR/man/aggregate.html)
function.

## Usage

``` r
metrics_texture(x, y, z, pixel_size, zmin = NA, chm_algorithm = NULL, ...)

.metrics_texture
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

- chm_algorithm:

  Function used to generate the CHM. By default
  `lidR::p2r(na.fill = lidR::knnidw())` is used.

- ...:

  additional parameters passed to
  [`ForestTools::glcm()`](https://rdrr.io/pkg/ForestTools/man/glcm.html).

## Value

A list. GLCM metrics.

## Details

Function first uses the
[`lidR::rasterize_canopy()`](https://rdrr.io/pkg/lidR/man/rasterize.html)
algorithm to create a CHM.
[`ForestTools::glcm()`](https://rdrr.io/pkg/ForestTools/man/glcm.html)
function is then used to calculate GLCM statistics (see package manual
for details). This implementation of GLCM does not allow for missing
values - after CHM is created, any missing values are converted to 0.

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_texture(x = X, y = Y, z = Z, pixel_size = 1))
#> Registered S3 method overwritten by 'stars':
#>   method                  from
#>   st_interpolate_aw.stars sf  

m2 <- pixel_metrics(las, ~metrics_texture(x = X, y = Y, z = Z, pixel_size = 1), res = 20)
```
