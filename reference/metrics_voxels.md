# Voxel-based metrics

A set of metrics calculated in a voxel space, designed to be used within
the `pixel_metrics` or `cloud_metrics` function from the `lidR` package.
For convenience, a point cloud is converted to a voxel space on the fly,
without the need of using additional processing steps. Note, that
because of the additional computation required to convert a point cloud
to voxels, calculating voxel-based metrics is markedly slower than other
metrics\_\* functions.

## Usage

``` r
metrics_voxels(x, y, z, vox_size = 1, zmin = NA)

.metrics_voxels
```

## Format

An object of class `formula` of length 2.

## Arguments

- x, y, z:

  X, Y, Z coordinates of a point cloud to be converted into voxels

- vox_size:

  voxel size

- zmin:

  Minimum height. If set, heights below are ignored in calculations.

## Details

Calculated metrics include:

- `vn` count of filled voxels

- `FRall` and `FRcanopy`: filled ratio. For `FRall` a ratio between the
  number of filled voxels and all voxels located in the maximum extent
  of the point cloud. For `FRcanopy` empty voxels above the canopy are
  excluded

- metrics describing the vertical distribution of filled voxels:
  standard deviation (`vzsd`), coeficient of variation (`vzcv`), and
  vertical rumple (`vzrumple`).

- Canopy volume classes based on Lefsky et al 1999 (see references),
  modified. A voxel representation of a forest stand is divided into
  four classes including: open gap space, closed gap space, euphotic
  zone, and oligophotic zone.

## References

Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A.,
& Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and
Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote
Sensing of Environment, 70(3), 339-361.
doi:10.1016/S0034-4257(99)00052-8

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_voxels(x = X, y = Y, z = Z, vox_size = 1))

m2 <- pixel_metrics(las, ~metrics_voxels(x = X, y = Y, z = Z, vox_size = 1), res = 20)
```
