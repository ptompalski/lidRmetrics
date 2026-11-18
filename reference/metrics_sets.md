# Pre-defined sets of metrics

Pre-defined sets of point cloud metrics composed of different
metrics\_\* functions available in the package.

## Usage

``` r
metrics_set1(
  z,
  zmin = NA,
  threshold = c(2, 5),
  dz = 1,
  interval_count = 10,
  zintervals = c(0, 0.15, 2, 5, 10, 20, 30)
)

.metrics_set1

metrics_set2(
  z,
  ReturnNumber,
  NumberOfReturns,
  zmin = NA,
  threshold = c(2, 5),
  dz = 1,
  interval_count = 10,
  zintervals = c(0, 0.15, 2, 5, 10, 20, 30),
  KeepReturns = c(1, 2, 3, 4)
)

.metrics_set2

metrics_set3(
  x,
  y,
  z,
  i,
  ReturnNumber,
  NumberOfReturns,
  zmin = NA,
  threshold = c(2, 5),
  dz = 1,
  interval_count = 10,
  zintervals = c(0, 0.15, 2, 5, 10, 20, 30),
  pixel_size = 1,
  vox_size = 1,
  KeepReturns = c(1, 2, 3, 4)
)

.metrics_set3
```

## Format

An object of class `formula` of length 2.

An object of class `formula` of length 2.

An object of class `formula` of length 2.

## Arguments

- zmin:

  Minimum height. If set, heights below are ignored in calculations.

- threshold:

  Threshold height(s). See
  [metrics_percabove](https://ptompalski.github.io/lidRmetrics/reference/metrics_percabove.md).

- dz:

  layer thickness to use when calculating entropy and VCI.

- interval_count:

  Number of intervals used to divide the point height distribution. See
  [metrics_canopydensity](https://ptompalski.github.io/lidRmetrics/reference/metrics_canopydensity.md).

- zintervals:

  Height intervals. See
  [metrics_interval](https://ptompalski.github.io/lidRmetrics/reference/metrics_interval.md).

- ReturnNumber:

  return number

- NumberOfReturns:

  number of returns

- x, y, z:

  X, Y, Z coordinates of the point cloud

- i:

  Intensity

- pixel_size:

  pixel size for calculating rumple index

- vox_size:

  voxel size for calculating voxel metrics

## Details

The three pre-defined sets of metrics include different number of the
metrics\_\* functions and are provided to conventiently compute most of
the metrics commonly used at the same time.

- `metrics_set1()`:

  Included metrics:
  [`metrics_basic()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_basic.md),
  [`metrics_percentiles()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_percentiles.md),
  [`metrics_percabove()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_percabove.md),
  [`metrics_dispersion()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_dispersion.md),
  [`metrics_canopydensity()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_canopydensity.md),
  [`metrics_Lmoments()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_Lmoments.md),
  [`metrics_lad()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_lad.md),
  [`metrics_interval()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_interval.md).

- `metrics_set2()`:

  Included metrics: All metrics in `metrics_set1()`,
  [`metrics_echo()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_echo.md),
  and
  [`metrics_echo2()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_echo2.md).

- `metrics_set3()`:

  Included metrics: All metrics in `metrics_set2()`,
  [`metrics_kde()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_kde.md),
  [`metrics_HOME()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_HOME.md),
  [`metrics_rumple()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_rumple.md),
  [`metrics_voxels()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_voxels.md)

Currently no set includes
[`metrics_texture()`](https://ptompalski.github.io/lidRmetrics/reference/metrics_texture.md).
