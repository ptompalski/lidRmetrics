# Canopy density metrics

Cumulative distribution of point heights in interval count.

## Usage

``` r
metrics_canopydensity(z, interval_count = 10, zmin = NA)

.metrics_canopydensity
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  Z coordinate of the point cloud

- interval_count:

  Number of intervals used to divide the point height distribution.

- zmin:

  Minimum height. If set, heights below are ignored in calculations.

## Value

Cumulative percentage of points in each interval
