# Interval metrics

Compute the percentage of points falling within user-defined height
intervals.

## Usage

``` r
metrics_interval(
  z,
  zintervals = c(0, 0.15, 2, 5, 10, 20, 30),
  zmin = NA_real_,
  right = FALSE
)

.metrics_interval
```

## Format

An object of class `formula` of length 2.

## Arguments

- z:

  A numeric vector of heights (e.g., \`Z\` from a LAS object).

- zintervals:

  Numeric, strictly increasing. Interior breakpoints for height
  intervals (e.g., `c(0, 0.15, 2, 5, 10, 20, 30)`). The full set of
  breaks used is `c(-Inf, zintervals, Inf)`, producing:

  - a "below" bin: `(-Inf, zintervals[1])`

  - interior bins: `[zintervals[i], zintervals[i+1])`

  - an "above" bin: `[zintervals[length(zintervals)], Inf)`

  With `right = FALSE` (used internally), each bin includes its left
  boundary and excludes its right boundary, except the open-ended top
  bin which includes all values \\\ge\\ the last breakpoint.

- zmin:

  Optional numeric scalar. If provided, values are filtered as
  `z > zmin` before binning (strictly greater).

- right:

  Logical, default \`FALSE\`. Passed to \`hist()\` to control whether
  intervals are right-closed (\`TRUE\`) or right-open (\`FALSE\`).

## Value

A named list of percentages (numeric) for each interval.

## Details

Internally, counts are computed with:


      breaks <- c(-Inf, zintervals, Inf)
      hist(z, breaks = breaks, plot = FALSE, right = FALSE)

Using \`right = FALSE\` enforces left-closed, right-open bins
(\`\[a,b)\`), which guarantees, for example, that \`0\` is included in
\`\[0, next)\` rather than the "below" bin. Proportions are the counts
divided by the number of points remaining after optional \`zmin\`
filtering, multiplied by 100.

Output names follow:

- `pz_below_<first>` for the below bin

- `pz_<a>-<b>` for each interior `[a,b)` bin

- `pz_above_<last>` for the above bin

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_interval(z = Z))

m2 <- pixel_metrics(las, ~metrics_interval(z = Z, zintervals = c(0, 5, 10)), res = 20)
```
