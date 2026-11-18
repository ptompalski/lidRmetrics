# Number and proportion of points by echo types

Based on the return number and number of returns, point cloud returns
are classified into different echo types. Two different classification
routines are applied. Under the first classification routine returns are
classified into First, Intermediate, and Last. Under the second
classification routine returns are classified into Single or Multiple.
Function then calculates point counts and proportions by each echo type.
Ratios of Last to First, Intermediate to First, and Multiple to Single,
are also calculated.

## Usage

``` r
metrics_echo(ReturnNumber, NumberOfReturns, z = NULL, zmin = NA)

.metrics_echo
```

## Format

An object of class `formula` of length 2.

## Arguments

- ReturnNumber:

  return number

- NumberOfReturns:

  number of returns

- z:

  Z coordinate of the point cloud (point heights)

- zmin:

  numeric. Minimum `z` value. If set, `z` values (heights) below are
  ignored in calculations.

## Value

A list. Calculated metrics include:

- Number of first (`n_first`), intermediate (`n_intermediate`), last
  (`n_last`), single (`n_single`), and multiple (`n_multiple`) returns

- Proportion of first (`p_first`), intermediate (`p_intermediate`), last
  (`p_last`), single (`p_single`), and multiple (`p_multiple`) returns

- Ratio of Last to First (`ratio_last_first`), Intermediate to First
  (`ratio_intermediate_first`), and Multiple to Single
  (`ratio_multiple_single`).

## Examples

``` r
library(lidR)
library(lidRmetrics)
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#> 
                                                                                


m1 <- cloud_metrics(las, ~metrics_echo(ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns))

m2 <- pixel_metrics(las, ~metrics_echo(ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns), res = 20)
```
