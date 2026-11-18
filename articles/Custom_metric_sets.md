# Custom metric sets

``` r

library(lidR)
library(lidRmetrics)
```

The lidRmetrics package is designed to provide a collection of functions
for calculating point cloud metrics. Each function is specialized in
computing metrics that characterize a specific aspect of the point cloud
structure. This modular approach allows users to select and combine
functions to create a custom set of metrics tailored to their specific
needs.

The package includes three sets of predefined metrics, but users are
encouraged to develop their own. The following examples demonstrate how
to do this.

We’ll begin with a simple example by combining metrics_basic() and
metrics_percentiles():

``` r

my_metric_set <- function(z) {
  
  m_basic   <- lidRmetrics::metrics_basic(z=z)
  m_prctls  <- lidRmetrics::metrics_percentiles(z=z)
  
  m <- c(m_basic, m_prctls)
  
  return(m)
}
```

It’s important to note that each function is explicitly called with the
lidRmetrics package prefix. This practice is crucial when processing
data on multiple cores using the future package.

Let’s test the function and examine the outputs:

``` r

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
```

``` r


my_metrics <- pixel_metrics(las, ~my_metric_set(Z), res = 20)

head(as.data.frame(my_metrics),1)
#>     n zmax zmin    zmean     zvar      zsd      zcv      zskew    zkurt zq1
#> 1 103   22    0 11.95777 48.78116 6.984351 58.40849 -0.4572028 1.741565   0
#>     zq5 zq10  zq15  zq20 zq25  zq30   zq35   zq40   zq45  zq50   zq55   zq60
#> 1 0.202 1.28 2.267 3.438 4.68 6.414 10.405 11.846 13.623 14.29 15.058 16.088
#>     zq65   zq70  zq75   zq80   zq85   zq90   zq95    zq99
#> 1 17.041 17.518 17.68 18.062 18.421 19.178 20.813 21.7698
```

Now, let’s consider a more complex scenario. Suppose we want to
calculate metrics_basic() and metrics_percentiles(), but for each
metric, we want to consider two different scenarios: with and without a
height cutoff of 2 meters (zmin). We can easily adapt the previous
function to calculate both variants of each metric:

``` r


my_metric_set <- function(z) {
  
  # set the height threshold
  zmin <- 2
  
  # define the text that will be appended to metric names calculated with height threshold. This is to avoid duplicated metric names.
  zmin_label <- paste0(".above",zmin)
  
  # metrics_basic without height threshold
  m_basic   <- lidRmetrics::metrics_basic(z=z)
  
  # metrics_basic with height threshold. To make metric names unique, `zmin_label` is appended.
  m_basic_2   <- lidRmetrics::metrics_basic(z=z, zmin=zmin)
  names(m_basic_2) <- paste0(names(m_basic_2),zmin_label)
  
  # the same for metrics_percentiles
  m_prctls  <- lidRmetrics::metrics_percentiles(z=z)
  m_prctls_2  <- lidRmetrics::metrics_percentiles(z=z, zmin=zmin)
  names(m_prctls_2) <- paste0(names(m_prctls_2),zmin_label)
  
  m <- c(m_basic, m_prctls, m_basic_2, m_prctls_2)
  return(m)
}
```

The output of the function above will look as follows:

``` r

head(as.data.frame(my_metrics),1)
#>     n  zmax zmin    zmean     zvar      zsd      zcv      zskew    zkurt zq1
#> 1 102 21.97    0 12.37745 55.39565 7.442826 60.13214 -0.5287653 1.725683   0
#>      zq5  zq10 zq15 zq20   zq25  zq30   zq35   zq40   zq45   zq50    zq55
#> 1 0.0075 0.314 1.46 3.22 4.3925 6.917 10.513 12.262 13.767 15.815 16.9395
#>     zq60    zq65   zq70  zq75 zq80   zq85   zq90    zq95  zq99 n.above2
#> 1 17.228 17.5625 18.183 18.44 18.7 19.101 19.768 21.1785 21.77       86
#>   zmax.above2 zmin.above2 zmean.above2 zvar.above2 zsd.above2 zcv.above2
#> 1       21.97        2.82     14.61674    33.44941   5.783546   39.56795
#>   zskew.above2 zkurt.above2 zq1.above2 zq5.above2 zq10.above2 zq15.above2
#> 1   -0.8554666     2.438011     2.8285     3.2225       4.055        6.04
#>   zq20.above2 zq25.above2 zq30.above2 zq35.above2 zq40.above2 zq45.above2
#> 1        9.48      11.305       13.13      13.785       15.43       16.62
#>   zq50.above2 zq55.above2 zq60.above2 zq65.above2 zq70.above2 zq75.above2
#> 1      17.065      17.475       17.62     18.2625       18.44      18.695
#>   zq80.above2 zq85.above2 zq90.above2 zq95.above2 zq99.above2
#> 1       18.87      19.215       20.72       21.43        21.8
```

Finally, metrics can be calculated for different echo types (e.g., for
first returns only, for all returns, etc.). This can be achieved using
the built-in functionality of the lidR package:

``` r

my_metrics <- pixel_metrics(las, ~my_metric_set(Z), by_echo=c("first", "all"))
```

The output will include the results of metrics_basic() and
metrics_percentiles(), calculated for the following scenarios: all
returns with no height threshold, all returns above 2 meters, first
returns only with no height threshold, and finally, first returns above
2 meters. Since point cloud subsetting is performed during metric
calculation, it is important to avoid applying any filtering when
loading the point cloud data or creating the catalog.

``` r

head(as.data.frame(my_metrics),1)
#>     n  zmax zmin    zmean     zvar      zsd      zcv      zskew    zkurt zq1
#> 1 102 21.97    0 12.37745 55.39565 7.442826 60.13214 -0.5287653 1.725683   0
#>      zq5  zq10 zq15 zq20   zq25  zq30   zq35   zq40   zq45   zq50    zq55
#> 1 0.0075 0.314 1.46 3.22 4.3925 6.917 10.513 12.262 13.767 15.815 16.9395
#>     zq60    zq65   zq70  zq75 zq80   zq85   zq90    zq95  zq99 n.above2
#> 1 17.228 17.5625 18.183 18.44 18.7 19.101 19.768 21.1785 21.77       86
#>   zmax.above2 zmin.above2 zmean.above2 zvar.above2 zsd.above2 zcv.above2
#> 1       21.97        2.82     14.61674    33.44941   5.783546   39.56795
#>   zskew.above2 zkurt.above2 zq1.above2 zq5.above2 zq10.above2 zq15.above2
#> 1   -0.8554666     2.438011     2.8285     3.2225       4.055        6.04
#>   zq20.above2 zq25.above2 zq30.above2 zq35.above2 zq40.above2 zq45.above2
#> 1        9.48      11.305       13.13      13.785       15.43       16.62
#>   zq50.above2 zq55.above2 zq60.above2 zq65.above2 zq70.above2 zq75.above2
#> 1      17.065      17.475       17.62     18.2625       18.44      18.695
#>   zq80.above2 zq85.above2 zq90.above2 zq95.above2 zq99.above2 n.first
#> 1       18.87      19.215       20.72       21.43        21.8      63
#>   zmax.first zmin.first zmean.first zvar.first zsd.first zcv.first zskew.first
#> 1      21.97        0.7    16.66556    21.5526  4.642478  27.85672   -1.809631
#>   zkurt.first zq1.first zq5.first zq10.first zq15.first zq20.first zq25.first
#> 1    5.970935     2.405     4.403     10.444     13.758      14.75      16.26
#>   zq30.first zq35.first zq40.first zq45.first zq50.first zq55.first zq60.first
#> 1       16.8     17.132     17.482     17.575      17.87     18.331      18.44
#>   zq65.first zq70.first zq75.first zq80.first zq85.first zq90.first zq95.first
#> 1     18.679     18.752      19.08     19.236     20.331     20.956     21.735
#>   zq99.first n.above2.first zmax.above2.first zmin.above2.first
#> 1     21.846             62             21.97              3.45
#>   zmean.above2.first zvar.above2.first zsd.above2.first zcv.above2.first
#> 1           16.92306          17.65985         4.202363         24.83216
#>   zskew.above2.first zkurt.above2.first zq1.above2.first zq5.above2.first
#> 1          -1.759413            5.98208           3.7245            6.307
#>   zq10.above2.first zq15.above2.first zq20.above2.first zq25.above2.first
#> 1            12.786           13.8945             15.15            16.325
#>   zq30.above2.first zq35.above2.first zq40.above2.first zq45.above2.first
#> 1            16.992            17.208            17.518            17.598
#>   zq50.above2.first zq55.above2.first zq60.above2.first zq65.above2.first
#> 1             17.96           18.3355             18.44           18.6895
#>   zq70.above2.first zq75.above2.first zq80.above2.first zq85.above2.first
#> 1            18.761            19.095            19.248           20.4405
#>   zq90.above2.first zq95.above2.first zq99.above2.first
#> 1            20.958           21.7475            21.848
```
