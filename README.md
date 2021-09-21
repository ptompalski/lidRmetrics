
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lidRmetrics

Additional point cloud metrics to use with \*\_metric functions in lidR.

## Installation

You can install the most recent version of the package by executing the
code below:

``` r
devtools::install_github("ptompalski/lidRmetrics")
library(lidRmetrics)
```

## Example usage

All of the functions in `lidRmetrics` are designed to be used with one
of the \*\_metrics functions in the `lidR` package
(e.g. `grid_metrics()`).

For example:

``` r
library(lidR)
library(lidRmetrics)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")

m1 <- cloud_metrics(las, ~metrics_basic(Z))

m2 <- grid_metrics(las, ~metrics_set1(Z), res = 20)

m3 <- grid_metrics(las, ~metrics_set2(X, Y, Z), res = 20)
```

## List of metrics

<table>
<thead>
<tr>
<th style="text-align:left;">
<img width="132" height="1">
Metrics name
</th>
<th style="text-align:left;">
<img width="353" height="1">
Description
</th>
<th style="text-align:left;">
<img width="132" height="1">
metrics\_\* function
</th>
<th style="text-align:left;">
<img width="265" height="1">
Notes
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
total number of returns
</td>
<td style="text-align:left;">
metrics\_basic
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
zmax, zmin, zmean, zsd, zcv, zskew, zkurt
</td>
<td style="text-align:left;">
elevation maximum, minimum, mean, standard deviation, coeficient of
variation, skewness, and kurtosis
</td>
<td style="text-align:left;">
metrics\_basic
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
zq1, zq5, …, zq95, zq99
</td>
<td style="text-align:left;">
elevation percentiles
</td>
<td style="text-align:left;">
metrics\_percentiles
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
pzabovemean, pzabove2, pzabove5
</td>
<td style="text-align:left;">
percentage of returns above a threshold. By default, percent of returns
above mean elevation, above 2 and 5 m are calculated.
</td>
<td style="text-align:left;">
metrics\_percabove
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
ziqr
</td>
<td style="text-align:left;">
interquartile distance
</td>
<td style="text-align:left;">
metrics\_dispersion
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
zMADmean, zMADmedian
</td>
<td style="text-align:left;">
mean absolute deviation from the mean, and from the median
</td>
<td style="text-align:left;">
metrics\_dispersion
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
CRR
</td>
<td style="text-align:left;">
canopy relief ratio ((mean - min) / (max – min))
</td>
<td style="text-align:left;">
metrics\_dispersion
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
zentropy, VCI
</td>
<td style="text-align:left;">
normalized Shanon diversity index, Vertical Complexity Index
</td>
<td style="text-align:left;">
metrics\_dispersion
</td>
<td style="text-align:left;">
van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing
Forest Succession in Central Ontario using LAS-derived Indices.
Photogrammetric Engineering and Remote Sensing, 77(3), 261-269
</td>
</tr>
<tr>
<td style="text-align:left;">
zpcum1, zpcum2,…, zpcum8, zpcum9
</td>
<td style="text-align:left;">
canopy density metrics as defined by Woods et al 2008. Elevation range
is divided into 10 equal intervals, and the cumulative proportion of
returns in each interval is calculated. For example, zpcum3 is a
cumulative percentage of returns located in lower 30% of maximum
elevation. The results for the last (topmost) layer is not reported as
it always equal to 100%. The number of layers (default = 10) can be
specified by the user.
</td>
<td style="text-align:left;">
metrics\_canopydensity
</td>
<td style="text-align:left;">
M. Woods, K. Lim, and P. Treitz. Predicting forest stand variables from
LiDAR data in the Great Lakes – St. Lawrence forest of Ontario. The
Forestry Chronicle. 84(6): 827-839. <https://doi.org/10.5558/tfc84827-6>
</td>
</tr>
<tr>
<td style="text-align:left;">
L1, L2, L3, L4, Lskew, Lkurt, Lcoefvar
</td>
<td style="text-align:left;">
L-moments (L1, L2, L3, L4), L-moment skewness and kurtosis, L-moment
coeficient of variation
</td>
<td style="text-align:left;">
metrics\_Lmoments
</td>
<td style="text-align:left;">
requires the {Lmoments} package
</td>
</tr>
<tr>
<td style="text-align:left;">
lad\_max, lad\_mean, lad\_cv, lad\_min
</td>
<td style="text-align:left;">
Metrics based on the leaf area density
</td>
<td style="text-align:left;">
metrics\_lad
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
pz\_below\_0, pz\_0.0.15, pz\_0.15.2, pz\_2.5, pz\_5.10, pz\_10.20,
pz\_20.30, pz\_above\_30
</td>
<td style="text-align:left;">
Interval metrics - proportion of returns between specified elevation
intervals. Default intervals are: 0, 0.15, 2, 5, 10, 20, and 30.
</td>
<td style="text-align:left;">
metrics\_interval
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
rumple
</td>
<td style="text-align:left;">
A wrapper function for the rumple metric
</td>
<td style="text-align:left;">
metrics\_rumple
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
vn
</td>
<td style="text-align:left;">
total number of filled voxels
</td>
<td style="text-align:left;">
metrics\_voxels
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
vFRall, vFRcanopy,
</td>
<td style="text-align:left;">
filled ratio; FRall - a ratio between the number of filled voxels and
all voxels located in the maximum extent of the point cloud. In case of
FRcanopy empty voxels above the canopy are excluded in the calculations
</td>
<td style="text-align:left;">
metrics\_voxels
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
vzrumple
</td>
<td style="text-align:left;">
vertical rumple
</td>
<td style="text-align:left;">
metrics\_voxels
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
vzsd, vzcv
</td>
<td style="text-align:left;">
voxel elevation standard deviation and coeficient of variation
</td>
<td style="text-align:left;">
metrics\_voxels
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
OpenGapSpace, ClosedGapSpace, Euphotic, Oligophotic
</td>
<td style="text-align:left;">
Canopy volume classes based on Lefsky et al 1999
</td>
<td style="text-align:left;">
metrics\_voxels
</td>
<td style="text-align:left;">
Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A.,
& Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and
Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote
Sensing of Environment, 70(3), 339–361.
<doi:10.1016/S0034-4257(99)00052-8>
</td>
</tr>
<tr>
<td style="text-align:left;">
kde\_peaks\_count, kde\_peaks\_elev, kde\_peaks\_value
</td>
<td style="text-align:left;">
Kernel density estimation applied to the distribution of point cloud
elevation (Z). KDE allows to create a probability density function
(using a Guassian kernel). The density function is then used to detect
peaks (function maxima). Based on similar metric available in Fusion
(see references), modified.
</td>
<td style="text-align:left;">
metrics\_kde
</td>
<td style="text-align:left;">
McGaughey, R.J., 2021. FUSION/LDV: Software for LIDAR Data Analysis and
Visualization.
<http://forsys.cfr.washington.edu/software/fusion/FUSION_manual.pdf>
</td>
</tr>
<tr>
<td style="text-align:left;">
pFirst pIntermediate pLast pSingle pMultiple
</td>
<td style="text-align:left;">
Percentage of returns by echo types (First, Intermediate, Last; and
Single, Multiple)
</td>
<td style="text-align:left;">
metrics\_echo
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
HOME
</td>
<td style="text-align:left;">
height of median energy
</td>
<td style="text-align:left;">
metrics\_HOME
</td>
<td style="text-align:left;">
calculations based on LAStools’ implementation of the HOME metric.
<http://lastools.org/download/lascanopy_README.txt>
</td>
</tr>
</tbody>
</table>
