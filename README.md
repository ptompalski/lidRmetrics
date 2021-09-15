
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
   <th style="text-align:left;"> Metrics name </th>
   <th style="text-align:left;"> Description </th>
   <th style="text-align:left;"> metrics_* function </th>
   <th style="text-align:left;"> Notes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> n </td>
   <td style="text-align:left;"> total number of returns </td>
   <td style="text-align:left;"> metrics_basic </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zmax, zmin, zmean, zsd, zcv, zskew, zkurt </td>
   <td style="text-align:left;"> elevation maximum, minimum, mean, standard deviation, coeficient of variation, skewness, and kurtosis </td>
   <td style="text-align:left;"> metrics_basic </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zq1, zq5, …, zq95, zq99 </td>
   <td style="text-align:left;"> elevation percentiles </td>
   <td style="text-align:left;"> metrics_percentiles </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pzabovemean, pzabove2, pzabove5 </td>
   <td style="text-align:left;"> percentage of returns above a threshold. By default, percent of returns above mean elevation, above 2 and 5 m are calculated. </td>
   <td style="text-align:left;"> metrics_percabove </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ziqr </td>
   <td style="text-align:left;"> interquartile distance </td>
   <td style="text-align:left;"> metrics_dispersion </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zMADmean, zMADmedian </td>
   <td style="text-align:left;"> mean absolute deviation from the mean, and from the median </td>
   <td style="text-align:left;"> metrics_dispersion </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CRR </td>
   <td style="text-align:left;"> canopy relief ratio ((mean - min) / (max – min)) </td>
   <td style="text-align:left;"> metrics_dispersion </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zentropy, VCI </td>
   <td style="text-align:left;"> normalized Shanon diversity index, Vertical Complexity Index </td>
   <td style="text-align:left;"> metrics_dispersion </td>
   <td style="text-align:left;"> van Ewijk, K. Y., Treitz, P. M., &amp; Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using LAS-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zpcum1, zpcum2,..., zpcum8, zpcum9 </td>
   <td style="text-align:left;"> canopy density metrics as defined by Woods et al 2008. Elevation range is divided into 10 equal intervals, and the cumulative proportion of returns in each interval is calculated. For example, zpcum3 is a cumulative percentage of returns located in lower 30% of maximum elevation. The results for the last (topmost) layer is not reported as it always equal to 100%. The number of layers (default = 10) can be specified by the user. </td>
   <td style="text-align:left;"> metrics_canopydensity </td>
   <td style="text-align:left;"> M. Woods, K. Lim, and P. Treitz. Predicting forest stand variables from LiDAR data in the Great Lakes – St. Lawrence forest of Ontario. The Forestry Chronicle. 84(6): 827-839. https://doi.org/10.5558/tfc84827-6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> L1, L2, L3, L4, Lskew, Lkurt, Lcoefvar </td>
   <td style="text-align:left;"> L-moments (L1, L2, L3, L4), L-moment skewness and kurtosis, L-moment coeficient of variation </td>
   <td style="text-align:left;"> metrics_Lmoments </td>
   <td style="text-align:left;"> requires the {Lmoments} package </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lad_max, lad_mean, lad_cv, lad_min </td>
   <td style="text-align:left;"> Metrics based on the leaf area density </td>
   <td style="text-align:left;"> metrics_lad </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pz_below_0, pz_0.0.15, pz_0.15.2, pz_2.5, pz_5.10, pz_10.20, pz_20.30, pz_above_30 </td>
   <td style="text-align:left;"> Interval metrics - proportion of returns between specified elevation intervals. Default intervals are: 0, 0.15, 2, 5, 10, 20, and 30. </td>
   <td style="text-align:left;"> metrics_interval </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rumple </td>
   <td style="text-align:left;"> A wrapper function for the rumple metric </td>
   <td style="text-align:left;"> metrics_rumple </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vn </td>
   <td style="text-align:left;"> total number of filled voxels </td>
   <td style="text-align:left;"> metrics_voxels </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vFRall, vFRcanopy, </td>
   <td style="text-align:left;"> filled ratio; FRall - a ratio between the number of filled voxels and all voxels located in the maximum extent of the point cloud. In case of  FRcanopy empty voxels above the canopy are excluded in the calculations </td>
   <td style="text-align:left;"> metrics_voxels </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vzrumple </td>
   <td style="text-align:left;"> vertical rumple </td>
   <td style="text-align:left;"> metrics_voxels </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vzsd, vzcv </td>
   <td style="text-align:left;"> voxel elevation standard deviation and coeficient of variation </td>
   <td style="text-align:left;"> metrics_voxels </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OpenGapSpace, ClosedGapSpace, Euphotic, Oligophotic </td>
   <td style="text-align:left;"> Canopy volume classes based on Lefsky et al 1999 </td>
   <td style="text-align:left;"> metrics_voxels </td>
   <td style="text-align:left;"> Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A., &amp; Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote Sensing of Environment, 70(3), 339–361. doi:10.1016/S0034-4257(99)00052-8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kde_peaks_count, kde_peaks_elev, kde_peaks_value </td>
   <td style="text-align:left;"> Kernel density estimation applied to the distribution of point cloud elevation (Z). KDE allows to create a probability density function (using a Guassian kernel). The density function is then used to detect peaks (function maxima). Based on similar metric available in Fusion (see references), modified. </td>
   <td style="text-align:left;"> metrics_kde </td>
   <td style="text-align:left;"> McGaughey, R.J., 2021. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. http://forsys.cfr.washington.edu/software/fusion/FUSION_manual.pdf </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pFirst pIntermediate pLast pSingle pMultiple </td>
   <td style="text-align:left;"> Percentage of returns by echo types (First, Intermediate, Last; and Single, Multiple) </td>
   <td style="text-align:left;"> metrics_echo </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HOME </td>
   <td style="text-align:left;"> height of median energy </td>
   <td style="text-align:left;"> metrics_HOME </td>
   <td style="text-align:left;"> calculations based on LAStools' implementation of the HOME metric. http://lastools.org/download/lascanopy_README.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glcm_mean, glcm_variance, glcm_autoCorrelation, glcm_cProminence, glcm_cShade, glcm_cTendency, glcm_contrast, glcm_correlation, glcm_differenceEntropy, glcm_dissimilarity, glcm_energy, glcm_entropy, glcm_homogeneity1, glcm_homogeneity2, glcm_IDMN, glcm_IDN, glcm_inverseVariance, glcm_maxProb, glcm_sumAverage, glcm_sumEntropy, glcm_sumVariance </td>
   <td style="text-align:left;"> GLCM (Grey-Level Co-Occurence Matrix) metrics of a canopy height model (CHM) </td>
   <td style="text-align:left;"> metrics_texture </td>
   <td style="text-align:left;"> requires the {ForestTools} package. ForestTools::glcm_img() function is used to calculate the GLCM statistics (see package manual for details) </td>
  </tr>
</tbody>
</table>
