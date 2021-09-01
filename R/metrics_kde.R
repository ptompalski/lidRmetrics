#' Kernel density estimation (KDE) metrics
#' 
#' Kernel density estimation applied to the distribution of point cloud elevation (Z). KDE allows to 
#' create a probability density function (using a Guassian kernel). The density function is then used to detect
#' peaks (function maxima). Based on similar metric available in Fusion (see references), modified.
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Number of peaks, elevation and density value of the peak with largest density
#' 
#' @references McGaughey, R.J., 2021. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. http://forsys.cfr.washington.edu/software/fusion/FUSION_manual.pdf
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_kde(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_kde(z = Z), res = 20)


#' @export
metrics_kde <- function(z, zmin=NA) {
  
  peaks_count <- kde_peaks_elev <- kde_peaks_value <- NULL
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  if (length(z) > 2) {
    peaks <- lidRmetrics:::get_peaks(z = z)  
    peaks_count <- nrow(peaks)
    peaks_highest <- peaks[peaks$y == max(peaks$y),]
    kde_peaks_elev <- peaks_highest[,1]
    kde_peaks_value <-  peaks_highest[,2]
  }
  
  out <- list(
    kde_peaks_count = peaks_count,
    kde_peaks_elev = kde_peaks_elev,
    kde_peaks_value = kde_peaks_value
  )
  
  return(out)
  
  
}





get_peaks <- function(z) {
  
  d <- stats::density(z)
  
  i <- which(diff(sign(diff(d$y))) < 0) + 1 #https://stackoverflow.com/questions/58785930/r-find-maximum-of-density-plot
  data.frame(x = d$x[i], y = d$y[i])
  
}