#' Dispersion metrics
#' 
#' Metrics characterizing variation of point cloud heights. 
#' 
#' @param z Z coordinate of the point cloud
#' @param dz layer thickness to use when calculating entropy and VCI.
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Interquartile distance, mean absolute deviation (MAD) around the mean and median, canopy relief ratio
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_dispersion(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_dispersion(z = Z), res = 40)


metrics_dispersion <- function(z, dz=1, zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  m = list(
    
    ziqr = IQR(z), # Interquartile distance 
    
    
    # AAD (Average Absolute Deviation):
    # https://en.wikipedia.org/wiki/Average_absolute_deviation
    
    #MAD around the mean
    zMADmean = mean(abs(z - mean(z))),
    
    # MAD around the median - Median absolute deviation (median of the absolute deviations from the data's median)
    zMADmedian = median(abs(z - median(z))),
    
    # Canopy relief ratio ((mean - min) / (max – min))
    CRR = ((mean(z) - min(z)) / (max(z) - min(z))),
    
    zentropy = entropy(z[z>0], dz), #forcing z to be always above 0, otherwise entropy does not work
    
    VCI = VCI(z[z>0], zmax = max(z), by = dz) #forcing z to be always above 0, otherwise entropy does not work
  )
  return(m)
}