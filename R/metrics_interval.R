#' Interval metrics
#' 
#' Percentage of points calculated for a set of horizontal layers. 
#' 
#' @param z Z coordinate of the point cloud
#' @param zintervals Height intervals
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Percentage of points within each height interval
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_interval(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_interval(z = Z), res = 40)

#' @export
metrics_interval <- function(z, zintervals=c(0, 0.15, 2, 5, 10, 20, 30), zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  allpts <- length(z)
  
  #define breaks for the histogram (z threshold(s))
  brks <- c(-Inf, zintervals, Inf)
  
  #use the built-in hist() function to calculate frequencies. how many points in each interval
  z_counts <- hist(z, breaks = brks, plot=F)$counts
  
  #convert to percentages
  z_prop <- z_counts / allpts * 100
  
  # add names
  names_temp <- paste0("pz_",as.character((zintervals)),"-",dplyr::lead(zintervals))
  names_temp[length(names_temp)] <- paste0("pz_above_",zintervals[length(zintervals)])
  names_temp <- c(paste0("pz_below_",zintervals[1]), names_temp)
  names(z_prop) <- names_temp
  
  return(as.list(z_prop))
}