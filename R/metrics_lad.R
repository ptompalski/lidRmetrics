#' LAD metrics
#' 
#' Metrics based on the leaf area density. \code{lidR::LAD()} used to calculate the leaf area density. 
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return min, max, mean, and cv, of the leaf area density profile.
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_lad(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_lad(z = Z), res = 40)


metrics_lad <- function(z, zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  lad_max <- lad_mean <- lad_cv <- lad_min <- NA_real_
  
  if(length(z) > 2) {
    
    ladprofile = lidR::LAD(z)
    
    lad_max = with(ladprofile, max(lad, na.rm = TRUE))
    lad_mean = with(ladprofile, mean(lad, na.rm = TRUE))
    lad_cv = with(ladprofile, sd(lad, na.rm=TRUE)/mean(lad, na.rm = TRUE))
    lad_min = with(ladprofile, min(lad, na.rm = TRUE))
    
  }
  
  lad_metrics <- list(lad_max = lad_max,
                      lad_mean = lad_mean,
                      lad_cv = lad_cv,
                      lad_min = lad_min)
  
  return(lad_metrics)
}