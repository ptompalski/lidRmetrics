#' Percentiles
#' 
#' Percentiles of point heights
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return A set of height percentiles
#' @export


metrics_percentiles <- function(z, zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  probs = c(0.01, seq(0.05, 0.95, 0.05),0.99)
  zq <- as.list(stats::quantile(z, probs))
  names(zq) <- paste0("zq", probs * 100)
  
  return(zq)
}
