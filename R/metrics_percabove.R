#' Percent of points above threshold
#' 
#' Calculates percentage of points above specified threshold heights (default = c(2, 5)) and mean height.
#' 
#' @param z Z coordinate of the point cloud
#' @param threshold Threshold height(s). 
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Percent of points above thresholds
#' @export



metrics_percabove <- function(z, threshold = c(2,5), zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  n <- length(z)
  
  pzabovex <- lapply(threshold, function(x) {
    lidR:::fast_countover(z, x)/n * 100
  })
  names(pzabovex) <- paste0("pzabove", threshold)
  
  pzabovemean <- lidR:::fast_countover(z, mean(z))/n * 100
  
  return(c(
    list(pzabovemean = pzabovemean),
    pzabovex
    ))
  
}


