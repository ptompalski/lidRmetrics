#' Percent of points above threshold
#' 
#' Calculates percentage of points above specified threshold heights and mean height.
#' 
#' @inheritParams metrics_basic
#' @param threshold Numeric. Threshold height(s). Default = c(2, 5).
#' @return A list. Percent of points above mean and above threshold value(s).
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_percabove(z = Z))
#' 
#' m2 <- pixel_metrics(las, ~metrics_percabove(z = Z, zmin = 2), res = 20)
#' 
#' @export



metrics_percabove <- function(z, threshold = c(2,5), zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  assert_all_are_positive(threshold)
  
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


