#' Percentiles
#' 
#' Percentiles of point heights
#' 
#' 
#' @inheritParams metrics_basic
#' @return A list. Set of height percentiles including: \code{zq1}, \code{zq5}, \code{zq10}, \code{zq15},
#' \code{zq20}, \code{zq25}, \code{zq30}, \code{zq35}, \code{zq40}, \code{zq45}, \code{zq50},
#' \code{zq55}, \code{zq60}, \code{zq65}, \code{zq70}, \code{zq75}, \code{zq80}, \code{zq85},
#' \code{zq90}, \code{zq95}, and \code{zq99}.
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_percentiles(z = Z))
#' 
#' m2 <- pixel_metrics(las, ~metrics_percentiles(z = Z, zmin = 2), res = 20)
#' 
#' @export


metrics_percentiles <- function(z, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  probs = c(0.01, seq(0.05, 0.95, 0.05),0.99)
  zq <- as.list(stats::quantile(z, probs))
  names(zq) <- paste0("zq", probs * 100)
  
  return(zq)
}


#' @rdname metrics_percentiles
#' @export
.metrics_percentiles = ~metrics_percentiles(Z)
