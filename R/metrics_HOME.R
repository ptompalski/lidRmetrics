

#' Height of median energy (HOME)
#' 
#' The function provided here aims to mimic the HOME metric, which is a metric typically used with full-waveform lidar. 
#' HOME is calculated by identifying an elevation that splits the total intensity into two equal parts. Function is based
#' on a similar metric implemented in LAStools.
#' 
#' @param z Z coordinate of the point cloud
#' @param i Intensity 
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Height of median energy
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_HOME(z = Z, i = Intensity))
#' 
#' m2 <- grid_metrics(las, ~metrics_HOME(z = Z, i = Intensity), res = 20)

metrics_HOME <- function(z, i, zmin=NA) {
  
  D <- data.frame(z, i)
  
  if (!is.na(zmin)) {
    D <- dplyr::filter(D, z > zmin)
  }
  
  D <- dplyr::arrange(D, z) %>%
    dplyr::mutate(int_csum1 = cumsum(i)) %>%
    dplyr::arrange(-z) %>%
    dplyr::mutate(int_csum2 = cumsum(i)) %>%
    dplyr::mutate(delta_int_csum = abs(int_csum1 - int_csum2))
  
  return(list(HOME = D[which.min(D$delta_int_csum),"z"]))
  
}

















  