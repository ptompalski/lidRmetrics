#' Pre-defined sets of metrics
#' 
#' Pre-defined sets of point cloud metrics composed of different metrics_* functions available in the package. 
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @param threshold Threshold height(s). See \link[=metrics_percabove]{metrics_percabove}.
#' @param dz layer thickness to use when calculating entropy and VCI.
#' @param interval_count Number of intervals used to divide the point height distribution. See \link[=metrics_canopydensity]{metrics_canopydensity}.
#' @param zintervals Height intervals. See \link[=metrics_interval]{metrics_interval}.
#' @export


metrics_set1 <- function(z, zmin=NA, threshold = c(2,5), dz=1, interval_count=10, zintervals=c(0, 0.15, 2, 5, 10, 20, 30)) {
  
  m_basic   <- metrics_basic(z=z, zmin=zmin)
  m_prctls  <- metrics_percentiles(z=z, zmin=zmin)
  m_percab  <- metrics_percabove(z = z, threshold = threshold, zmin = zmin)
  m_disp    <- metrics_dispersion(z=z, dz=dz, zmin=zmin)
  m_candens <- metrics_canopydensity(z=z, interval_count = interval_count, zmin = zmin)
  m_lmom    <- metrics_Lmoments(z=z, zmin=zmin)
  m_lad     <- metrics_lad(z=z, zmin=zmin)
  m_int     <- metrics_interval(z=z, zmin=zmin, zintervals = zintervals)
  
  m <- c(m_basic, m_prctls, m_percab, m_disp, m_candens, m_lmom, m_lad, m_int)
  
  return(m)
  
}



