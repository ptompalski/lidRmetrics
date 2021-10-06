#' Pre-defined sets of metrics
#' 
#' Pre-defined sets of point cloud metrics composed of different metrics_* functions available in the package. 
#' 
#' @details The three pre-defined sets of metrics include different number of the metrics_* functions and are provided 
#' to conventiently compute most of the metrics commonly used at the same time.
#' \describe{
#'   \item{\code{metrics_set1()}}{Included metrics: \code{metrics_basic()}, \code{metrics_percentiles()},  
#'   \code{metrics_percabove()}, \code{metrics_dispersion()},  \code{metrics_canopydensity()}, 
#'   \code{metrics_Lmoments()}, \code{metrics_lad()}, \code{metrics_interval()}.}
#'   \item{\code{metrics_set2()}}{Included metrics: All metrics in \code{metrics_set1()}, \code{metrics_rumple()},
#'   \code{metrics_voxels()}. }
#'   \item{\code{metrics_set3()}}{Included metrics: All metrics in \code{metrics_set2()}, \code{metrics_kde()}, 
#'    \code{metrics_echo()}, \code{metrics_HOME()}}.
#' }
#' Currently no set includes \code{metrics_texture()} as the function is considered experimental at this stage.
#' 
#' 
#' 
#' @param x,y,z X, Y, Z coordinates of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @param threshold Threshold height(s). See \link[=metrics_percabove]{metrics_percabove}.
#' @param dz layer thickness to use when calculating entropy and VCI.
#' @param interval_count Number of intervals used to divide the point height distribution. See \link[=metrics_canopydensity]{metrics_canopydensity}.
#' @param zintervals Height intervals. See \link[=metrics_interval]{metrics_interval}.
#' @param pixel_size pixel size for calculating rumple index
#' @param vox_size voxel size for calculating voxel metrics


#' @rdname metrics_sets
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

#' @rdname metrics_sets
#' @export
metrics_set2  <- function(x, y, z, 
                          zmin=NA, 
                          threshold = c(2,5), 
                          dz=1, 
                          interval_count=10, 
                          zintervals=c(0, 0.15, 2, 5, 10, 20, 30),
                          pixel_size=1,
                          vox_size=1) {
  
  m_set1    <- metrics_set1(z = z, zmin = zmin, threshold = threshold, dz = dz, interval_count = interval_count, zintervals = zintervals)
  m_rumple  <- metrics_rumple(x = x, y = y, z = z, pixel_size = pixel_size)
  m_vox     <- metrics_voxels(x = x, y = y, z = z, vox_size = vox_size, zmin = zmin)
  
  m <- c(m_set1, m_rumple, m_vox)
  
  return(m)
  
}


#' @rdname metrics_sets
#' @export
metrics_set3  <- function(x, y, z, i,
                         ReturnNumber,NumberOfReturns,
                         zmin=NA, 
                         threshold = c(2,5), 
                         dz=1, 
                         interval_count=10, 
                         zintervals=c(0, 0.15, 2, 5, 10, 20, 30),
                         pixel_size=1,
                         vox_size=1) {
  
  m_set1    <- metrics_set1(z = z, zmin = zmin, threshold = threshold, dz = dz, interval_count = interval_count, zintervals = zintervals)
  m_rumple  <- metrics_rumple(x = x, y = y, z = z, pixel_size = pixel_size)
  m_vox     <- metrics_voxels(x = x, y = y, z = z, vox_size = vox_size, zmin = zmin)
  m_kde     <- metrics_kde(z = z, zmin = zmin)
  m_echo    <- metrics_echo(z=z, ReturnNumber = ReturnNumber, NumberOfReturns=NumberOfReturns)
  m_HOME    <- metrics_HOME(z = z, i = i, zmin = zmin)
  #m_tex     <- metrics_texture(x = x, y = y, z = z, pixel_size = pixel_size, zmin = zmin)
  
  m <- c(m_set1, m_rumple, m_vox, m_kde, m_echo, m_HOME)
  
  return(m)
  
}
