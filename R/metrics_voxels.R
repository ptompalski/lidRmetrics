#' Voxel-based metrics
#' 
#' A set of metrics calculated in a voxel space, designed to be used within the \code{grid_metrics} or \code{cloud_metrics} function from the \code{lidR}
#' package. 
#' For convenience, a point cloud is converted to a voxel space on the fly, without the need of using additional processing steps. 
#' Note, that because of the additional computation required to convert a point cloud to voxels, calculating voxel-based metrics
#' is markedly slower than other metrics_* functions.
#' 
#' Calculated metrics include:
#' \itemize{
#' \item{\code{vn} count of filled voxels}
#' \item{\code{FRall} and \code{FRcanopy}: filled ratio. For \code{FRall} a ratio between the number of filled voxels and all voxels located in 
#' the maximum extent of the point cloud. For \code{FRcanopy} empty voxels above the canopy are excluded}
#' \item{metrics describing the vertical distribution of filled voxels: standard deviation (\code{vzsd}), 
#' coeficient of variation (\code{vzcv}), and vertical rumple (\code{vzrumple}).}
#' \item{Canopy volume classes based on Lefsky et al 1999 (see references), modified. A voxel representation of a forest stand
#' is divided into four classes including: open gap space, closed gap space, euphotic zone, and oligophotic zone.}
#' 
#' }
#' 
#'  
#' @param x,y,z  X, Y, Z coordinates of a point cloud to be converted into voxels
#' @param vox_size voxel size
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @references 
#' Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A., & Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote Sensing of Environment, 70(3), 339-361. doi:10.1016/S0034-4257(99)00052-8
#' @export
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_voxels(x = X, y = Y, z = Z, vox_size = 1))
#' 
#' m2 <- grid_metrics(las, ~metrics_voxels(x = X, y = Y, z = Z, vox_size = 1), res = 40)



metrics_voxels <- function(x, y, z, vox_size=1, zmin = NA) {
  
  vn <- vFRall <- vFRcanopy <- NA_real_
  
  if (length(z) > 2) {
    
    vox <- lidRmetrics:::create_voxels(x = x, y = y, z = z, vox_size = vox_size, zmin = zmin)
    
    vox_filled <- vox[!is.na(vox$n),]
    
    vn <- nrow(vox_filled)
    
    # FR (filling ratio, number of voxels with data to the number of all possible voxels)
    vFRall <- nrow(vox_filled) / nrow(vox) * 100
    
    #FR under canopy (limit the filling ratio calculations with the top of the canopy)
    vox2 <- vox %>%
      dplyr::filter(!is.na(n)) %>%         #remove empty voxels
      dplyr::group_by(X, Y) %>%             #for each X and Y...
      dplyr::summarise(zmax = max(Z), .groups = "keep") %>%   #...find the highest voxel
      dplyr::right_join(vox, by=c("X", "Y")) %>%            #combine with original voxel data
      dplyr::filter(Z <= zmax)              #remove empty voxels above canopy
    
    vFRcanopy <- nrow(vox2[!is.na(vox2$n),])  / nrow(vox2) * 100
    
    mhist <- lidRmetrics:::metrics_voxstructure(z = vox_filled$Z, vox_size = vox_size)
    
    mlefsky <- lidRmetrics:::metrics_lefsky(x = vox$X, y = vox$Y, z = vox$Z, n=vox$n)
    
  } else {
    
    mhist <- lidRmetrics:::metrics_voxstructure(z = NA) #this is temporary fix
    mlefsky <- lidRmetrics:::metrics_lefsky(x = NA, y = NA, z = NA, n=NA) #this is temporary fix
    
  }
  
  
  output = list(vn = vn,
                vFRall = vFRall,
                vFRcanopy = vFRcanopy)
  
  output <- c(output, mhist, mlefsky)
  
  return(output)
  
  
}













