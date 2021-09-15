#' Canopy texture metrics
#' 
#' Generates a suite of GLCM (Grey-Level Co-Occurence Matrix) metrics of a canopy height model (CHM). CHM is calculated on the fly
#' to allow easy integration with e.g. \code{lidR::grid_metrics} function.
#' 
#' @param x,y,z  X, Y, Z coordinates of a point cloud
#' @param pixel_size Pixel size of the CHM
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @param chm_algorithm Function used to generate the CHM. By default \code{lidR::p2r(na.fill = lidR::knnidw())} is used.
#' @param ... additional parameters passed to \code{ForestTools::glcm_img()} function
#' @return GLCM metrics
#'
#' @details Function first uses the \code{lidR::grid_canopy()} algorithm to create a CHM. 
#'  \code{ForestTools::glcm_img()} function is then used to calculate GLCM statistics (see package manual for details). 
#' This implementation of GLCM does not allow for missing values - after CHM is created, any missing values are converted to 0.
#' 
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_texture(x = X, y = Y, z = Z, pixel_size = 1))
#' 
#' m2 <- grid_metrics(las, ~metrics_texture(x = X, y = Y, z = Z, pixel_size = 1), res = 20)


metrics_texture <- function(x, y, z, pixel_size, zmin=NA, chm_algorithm = NULL, ...) {
  
  tex <- NULL #if glcm_img fails return empty result
  
  #chm algorithm
  if(is.null(chm_algorithm)) {
    chm_algorithm <- lidR::p2r(na.fill = lidR::knnidw())
  }
  
  #construct dt
  D <-  data.table::data.table(X=x, Y=y, Z=z)
  
  #filter if zmin
  if (!is.na(zmin)) {
    D <- dplyr::filter(D, Z > zmin)
  }
  
  #check if enough data to proceed
  if (nrow(D) >= 1) {
    
    #construct a LAS object
    D <- lidR::LAS(D, header = rlas::header_create(D), check=F)
    
    #create chm
    chm <- lidR::grid_canopy(D, res = pixel_size, algorithm = chm_algorithm)
    
    # glcm_img does not accept NA values.
    # NAs are replaced with 0
    chm[is.na(chm)] <- 0
    
    #calculating texture
    try({
      tex <- ForestTools::glcm_img(chm, ...)
    }, silent = T)
    
  }
  
  return(as.list(tex))
}



