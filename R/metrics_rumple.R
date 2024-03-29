
#' Calculate rumple index
#' 
#' A wrapper of the \code{lidR::rumple_index} function that allows to calculate rumple index without the need for CHM, and 
#' can be used directly in the e.g. \code{pixel_metrics} function. The function combines the two required steps, 
#' i.e. creating a surface model, and calculating rumple index, into one.
#' Top surface is created using highest points within each pixel.
#' 
#' 
#' @inheritParams metrics_basic
#' @param x,y,z  X, Y, Z coordinates of a point cloud
#' @param pixel_size pixel size
#' @return Same as \code{lidR::rumple_index} - the calculated rumple index
#' @export
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_rumple(x = X, y = Y, z = Z, pixel_size = 1))
#' 
#' m2 <- pixel_metrics(las, ~metrics_rumple(x = X, y = Y, z = Z, pixel_size = 1), res = 20)


metrics_rumple <- function(x, y, z, pixel_size, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  assert_all_are_positive(pixel_size)
  
  if (!is.na(zmin)) {
    filt<- z>zmin
    x <- x[filt]
    y <- y[filt]
    z <- z[filt]
  }
  
  r <- NA_real_
  
  if (length(z) > 2) {
    
    D <-  data.table::data.table(X=x, Y=y, Z=z)
    
    # if (!is.na(zmin)) D <- D[Z > zmin]#z <- z[z>zmin]
    
    if(nrow(D)>0) {
      
      D <- LAS(D, header = rlas::header_create(D), check=F)
      
      D <- lidR::decimate_points(D, lidR::highest(pixel_size))
      
      r <- lidR::rumple_index(x = D$X, y = D$Y, z = D$Z)
      
    }
    
    
    
  }
  return(list(rumple=r))
}


#' @rdname metrics_rumple
#' @export
.metrics_rumple = ~metrics_rumple(X,Y,Z,1)
