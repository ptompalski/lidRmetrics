#' L-moments and L-moment ratios
#' 
#' Calculates L-moments and L-moment ratios of point cloud heihts.
#' 
#' @inheritParams metrics_basic
#' @return A list. L-moments and L-moment ratios
#' \itemize{
#' \item L-moments: \code{L1}, \code{L2}, \code{L3}, \code{L4}
#' \item L-moment ratios: \code{Lskew} (\code{L3}/\code{L2}), \code{Lkurt} (\code{L4}/\code{L2}), and \code{Lcoefvar} (\code{L2}/\code{L1})
#' }
#' 
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_Lmoments(z = Z))
#' 
#' m2 <- pixel_metrics(las, ~metrics_Lmoments(z = Z, zmin = 2), res = 20)
#' 
#' @export

metrics_Lmoments <- function(z, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  # Lmoments - code from Murray Woods. Modified.
  
  if (!requireNamespace("Lmoments", quietly = TRUE)) {
    stop("Package \"Lmoments\" is required to run metrics_Lmoments().",
         call. = FALSE)
  }
  
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  lmom_temp <- list(L1 = as.numeric(NA), L2 = as.numeric(NA), L3 = as.numeric(NA), L4 = as.numeric(NA),
                    Lskew = as.numeric(NA), Lkurt = as.numeric(NA), Lcoefvar = as.numeric(NA))
  
  if (length(z) >= 1) {
    
    lmom <- as.numeric(Lmoments::Lmoments(z))
    
    # an error occurs in some cases and Lmoments are not calcualted or only some are calculated
    # Code below is to check the output of the Lmoments and store it only if 4 values are present
    
    if(length(lmom)==4) {
      names(lmom) <- paste0("L",1:4)
      
      #calculate Lmoments ratios:
      Lskew = lmom[3] / lmom[2]
      Lkurt = lmom[4] / lmom[2]
      Lcoefvar = lmom[2] / lmom[1]
      
      lmom <- as.list(lmom)
      
      lmom[["Lskew"]] <- as.numeric(Lskew)
      lmom[["Lkurt"]] <- as.numeric(Lkurt)
      lmom[["Lcoefvar"]] <- as.numeric(Lcoefvar)
      
    } else {
      lmom <- lmom_temp
    }
  } else {
    lmom <- lmom_temp
  }
  return(lmom)
}


#' @rdname metrics_Lmoments
#' @export
.metrics_Lmoments = ~metrics_Lmoments(Z)
