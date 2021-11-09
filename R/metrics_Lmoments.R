#' L-moments and L-moment ratios
#' 
#' Calculates L-moments and L-moment ratios of point cloud heihts.
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return L-moments and L-moment ratios
#' @export

metrics_Lmoments <- function(z, zmin=NA) {
  # Lmoments - code from Murray Woods. Modified.
  
  if (!requireNamespace("Lmoments", quietly = TRUE)) {
    stop("Package \"Lmoments\" needed for this function to work.",
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
