#' Dispersion metrics
#' 
#' Metrics characterizing the dispersion of point heights 
#' 
#' @inheritParams metrics_basic
#' @param dz Numeric. Layer thickness to use when calculating entropy and VCI.
#' @param zmax Numeric. Maximum elevation for an entropy normalized to zmax.
#' @return A list. Calculated metrics include:
#' \itemize{
#' \item \code{ziqr} interquartile distance
#' \item \code{zMADmean} mean absolute deviation (MAD) around the mean
#' \item \code{zMADmedian} mean absolute deviation (MAD) around the median
#' \item \code{CRR} canopy relief ratio
#' \item \code{zentropy} entropy
#' \item \code{VCI} vertical complexity index. Optional - calculated only if the \code{zmax} parameter is provided.
#' 
#' }

#' @details
#' When calculating \code{zentropy} and \code{VCI}, \code{z} values below 0 are removed.
#' 
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_dispersion(z = Z))
#' 
#' m2 <- pixel_metrics(las, ~metrics_dispersion(z = Z, dz = 2,  zmax = 30), res = 20)


metrics_dispersion <- function(z, dz=1, zmin=NA, zmax=NA) {
  
  #check user inputs
  assert_is_a_number(dz)
  if(!is.na(zmin))  assert_is_a_number(zmin)
  if(!is.na(zmax))  assert_is_positive(zmax)
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  # prepare output 
  out <- list(
    ziqr=NA_real_,
    zMADmean=NA_real_,
    zMADmedian=NA_real_,
    CRR=NA_real_,
    zentropy=NA_real_
  )
  
  if(!is.na(zmax)) {out <- c(out, list(VCI=NA_real_))}
 
  
  if (length(z)!=0) { #check if z is empty
    
    out$ziqr = IQR(z) # Interquartile distance 
    
    # AAD (Average Absolute Deviation):
    # https://en.wikipedia.org/wiki/Average_absolute_deviation
    
    #MAD around the mean
    out$zMADmean = mean(abs(z - mean(z)))
    
    # MAD around the median - Median absolute deviation (median of the absolute deviations from the data's median)
    out$zMADmedian = median(abs(z - median(z)))
    
    # Canopy relief ratio ((mean - min) / (max – min))
    out$CRR = ((mean(z) - min(z)) / (max(z) - min(z)))
    
    z1 <- z[z>0] # for entropy and VCI forcing z to be always above 0, otherwise function do not work
    
    if (length(z1)!=0) { #check if z1 is empty
      out$zentropy = entropy(z1, dz)
      # out$VCI = VCI(z1, zmax = max(z1), by = dz)
    }
    
    if(!is.na(zmax) & length(z1)!=0) { #if zmax then include VCI
      out$VCI = VCI(z1, zmax = zmax, by = dz)
    }
    
  }
  
  return(out)
}

#' @rdname metrics_dispersion
#' @export
.metrics_dispersion = ~metrics_dispersion(Z)