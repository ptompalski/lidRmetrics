#' LAD metrics
#' 
#' Metrics based on the leaf area density. \code{lidR::LAD()} used to calculate the leaf area density. 
#' 
#' @inheritParams metrics_basic
#' @param dz numeric. The thickness of the layers used (height bin)
#' @param k numeric. is the extinction coefficient
#' @param z0 numeric. The bottom limit of the profile
#' @return A list. \code{lad_min}, \code{lad_max}, \code{lad_mean}, \code{lad_cv}, and \code{lad_sum}.
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_lad(z = Z))
#' 
#' m2 <- pixel_metrics(las, ~metrics_lad(z = Z), res = 20)


metrics_lad <- function(z, zmin=NA, dz = 1, k = 0.5, z0 = 2) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  assert_is_a_number(dz)
  assert_is_a_number(k)
  assert_is_a_number(z0)
  
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  lad_max <- lad_mean <- lad_cv <- lad_min <- lad_sum <- NA_real_
  
  #custom min and max to protect against Inf
  custom_min <- function(x) {if (length(x)>0) min(x, na.rm = T) else NA_real_}
  custom_max <- function(x) {if (length(x)>0) max(x, na.rm = T) else NA_real_}
  
  if(length(z) > 2) {
    
    ladprofile <- lidR::LAD(z, dz = dz, k = k, z0 = z0)
    
    lad_min <- custom_min(ladprofile$lad)
    lad_max <- custom_max(ladprofile$lad)
    lad_mean <- mean(ladprofile$lad, na.rm = T)
    lad_sd <- sd(ladprofile$lad, na.rm = T)
    lad_cv <- lad_sd / lad_mean
    lad_sum <- sum(ladprofile$lad,na.rm = T)

  }
  
  lad_metrics <- list(
    lad_min = lad_min,
    lad_max = lad_max,
    lad_mean = lad_mean,
    lad_cv = lad_cv,
    lad_sum = lad_sum)
  
  return(lad_metrics)
}

#' @rdname metrics_lad
#' @export
.metrics_lad = ~metrics_lad(Z)


