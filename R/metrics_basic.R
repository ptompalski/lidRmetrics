#' Basic metrics
#' 
#' Most common descriptive statistics used to characterize the vertical distribution of points in a point cloud.
#' 
#' @param z Z coordinate of the point cloud (point heights)
#' @param zmin numeric. Minimum \code{z} value. If set, \code{z} values (heights) below are ignored in calculations.
#' @return A set of descriptive statistics including: total number of points, maximum height, minimum height, mean height, 
#' variance of height, standard deviation of height, coefficient of variation of height, skewness and kurtosis of height.
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' #================
#' # CLOUD METRICS
#' #================
#' 
#' m1 <- cloud_metrics(las, ~metrics_basic(z = Z))
#' 
#' #================
#' # PIXEL METRICS
#' #================
#' 
#' m2 <- pixel_metrics(las, ~metrics_basic(z = Z), res = 20)
#' 
#' 
#' #================
#' # PLOT METRICS
#' #================
#' 
#' shpfile <- system.file("extdata", "efi_plot.shp", package="lidR")
#' inventory <- sf::st_read(shpfile, quiet = TRUE)
#' 
#' m3 <- plot_metrics(las, ~metrics_basic(z = Z,  zmin = 2), inventory, radius = 11.28)


metrics_basic <- function(z, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  # prepare output 
  out <- list(
    n=NA_integer_,
    zmax=NA_real_,
    zmin = NA_real_,
    zmean = NA_real_, 
    zvar = NA_real_,
    zsd = NA_real_, 
    zcv = NA_real_,
    zskew = NA_real_,
    zkurt = NA_real_
  )
  
  
  if (length(z)!=0) { #check if z is empty

    out$n <- length(z)
    out$zmax <- max(z)
    out$zmin <- min(z) #this is to confirm if any threshold was applied
    out$zmean <- mean(z)
    out$zvar <- stats::var(z)
    out$zsd <- stats::sd(z)
    out$zcv <- out$zsd / out$zmean * 100
    out$zskew <- (sum((z - out$zmean)^3)/out$n)/(sum((z - out$zmean)^2)/out$n)^(3/2)
    out$zkurt <- out$n * sum((z - out$zmean)^4)/(sum((z - out$zmean)^2)^2)
    
  }

  return(out)

  
}

#' @rdname metrics_basic
#' @export
.metrics_basic = ~metrics_basic(Z)

