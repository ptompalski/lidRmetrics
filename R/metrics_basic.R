#divide stdmetrics into smaller components

#standard stats 
#percentiles
#cumulative 
#proportion above x - cover metrics
#move entropy and VCI to metrics_dispersion?


#' Basic metrics
#' 
#' Most common descriptive statistics used to characterize the vertical distribution of a point cloud.
#' 
#' @param z Z coordinate of the point cloud
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return A set of descriptive statistics including: total number of points, maximum height, minimum height, mean height, 
#' standard deviation of height, coefficient of variation of height, skewness and kurtosis of height
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_basic(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_basic(z = Z), res = 40)

metrics_basic <- function(z, zmin=NA) {
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  n <- length(z)
  zmax <- max(z)
  zminimum <- min(z) #this is to confirm if any threshold was applied
  zmean <- mean(z)
  zsd <- stats::sd(z)
  zcv <- zsd / zmean * 100
  zskew <- (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2)
  zkurt <- n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2)
  
  return(list(
    n=n,
    zmax=zmax,
    zmin = zminimum,
    zmean = zmean, 
    zsd = zsd, 
    zcv = zcv,
    zskew = zskew,
    zkurt = zkurt
  )
  )
  
}

