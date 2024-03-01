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
  
  # if (length(z)>2) {
    
    n <- length(z)
    zmax <- max(z, na.rm = T)
    zminimum <- min(z, na.rm = T) #this is to confirm if any threshold was applied
    zmean <- mean(z, na.rm = T)
    zvar <- stats::var(z, na.rm = T)
    zsd <- stats::sd(z, na.rm = T)
    zcv <- zsd / zmean * 100
    zskew <- (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2)
    zkurt <- n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2)
    
    return(list(
      n=n,
      zmax=zmax,
      zmin = zminimum,
      zmean = zmean, 
      zvar = zvar,
      zsd = zsd, 
      zcv = zcv,
      zskew = zskew,
      zkurt = zkurt
    )
    )
  # }
}

#' @rdname metrics_basic
#' @export
.metrics_basic = ~metrics_basic(Z)

