#' Canopy density metrics
#' 
#' Cumulative distribution of point heights in interval count. 
#' 
#' @param z Z coordinate of the point cloud
#' @param interval_count Number of intervals used to divide the point height distribution. 
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Cumulative percentage of points in each interval
#' @export


metrics_canopydensity <- function(z, interval_count=10, zmin=NA) { #after Woods et al 2008  https://doi.org/10.5558/tfc84827-6
  
  #check user inputs
  assert_is_a_number(interval_count)
  assert_is_positive(interval_count)
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  
  if (!is.na(zmin)) z <- z[z>zmin]
  
  # prepare output 
  out <- sapply(paste0("zpcum", 1:(interval_count-1)),function(x) NULL)
  out <- lapply(out, function(x) NA_real_)
  
  if (length(z)!=0) { #check if z is empty
    
    zmax <- max(z)
    zminimum <- min(z)
    
    if (zmax <= zminimum) {
      d <- as.list(rep(0, interval_count-1))
    }
    else {
      breaks <- seq(zminimum, zmax, (zmax - zminimum)/interval_count)
      d <- findInterval(z[z > zminimum], breaks)
      d <- lidR:::fast_table(d, interval_count)
      d <- d/sum(d) * 100
      d <- cumsum(d)[1:(interval_count-1)]
      d <- as.list(d)
    }
    names(d) <- paste0("zpcum", 1:(interval_count-1))
    out <- d
  }
  
  return(out)
  
}


#' @rdname metrics_canopydensity
#' @export
.metrics_canopydensity = ~metrics_canopydensity(Z)
