#' Kernel density estimation (KDE) metrics
#' 
#' Kernel density estimation applied to the distribution of point cloud elevation (Z). KDE allows to 
#' create a probability density function (using a Guassian kernel). The density function is then used to detect
#' peaks (function maxima). Based on similar metric available in Fusion (see references), with significant differences
#' in the list of output statistics as well as the default bandwidth used when estimating kernel density.
#' 
#' @param z Z coordinate of the point cloud
#' @param bw the smoothing bandwidth of the \code{stats::density} function. Note that the default value (\code{bw=2}) does not 
#' correspond to the default \code{bw} parameter in \code{stats::density}.  
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @param npeaks Total number of recorded peaks. If the number of detected peaks is larger 
#' than \code{npeaks}, only the peaks with highest density value are kept.
#' @param ... Other parameters of the \code{stats::density} function
#' @return Number of peaks, elevation, and density value of each peak, distance (height difference) between peaks
#' 
#' @references McGaughey, R.J., 2021. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. http://forsys.cfr.washington.edu/software/fusion/FUSION_manual.pdf
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_kde(z = Z))
#' 
#' m2 <- grid_metrics(las, ~metrics_kde(z = Z), res = 20)


#' @export
metrics_kde <- function(z, bw=2, zmin=NA, npeaks=4, ...) {
  
  calc_diff <- ifelse(npeaks >= 2, T, F)
  
  # if(npeaks >= 2) {
  #   calc_diff <- T
  #   }
  # 
  
  #initialize output variables, assign NA
  peaks_count <- NA_integer_
  
  varnames_elev  <- paste0("kde_peak",1:npeaks,"_elev")
  varnames_value <- paste0("kde_peak",1:npeaks,"_value")
  if(calc_diff) varnames_diff  <- paste0("kde_peak",1:(npeaks-1),"_diff")
  
  
  for (i in 1:npeaks) {
    assign(varnames_elev[i], NA_real_)
    assign(varnames_value[i], NA_real_)
  }
  if(calc_diff) {
    for (i in 1:(npeaks-1)) {
      assign(varnames_diff[i], NA_real_)
    }
  }
  
  
  #filter z if zmin
  if (!is.na(zmin)) z <- z[z>zmin]
  
  
  if (length(z) > 2) {
    
    d <- stats::density(z, bw=bw, ...)
    
    peaks <- lidRmetrics:::get_peaks(d = d)  
    
    # peaks <- peaks[order(peaks$x, decreasing = T),]
    
    peaks_count <- nrow(peaks)
    
    
    # if more peaks are detected than npeaks then
    # filter peaks based on their density value (aka "strengh")
    # and keep the strongest ones
    
    if (peaks_count > npeaks) {
      
      peaks <- peaks[order(peaks$y, decreasing = T),] #sort by density value
      peaks <- peaks[1:npeaks,]
      
    }
    
    #assign peak location (x) and value (y) to each of the initialized vars, 
    #but only up to peaks count
    #compare npeaks with peaks_count
    #use whichever is smaller
    
    calc_reported_peaks <- min(peaks_count, npeaks)
    
    peaks <- peaks[order(peaks$x, decreasing = T),] #sort by elevation to report peaks from top to bottom
    
    for (j in 1:calc_reported_peaks) {
      assign(varnames_elev[j], peaks$x[j])
      assign(varnames_value[j], peaks$y[j])
    }
    
    
    if(calc_diff & calc_reported_peaks >= 2) {
      # calculate distance between peaks
      peaks_diff <- abs(diff(peaks$x))
      
      for (j in 1:length(peaks_diff)) {
        assign(varnames_diff[j], peaks_diff[j])
      }
    }
    
    
  }
  
  out1 <- out2 <- list()
  
  for (i in 1:npeaks) {
    out1[[i]] <- get(varnames_elev[i])
    out2[[i]] <- get(varnames_value[i])
  }
  
  names(out1) <- varnames_elev
  names(out2) <- varnames_value
  
  out <- list(kde_peaks_count = peaks_count)
  
  out <- c(out, out1, out2)
  
  if(calc_diff) {
    out3 <- list()
    
    for (i in 1:(npeaks-1)) {
      out3[[i]] <- get(varnames_diff[i])
    }
    
    names(out3) <- varnames_diff
    
    out <- c(out, out3)
  }
  
  return(out)
}





get_peaks <- function(d) {
  
  i <- which(diff(sign(diff(d$y))) < 0) + 1 #https://stackoverflow.com/questions/58785930/r-find-maximum-of-density-plot
  
  data.frame(x = d$x[i], y = d$y[i])
  
}

