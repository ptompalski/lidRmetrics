#' Metrics based on echo types
#' 
#' Based on the return number and number of returns, point cloud returns are classified into different echo types. Two different
#' classification routines are applied. Under the first classification routine returns are classified into First, Intermediate, and Last. 
#' Under the second classification routine returns are classified into Single or Multiple.
#' 
#' @param ReturnNumber return number
#' @param NumberOfReturns number of returns
#' @param z Z coordinate of the point cloud. Required only if \code{zmin} is used. 
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return Percentage of returns by each echo type
#' 
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_echo(z = Z, ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns))
#' 
#' m2 <- grid_metrics(las, ~metrics_echo(z = Z, ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns), res = 20)


metrics_echo <- function(ReturnNumber, NumberOfReturns, z=NULL, zmin=NA) {
  
  if (!is.na(zmin) & is.null(z)) {warning("Both z and zmin parameters are required to apply zmin filter. zmin threshold not applied.")}
  
  if (!is.na(zmin) & !is.null(z))  {
    zfilter <- z>zmin
    ReturnNumber <- ReturnNumber[zfilter]
    NumberOfReturns <- NumberOfReturns[zfilter]
    }
  
  
  filter <- ReturnNumber > 0 & NumberOfReturns > 0 & ReturnNumber <= NumberOfReturns
  ReturnNumber <- ReturnNumber[filter]
  NumberOfReturns <- NumberOfReturns[filter]
  
  n = length(ReturnNumber)
  
  first = sum(ReturnNumber == 1)/n * 100
  intermediate = sum(ReturnNumber > 1 & ReturnNumber < NumberOfReturns)/n * 100
  last = sum(ReturnNumber == NumberOfReturns & ReturnNumber > 1)/n * 100
  
  
  #single/multiple
  single <- sum(NumberOfReturns==1) / n * 100
  multiple <- sum(NumberOfReturns > 1) / n * 100
  
  
  out <- list(pfirst = first, pintermidiate = intermediate, plast = last, psingle=single, pmultiple=multiple)
  
  return(out)
}

