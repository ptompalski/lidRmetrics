

#' Number of points by return number
#' 
#' Calculates the number of points by each return number.
#' 
#' 
#' @param ReturnNumber return number
#' @param KeepReturns numeric. Return numbers to include. 
#' Allows to either limit the output to particular return numbers of interest (e.g. c(1, 3)), 
#' or extend the output to always include the same return numbers (useful when processing multiple datasets with 
#' different return numbers). See examples.
#' Default NULL - all return numbers are included. 
#' @param z Z coordinate of the point cloud. Required only if \code{zmin} is used. 
#' @param zmin Minimum height. If set, heights below are ignored in calculations.
#' @return number of points by each return number
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_echo2(ReturnNumber=ReturnNumber))
#' 
#' #indlude only Returns 1 and 3
#' m2 <- pixel_metrics(las, ~metrics_echo2(ReturnNumber=ReturnNumber, KeepReturns=c(1,3)), res = 20)
#' 
#' #include returns 1-5. In this example the highest return number is 4 but the output will still include counts for return number 5 (will be NA)
#' m3 <- pixel_metrics(las, ~metrics_echo2(ReturnNumber=ReturnNumber, MaxReturnNumber=1:5, z=Z, zmin=2), res = 20)



metrics_echo2 <- function(ReturnNumber, KeepReturns=NULL, z=NULL, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  if(!is.null(KeepReturns)) {
    assert_all_are_positive(KeepReturns)
    assert_all_are_in_closed_range(KeepReturns, 1, 10) #allowed values of returns. Protects against users entering crazy numbers
  }
  
  #check z and zmin
  if (!is.na(zmin) & is.null(z)) {warning("Both z and zmin parameters are required to apply zmin filter. zmin threshold not applied.")}
  
  #filter z values based on zmin (if provided)
  if (!is.na(zmin) & !is.null(z))  {
    zfilter <- z>zmin
    ReturnNumber <- ReturnNumber[zfilter]
  }
  
  #main part of the function - calculate point counts by ReturnNumber
  PointCounts <- as.data.frame(table(ReturnNumber))
  PointCounts$ReturnNumber <- as.numeric(PointCounts$ReturnNumber)
  
  #if MaxReturnnumber is provided, then limit/extend the output to match it
  if(!is.null(KeepReturns)) {
    # if KeepReturns is not null
    PointCounts_max <- data.frame(ReturnNumber=KeepReturns)
    
    #merge
    PointCounts <- merge(PointCounts, PointCounts_max, by="ReturnNumber", all.y = T)
  }
  
  PointCounts$ReturnNumber <- paste0("return_",PointCounts$ReturnNumber,"_count")
  
  #format output
  PointCounts.list <- as.list(PointCounts$Freq)
  names(PointCounts.list) <- PointCounts$ReturnNumber
  
  return(PointCounts.list)
  
}


