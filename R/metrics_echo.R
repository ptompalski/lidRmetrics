#' Number and proportion of points by echo types
#' 
#' Based on the return number and number of returns, point cloud returns are classified into different echo types. Two different
#' classification routines are applied. Under the first classification routine returns are classified into First, Intermediate, and Last. 
#' Under the second classification routine returns are classified into Single or Multiple. Function then calculates point counts and proportions 
#' by each echo type. Ratios of Last to First, Intermediate to First, and Multiple to Single, are also calculated.
#' 
#' @param ReturnNumber return number
#' @param NumberOfReturns number of returns
#' @inheritParams metrics_basic
#' 
#' @return A list. Calculated metrics include:
#' \itemize{
#' \item Number of first (\code{n_first}), intermediate (\code{n_intermediate}), last (\code{n_last}), single (\code{n_single}), and multiple  (\code{n_multiple}) returns
#' \item Proportion of first (\code{p_first}), intermediate (\code{p_intermediate}), last (\code{p_last}), single (\code{p_single}), and multiple  (\code{p_multiple}) returns
#' \item Ratio of Last to First (\code{ratio_last_first}), Intermediate to First (\code{ratio_intermediate_first}), and Multiple to Single (\code{ratio_multiple_single}). 
#' }
#' 
#' 
#' @export
#' 
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#' 
#' m1 <- cloud_metrics(las, ~metrics_echo(ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns))
#' 
#' m2 <- pixel_metrics(las, ~metrics_echo(ReturnNumber=ReturnNumber, NumberOfReturns=NumberOfReturns), res = 20)


metrics_echo <- function(ReturnNumber, NumberOfReturns, z=NULL, zmin=NA) {
  
  #check user inputs
  if(!is.na(zmin))  assert_is_a_number(zmin)
  
  if (!is.na(zmin) & is.null(z)) {warning("Both z and zmin parameters are required to apply zmin filter. zmin threshold not applied.")}
  
  if (!is.na(zmin) & !is.null(z))  {
    
    zfilter <- z>zmin
    ReturnNumber <- ReturnNumber[zfilter]
    NumberOfReturns <- NumberOfReturns[zfilter]
    
  }
  
  # prepare output 
  out <- list(n_first = NA_integer_,
              n_intermediate = NA_integer_,
              n_last = NA_integer_,
              n_single = NA_integer_,
              n_multiple = NA_integer_,
              p_first = NA_real_, 
              p_intermidiate = NA_real_, 
              p_last = NA_real_, 
              p_single = NA_real_, 
              p_multiple = NA_real_,
              ratio_last_first = NA_real_,
              ratio_intermediate_first = NA_real_,
              ratio_multiple_single = NA_real_)
  
  if (length(ReturnNumber)!=0 & length(NumberOfReturns)!=0) { #check if empty
    
    filter <- ReturnNumber > 0 & NumberOfReturns > 0 & ReturnNumber <= NumberOfReturns
    ReturnNumber <- ReturnNumber[filter]
    NumberOfReturns <- NumberOfReturns[filter]
    
    n = length(ReturnNumber)
    
    n_first <- sum(ReturnNumber == 1)
    p_first <- n_first / n * 100
    
    n_intermediate <- sum(ReturnNumber > 1 & ReturnNumber < NumberOfReturns)
    p_intermediate <- n_intermediate / n * 100
    
    n_last <- sum(ReturnNumber == NumberOfReturns & ReturnNumber > 1)
    p_last <- n_last / n * 100
    
    
    #single/multiple
    n_single <- sum(NumberOfReturns==1)
    p_single <- n_single / n * 100
    
    n_multiple <- sum(NumberOfReturns > 1)
    p_multiple <- n_multiple / n * 100
    
    #ratios
    if(n_first>0)  {ratio_last_first = n_last / n_first } else ratio_last_first=NA_real_
    if(n_first>0) {ratio_intermediate_first = n_intermediate / n_first} else ratio_intermediate_first = NA_real_
    if(n_single>0) {ratio_multiple_single = n_multiple / n_single} else ratio_multiple_single=NA_real_
    
    out <- list(n_first = n_first,
                n_intermediate = n_intermediate,
                n_last = n_last,
                n_single = n_single,
                n_multiple = n_multiple,
                p_first = p_first, 
                p_intermidiate = p_intermediate, 
                p_last = p_last, 
                p_single = p_single, 
                p_multiple = p_multiple,
                ratio_last_first = ratio_last_first,
                ratio_intermediate_first = ratio_intermediate_first,
                ratio_multiple_single = ratio_multiple_single)
  }
  
  return(out)
}

#' @rdname metrics_echo
#' @export
.metrics_echo = ~metrics_echo(ReturnNumber, NumberOfReturns)