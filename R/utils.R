
#slightly modified voxel generation function from lidR
#always returning full set of voxels: with and without points
#this is needed to calculate the FR metric
create_voxels <- function(x, y, z, vox_size=1, zmin = NA) {
  
  D <-  data.table::data.table(
    X = as.double(lidR:::round_any(x, vox_size)),
    Y = as.double(lidR:::round_any(y, vox_size)),
    Z = as.double(lidR:::round_any(z, vox_size))
  ) 
  
  if (!is.na(zmin)) {
    D <- D %>% filter(Z >= zmin)
  }
  
  V <- D %>%
    dplyr::group_by(X, Y, Z) %>%
    dplyr::count()
  
  data.table::setDT(V)
  data.table::setkey(V, X, Y, Z)
  
  xrange <- range(V$X)
  yrange <- range(V$Y)
  zrange <- range(V$Z)
  
  V_all <- expand.grid(
    X = seq(xrange[1], xrange[2], by = vox_size),
    Y = seq(yrange[1], yrange[2], by = vox_size),
    Z = seq(zrange[1], zrange[2], by = vox_size)
  )
  
  data.table::setDT(V_all)
  data.table::setkey(V_all, X, Y, Z)
  
  V <- merge(V, V_all, all = TRUE)
  
  return(V)
  
}


### vertical rumple and other histogram variability metrics ####
# to be used with metrics_voxels()

metrics_voxstructure <- function(z, vox_size) {
  
  # define breaks
  brks <- seq(from=min(z), to=max(z), by=vox_size)
  
  if (length(brks) <= 1) {
    output <- list(
      v_vert_rumple =  NA_real_ , 
      v_freq_sd= NA_real_ ,  
      v_freq_cv= NA_real_ 
    )
  } else {
    
    # calculate frequencies
    v_hist <- hist(z, breaks=brks,  plot=F)
    
    # normalize frequencies (0,1)
    freq <- v_hist$counts
    freq <- (freq-min(freq))/(max(freq)-min(freq))
    
    #vertical rumple 
    # flength <- sum(sqrt(abs(lag(freq) - freq)^2 + vox_size^2), na.rm=T) 
    euclidean <- function(a, b) sqrt(sum((a - b)^2))
    
    flength = euclidean(freq, brks[1:length(freq)])
    fheight <- length(freq)*vox_size
    frumple <- flength/fheight
    
    #other metrics
    fsd <- sd(freq)
    fcv <- fsd / mean(freq) * 100
    
    output <- list(
      
      vzrumple = if(is.null(frumple)) NA_real_ else frumple , 
      vzsd= if(is.null(fsd)) NA_real_ else fsd  , 
      vzcv= if(is.null(fcv)) NA_real_ else fcv   
    )
    
  }
  
  return(output)
}
