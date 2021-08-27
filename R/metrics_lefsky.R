#' Canopy volume classes
#' 
#' Canopy volume classes based on Lefsky et al 1999 (see references), modified. A voxel rerprenetation of a forest stand
#' is divided into four classes including: open gap space, closed gap space, euphotic zone, and oligophotic zone. 
#' This function is meant to be used within metrics_voxels.
#' 
#' @param x,y,z  X, Y, Z coordinate of the voxels
#' @param n Point count inside each voxel. Used to distinguish filled and empty voxels.
#' @return Percentage of voxels in each class
#' @references 
#' Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A., & Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote Sensing of Environment, 70(3), 339-361. doi:10.1016/S0034-4257(99)00052-8
#' 


metrics_lefsky <- function(x, y, z, n) {
  
  dvox <-  data.table::data.table(X=x, Y=y, Z=z, n=n)
  
  dvox_top <- dvox %>% 
    dplyr::filter(!is.na(n)) %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(Zmax = max(Z), .groups = "keep")
  
  
  vox_stats <- dvox %>% 
    dplyr::left_join(dvox_top, by = c("X", "Y")) %>%
    dplyr::mutate(class = 
                    dplyr::case_when(
               Z > Zmax ~ "OpenGapSpace",                  #empty voxels above canopy
               Z < Zmax & is.na(n) ~ "ClosedGapSpace",     #empty voxels below canopy
               Z >= 0.65 * Zmax ~ "Euphotic",              #filled voxels in the top 0.65 portion of the canopy
               TRUE ~ "Oligophotic"                        #filled voxels in the lower section of the canopy
             )) %>% 
    dplyr::group_by(class) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = n/sum(n) * 100) %>%
    dplyr::select(-n)
  
  
  #ensure all classes are reported, NA if empty:
  all_classes <- c("OpenGapSpace", "ClosedGapSpace", "Euphotic", "Oligophotic")
  
  vox_stats <- dplyr::tibble(class = all_classes) %>%
    dplyr::left_join(vox_stats, by = "class")
  
  out <- vox_stats %>% tidyr::pivot_wider(names_from = class, values_from = perc) %>% #, names_glue = "{.value}_{class}") %>%
    as.list()
  
  
  return(out)
}


