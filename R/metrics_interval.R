#' Interval metrics
#'
#' Compute the percentage of points falling within user-defined height
#' intervals. 
#'
#' @inheritParams metrics_basic
#' @param z A numeric vector of heights (e.g., `Z` from a LAS object).
#' @param zintervals Numeric, strictly increasing. Interior breakpoints for
#'   height intervals (e.g., \code{c(0, 0.15, 2, 5, 10, 20, 30)}).
#'   The full set of breaks used is \code{c(-Inf, zintervals, Inf)}, producing:
#'   \itemize{
#'     \item a "below" bin: \code{(-Inf, zintervals[1])}
#'     \item interior bins: \code{[zintervals[i], zintervals[i+1])}
#'     \item an "above" bin: \code{[zintervals[length(zintervals)], Inf)}
#'   }
#'   With \code{right = FALSE} (used internally), each bin includes its left
#'   boundary and excludes its right boundary, except the open-ended top bin
#'   which includes all values \eqn{\ge} the last breakpoint.
#' @param zmin Optional numeric scalar. If provided, values are filtered as
#'   \code{z > zmin} before binning (strictly greater).
#'
#' @details
#' Internally, counts are computed with:
#' \preformatted{
#'   breaks <- c(-Inf, zintervals, Inf)
#'   hist(z, breaks = breaks, plot = FALSE, right = FALSE)
#' }
#' Using `right = FALSE` enforces left-closed, right-open bins (`[a,b)`),
#' which guarantees, for example, that `0` is included in `[0, next)` rather
#' than the "below" bin. Proportions are the counts divided by the number of
#' points remaining after optional `zmin` filtering, multiplied by 100.
#'
#' Output names follow:
#' \itemize{
#'   \item \code{pz_below_<first>} for the below bin
#'   \item \code{pz_<a>-<b>} for each interior \code{[a,b)} bin
#'   \item \code{pz_above_<last>} for the above bin
#' }
#'
#' @return A named list of percentages (numeric) for each interval.
#'
#' @examples
#' library(lidR)
#' library(lidRmetrics)
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#'
#' m1 <- cloud_metrics(las, ~metrics_interval(z = Z))
#'
#' m2 <- pixel_metrics(las, ~metrics_interval(z = Z, zintervals = c(0, 5, 10)), res = 20)
metrics_interval <- function(
    z,
    zintervals = c(0, 0.15, 2, 5, 10, 20, 30),
    zmin = NA_real_
) {
  # --- input checks ---
  stopifnot(is.numeric(z), is.numeric(zintervals))
  if (!any(is.na(zmin))) stopifnot(length(zmin) == 1, is.finite(zmin))
  if (any(zintervals < 0)) stop("All 'zintervals' must be non-negative.")
  if (is.unsorted(zintervals, strictly = TRUE)) {
    stop("'zintervals' must be strictly increasing.")
  }
  
  # optional lower cutoff (strictly greater than zmin)
  if (!is.na(zmin)) z <- z[z > zmin]
  
  # handle empty input after filtering
  k <- length(zintervals)
  if (length(z) == 0L) {
    if (k == 1) {
      nm <- c(
        paste0("pz_below_", zintervals[1]),
        paste0("pz_above_", zintervals[1])
      )
    } else {
      nm <- c(
        paste0("pz_below_", zintervals[1]),
        paste0("pz_", zintervals[-k], "-", zintervals[-1]),
        paste0("pz_above_", zintervals[k])
      )
    }
    out <- rep(0, length(nm))
    names(out) <- nm
    return(as.list(out))
  }
  
  # --- define breaks and compute counts ---
  brks <- c(-Inf, zintervals, Inf)
  h <- hist(
    z,
    breaks = brks,
    plot = FALSE,
    right = FALSE,
    include.lowest = FALSE
  )
  
  # proportions (%)
  z_prop <- h$counts / length(z) * 100
  
  # --- build names ---
  if (k == 1) {
    names_vec <- c(
      paste0("pz_below_", zintervals[1]),
      paste0("pz_above_", zintervals[1])
    )
  } else {
    names_vec <- c(
      paste0("pz_below_", zintervals[1]),
      paste0("pz_", zintervals[-k], "-", zintervals[-1]),
      paste0("pz_above_", zintervals[k])
    )
  }
  names(z_prop) <- names_vec
  
  as.list(z_prop)
}

#' @rdname metrics_interval
#' @export
.metrics_interval = ~metrics_interval(Z)


