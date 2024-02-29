
# internal function to check user input
# based on https://github.com/r-lidar/lidR/blob/master/R/utils_assertive.R

assert_all_are_non_negative = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x >= 0))
    stop(glue::glue("Values of {x.} are not all positive or null."), call. = FALSE)
}


assert_all_are_positive = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x > 0))
    stop(glue::glue("Values of {x.} are not all positive."), call. = FALSE)
}


assert_all_are_in_closed_range = function(x, a, b)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x >= a) | !all(x <= b))
    stop(glue::glue("Values of {x.} are not all in range [{a}, {b}]."), call. = FALSE)
}

assert_is_a_number = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.numeric(x) | length(x) > 1)
    stop(glue::glue("{x.} is not a number."), call. = FALSE)
}


assert_is_positive = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!(x > 0))
    stop(glue::glue("Value of {x.} is not positive."), call. = FALSE)
}

assert_package_is_installed = function(x)
{
  if (!requireNamespace(x, quietly = TRUE))
    stop(glue::glue("Package '{x}' is required for this function to work."), call. = FALSE) 
}