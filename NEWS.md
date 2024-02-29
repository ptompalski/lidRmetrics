# lidRmetrics 0.1.1

## Changes in version 0.1.1

### New features

1.  New function `metrics_echo()`
2.  New function `metrics_echo2()`

### Enhancements

1. Variance (`var`) is now calculated by `metrics_basic()`. This may potentially break some existing processing routines.
2. Added several checks for user-provided input. Error messages should now be a bit more informative.

### Fixes

- Examples are now provided for all functions.
- All examples were updated - `lidR::grid_metrics()` function replaced with `lidR::pixel_metrics()`
