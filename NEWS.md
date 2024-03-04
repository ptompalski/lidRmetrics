# lidRmetrics 0.1.1

## Changes in version 0.1.1

### New features

1.  New function `metrics_echo()`
2.  New function `metrics_echo2()`
3.  A new vignette demonstrating how to create custom metric sets.
4.  General improvements and bug fixes.

### Enhancements

1. Variance (`var`) is now calculated by `metrics_basic()`. This may potentially break some existing processing routines.
2. Added several checks for user-provided input. Error messages should now be a bit more informative.

### Fixes

- Examples are now provided for all functions.
- All examples were updated - `lidR::grid_metrics()` function replaced with `lidR::pixel_metrics()`
- Fixed issue with `metrics_voxels()` failing when the `zmin` parameter was used #3 and #11
- Fixed issue with warnings being generated when using `metrics_basic` #12