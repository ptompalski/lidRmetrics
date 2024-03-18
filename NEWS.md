# lidRmetrics 0.1.2

### Breaking changes

- changes in `metrics_set2()` and `metrics_set3()`. `metrics_set2()` now consists of all metrics that are calculated based on the `Z`, `ReturnNumber` and `NumberOfReturn` attributes. Metrics that require more time consuming computations in 3D space (`metrics_rumple()`, `metrics_voxels()`) as well as less common metrics are now moved to `metrics_set3()`.

### Enhancements

- added benchmarking results in `readme.md`

### Fixes

- Fixed issue with `metrics_echo2()` that caused function fail when the `KeepReturns` was `NULL` (default value). Function's new default value of `KeepReturns` is set to `c(1,2,3,4)` (#19).
- Fixed issue in `metrics_texture()`. Updated to match the most recent changes in the `ForestTool` package (#20).




# lidRmetrics 0.1.1

## Changes in version 0.1.1

### New features

1.  New function `metrics_echo()`
2.  New function `metrics_echo2()`
3.  A new vignette demonstrating how to create custom metric sets.
4.  General improvements and bug fixes.
5.  Each `metrics_*()` function gained a convenient shortcut version: `.metrics_*` 

### Enhancements

1. Variance (`var`) is now calculated by `metrics_basic()`. This may potentially break some existing processing routines.
2. Added several checks for user-provided input. Error messages should now be a bit more informative.

### Fixes

- Examples are now provided for all functions.
- All examples were updated - `lidR::grid_metrics()` function replaced with `lidR::pixel_metrics()`
- Fixed issue with `metrics_voxels()` failing when the `zmin` parameter was used #3 and #11
- Fixed issue with warnings being generated when using `metrics_basic` #12
- Fixed issue with warnings being generated when using `metrics_dispersion` #13
- VCI calculations were incorrect. VCI now requires zmax parameter to be calculated.