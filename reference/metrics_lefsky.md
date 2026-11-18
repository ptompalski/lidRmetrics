# Canopy volume classes

Canopy volume classes based on Lefsky et al 1999 (see references),
modified. A voxel representation of a forest stand is divided into four
classes including: open gap space, closed gap space, euphotic zone, and
oligophotic zone. This function is meant to be used within
metrics_voxels.

## Usage

``` r
metrics_lefsky(x, y, z, n)
```

## Arguments

- x, y, z:

  X, Y, Z coordinate of the voxels

- n:

  Point count inside each voxel. Used to distinguish filled and empty
  voxels.

## Value

Percentage of voxels in each class

## References

Lefsky, M. A., Cohen, W. B., Acker, S. A., Parker, G. G., Spies, T. A.,
& Harding, D. (1999). Lidar Remote Sensing of the Canopy Structure and
Biophysical Properties of Douglas-Fir Western Hemlock Forests. Remote
Sensing of Environment, 70(3), 339-361.
doi:10.1016/S0034-4257(99)00052-8
