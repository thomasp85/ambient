# ambient 1.0.2

* Fixed an integer overflow in the white noise generator

# ambient 1.0.1

* Added `noise_blue()` for generating blue noise texture based on 
  Void-and-cluster algorithm.
* Move compiled code to cpp11

# ambient 1.0.0

* Added a new tidy interface to noise generation, which will be the new 
  recommended way of using ambient. The old array-based constructors will 
  continue to exist as direct interfaces to FastNoise, but the new interface is
  much more powerful in what you can do. The tidy API consists of
  
  - The `long_grid()` constructor that creates a data frame representation of
    a 1-4D array at user specified grid-points. This class has a range of 
    methods for base generics such as `as.matrix()`, `as.array()`, `as.raster()`,
    as well as for standard dplyr verbs. The row number encodes the position in
    the grid, so that the coordinates can be freely manipulated giving rise to 
    very customisable permutations.
  - All noise functions now has a `gen_*()` version, e.g. `gen_perlin()`, that
    takes coordinates, frequency, and seed and return the noise value.
  - A new range of pattern generators has been added as well, e.g. `gen_waves()`,
    that has the same interface as the new noise generators.
  - A `fracture()` function that takes a noise/pattern generator and a fractal
    function to create a fractal version of the generator. Further, `fbm()`, 
    `billow()`, and `ridges()` are provided as equivalent fractal functions to
    the ones provided by FastNoise, and a new `clamped()` fractal function has
    been added.
  - A `trans_affine()` function has been added to make linear transformations of
    the coordinates, along with helpers to build up the transformation matrix.
  - A few modify-helpers has been added, e.g. `blend()` to help with noise level
    manipulation.
  - A `curl_noise()` function that takes a generator and creates curl noise from
    it has been added.
  - A `gradient_noise()` in the same vein as `curl_noise()` has been added for
    calculating the gradient of scalar noise fields
    
* A pkgdown site has been created at https://ambient.data-imaginist.com
