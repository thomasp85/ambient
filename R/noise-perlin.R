#' Perlin noise generator
#'
#' This function generates either 2 or 3 dimensional perlin noise, with optional
#' pertubation and fractality. Perlin noise is one of the most well known
#' gradient noise algorithms and have been used extensively as the basis for
#' generating landscapes and textures, as well as within generative art.
#' The algorithm was developed by Ken Perlin in 1983.
#'
#' @param dim The dimensions (height, width, (and depth)) of the noise to be
#' generated. The length determines the dimensionality of the noise.
#' @param frequency Determines the granularity of the features in the noise.
#' @param interpolator How should values between sampled points be calculated?
#' Either `'linear'`, `'hermite'`, or `'quintic'` (default), ranging from lowest
#' to highest quality.
#' @param fractal The fractal type to use. Either `'none'`, `'fbm'` (default),
#' `'billow'`, or `'rigid-multi'`. It is suggested that you experiment with the
#' different types to get a feel for how they behaves.
#' @param octaves The number of noise layers used to create the fractal noise.
#' Ignored if `fractal = 'none'`. Defaults to `3`.
#' @param lacunarity The frequency multiplier between successive noise layers
#' when building fractal noise. Ignored if `fractal = 'none'`. Defaults to `2`.
#' @param gain The relative strength between successive noise layers when
#' building fractal noise. Ignored if `fractal = 'none'`. Defaults to `0.5`.
#' @param pertubation The pertubation to use. Either `'none'` (default),
#' `'normal'`, or `'fractal'`. Defines the displacement (warping) of the noise,
#' with `'normal'` giving a smooth warping and `'fractal'` giving a more eratic
#' warping.
#' @param pertubation_amplitude The maximal pertubation distance from the
#' origin. Ignored if `pertubation = 'none'`. Defaults to `1`.
#'
#' @return If `length(dim) == 2` a matrix, if `length(dim) == 3` a 3-dimensional
#' array.
#'
#' @references
#' Perlin, Ken (1985). *An Image Synthesizer*. SIGGRAPH Comput. Graph. 19
#' (0097-8930): 287–296. doi:10.1145/325165.325247.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_perlin(c(100, 100))
#'
#' image(noise, col = grey.colors(256, 0, 1))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_perlin(grid$x, grid$y)
#' plot(as.raster(grid, normalise(noise)))
#'
noise_perlin <- function(dim, frequency = 0.01, interpolator = 'quintic',
                   fractal = 'fbm', octaves = 3, lacunarity = 2, gain = 0.5,
                   pertubation = 'none', pertubation_amplitude = 1) {
  interpolator <- match.arg(interpolator, interpolators)
  interpolator <- match(interpolator, interpolators) - 1
  fractal <- match.arg(fractal, fractals)
  fractal <- match(fractal, fractals) - 1
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1

  if (length(dim) == 2) {
    noise <- perlin_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                freq = frequency, interp = interpolator, fractal = fractal,
                octaves = octaves, lacunarity = lacunarity, gain = gain,
                pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- perlin_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                freq = frequency, interp = interpolator, fractal = fractal,
                octaves = octaves, lacunarity = lacunarity, gain = gain,
                pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('Perlin noise only supports two or three dimensions', call. = FALSE)
  }
  noise
}

#' @rdname noise_perlin
#' @param x,y,z Coordinates to get noise value from
#' @param seed The seed to use for the noise. If `NULL` a random seed will be
#' used
#' @export
gen_perlin <- function(x, y = NULL, z = NULL, frequency = 1, seed = NULL,
                       interpolator = 'quintic') {
  dims <- check_dims(x, y, z)
  interpolator <- match.arg(interpolator, interpolators)
  interpolator <- match(interpolator, interpolators) - 1
  if (is.null(seed)) seed <- random_seed()
  if (is.null(z)) {
    gen_perlin2d_c(dims$x, dims$y, frequency, seed, interpolator)
  } else {
    gen_perlin3d_c(dims$x, dims$y, dims$z, frequency, seed, interpolator)
  }
}
