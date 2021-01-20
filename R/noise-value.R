#' Value noise generator
#'
#' Value noise is a simpler version of cubic noise that uses linear
#' interpolation between neighboring grid points. This creates a more distinct
#' smooth checkerboard pattern than cubic noise, where interpolation takes all
#' the surrounding grid points into accout.
#'
#' @inheritParams noise_perlin
#'
#' @return For `noise_value()` a matrix if `length(dim) == 2` or an array if
#' `length(dim) == 3`. For `gen_value()` a numeric vector matching the length of
#' the input.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_value(c(100, 100))
#'
#' plot(as.raster(normalise(noise)))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_value(grid$x, grid$y)
#' plot(grid, noise)
#'
noise_value <- function(dim, frequency = 0.01, interpolator = 'quintic',
                   fractal = 'fbm', octaves = 3, lacunarity = 2, gain = 0.5,
                   pertubation = 'none', pertubation_amplitude = 1) {
  interpolator <- match.arg(interpolator, interpolators)
  interpolator <- match(interpolator, interpolators) - 1L
  fractal <- match.arg(fractal, fractals)
  fractal <- match(fractal, fractals) - 1L
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1L

  if (length(dim) == 2) {
    noise <- value_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, interp = interpolator, fractal = fractal,
                        octaves = octaves, lacunarity = lacunarity, gain = gain,
                        pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- value_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, interp = interpolator, fractal = fractal,
                        octaves = octaves, lacunarity = lacunarity, gain = gain,
                        pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('Value noise only supports two or three dimensions', call. = FALSE)
  }
  noise
}

#' @rdname noise_value
#' @param x,y,z Coordinates to get noise value from
#' @export
gen_value <- function(x, y = NULL, z = NULL, frequency = 1, seed = NULL,
                      interpolator = 'quintic', ...) {
  dims <- check_dims(x, y, z)
  interpolator <- match.arg(interpolator, interpolators)
  interpolator <- match(interpolator, interpolators) - 1
  if (is.null(seed)) seed <- random_seed()
  if (is.null(z)) {
    gen_value2d_c(dims$x, dims$y, frequency, seed, interpolator)
  } else {
    gen_value3d_c(dims$x, dims$y, dims$z, frequency, seed, interpolator)
  }
}
