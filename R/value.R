#' Value noise generator
#'
#' Value noise is a simpler version of cubic noise that uses linear
#' interpolation between neighboring grid points. This creates a more distinct
#' smooth checkerboard pattern than cubic noise, where interpolation takes all
#' the surrounding grid points into accout.
#'
#' @inheritParams noise_perlin
#'
#' @return If `length(dim) == 2` a matrix, if `length(dim) == 3` a 3-dimensional
#' array.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_value(c(100, 100))
#'
#' image(noise, col = grey.colors(256, 0, 1))
#'
noise_value <- function(dim, frequency = 0.01, interpolator = 'quintic',
                   fractal = 'fbm', octaves = 3, lacunarity = 2, gain = 0.5,
                   pertubation = 'none', pertubation_amplitude = 1) {
  interpolator <- match.arg(interpolator, interpolators)
  interpolator <- match(interpolator, interpolators) - 1
  fractal <- match.arg(fractal, fractals)
  fractal <- match(fractal, fractals) - 1
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1

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
