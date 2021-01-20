#' Cubic noise generator
#'
#' Cubic noise is a pretty simple alternative to perlin and simplex noise. In
#' essence it takes a low resolution white noise and scales it up using cubic
#' interpolation. This approach means that while cubic noise is smooth, it is
#' much more random than perlin and simplex noise.
#'
#' @inheritParams noise_perlin
#'
#' @return For `noise_cubic()` a matrix if `length(dim) == 2` or an array if
#' `length(dim) == 3`. For `gen_cubic()` a numeric vector matching the length of
#' the input.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_cubic(c(100, 100))
#'
#' plot(as.raster(normalise(noise)))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_cubic(grid$x, grid$y)
#' plot(grid, noise)
#'
noise_cubic <- function(dim, frequency = 0.01, fractal = 'fbm', octaves = 3,
                        lacunarity = 2, gain = 0.5, pertubation = 'none',
                        pertubation_amplitude = 1) {
  fractal <- match.arg(fractal, fractals)
  fractal <- match(fractal, fractals) - 1L
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1L

  if (length(dim) == 2) {
    noise <- cubic_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, fractal = fractal,
                        octaves = octaves, lacunarity = lacunarity, gain = gain,
                        pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- cubic_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, fractal = fractal,
                        octaves = octaves, lacunarity = lacunarity, gain = gain,
                        pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('Cubic noise only supports two or three dimensions', call. = FALSE)
  }
  noise
}

#' @rdname noise_cubic
#' @param x,y,z Coordinates to get noise value from
#' @export
gen_cubic <- function(x, y = NULL, z = NULL, frequency = 1, seed = NULL, ...) {
  dims <- check_dims(x, y, z)
  if (is.null(seed)) seed <- random_seed()
  if (is.null(z)) {
    gen_cubic2d_c(dims$x, dims$y, frequency, seed)
  } else {
    gen_cubic3d_c(dims$x, dims$y, dims$z, frequency, seed)
  }
}
