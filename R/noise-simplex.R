#' Simplex noise generator
#'
#' Simplex noise has been developed by Ken Perlin, the inventor of perlin noise,
#' in order to address some of the shortcomings he saw in perlin noise. Compared
#' to perlin noise, simplex noise has lower computational complexity, making it
#' feasable for dimensions above 3 and has no directional artifacts.
#'
#' @param dim The dimensions (height, width, (and depth, (and time))) of the
#' noise to be generated. The length determines the dimensionality of the noise.
#' @inheritParams noise_perlin
#'
#' @return For `noise_simplex()` a matrix if `length(dim) == 2` or an array if
#' `length(dim) >= 3`. For `gen_simplex()` a numeric vector matching the length of
#' the input.
#'
#' @references Ken Perlin, (2001) *Noise hardware*. In Real-Time Shading SIGGRAPH Course Notes, Olano M., (Ed.)
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_simplex(c(100, 100))
#'
#' plot(as.raster(normalise(noise)))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_simplex(grid$x, grid$y)
#' plot(grid, noise)
#'
noise_simplex <- function(
  dim,
  frequency = 0.01,
  interpolator = 'quintic',
  fractal = 'fbm',
  octaves = 3,
  lacunarity = 2,
  gain = 0.5,
  pertubation = 'none',
  pertubation_amplitude = 1
) {
  fractal <- arg_match0(fractal, fractals)
  fractal <- match(fractal, fractals) - 1L
  pertubation <- arg_match0(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1L

  if (length(dim) == 2) {
    noise <- simplex_2d_c(
      dim[1],
      dim[2],
      seed = sample(.Machine$integer.max, size = 1),
      freq = frequency,
      fractal = fractal,
      octaves = octaves,
      lacunarity = lacunarity,
      gain = gain,
      pertube = pertubation,
      pertube_amp = pertubation_amplitude
    )
  } else if (length(dim) == 3) {
    noise <- simplex_3d_c(
      dim[1],
      dim[2],
      dim[3],
      seed = sample(.Machine$integer.max, size = 1),
      freq = frequency,
      fractal = fractal,
      octaves = octaves,
      lacunarity = lacunarity,
      gain = gain,
      pertube = pertubation,
      pertube_amp = pertubation_amplitude
    )
    noise <- array(noise, dim)
  } else if (length(dim) == 4) {
    if (fractal != 0) {
      cli::cli_abort('4D Simplex noise does not support fractals')
    }
    if (pertubation != 0) {
      cli::cli_abort('4D Simplex noise does not support pertubation')
    }

    noise <- simplex_4d_c(
      dim[1],
      dim[2],
      dim[3],
      dim[4],
      seed = sample(.Machine$integer.max, size = 1),
      freq = frequency,
      fractal = fractal,
      octaves = octaves,
      lacunarity = lacunarity,
      gain = gain,
      pertube = pertubation,
      pertube_amp = pertubation_amplitude
    )
    noise <- array(noise, dim)
  } else {
    cli::cli_abort('Simplex noise only supports two, three, or four dimensions')
  }
  noise
}

#' @rdname noise_simplex
#' @param x,y,z,t Coordinates to get noise value from
#' @export
gen_simplex <- function(
  x,
  y = NULL,
  z = NULL,
  t = NULL,
  frequency = 1,
  seed = NULL,
  ...
) {
  dims <- check_dims(x, y, z, t)
  if (is.null(seed)) {
    seed <- random_seed()
  }
  frequency <- as.numeric(frequency)
  seed <- as.integer(seed)
  if (is.null(t)) {
    if (is.null(z)) {
      gen_simplex2d_c(dims$x, dims$y, frequency, seed)
    } else {
      gen_simplex3d_c(dims$x, dims$y, dims$z, frequency, seed)
    }
  } else {
    gen_simplex4d_c(dims$x, dims$y, dims$z, dims$t, frequency, seed)
  }
}
