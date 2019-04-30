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
#' @return If `length(dim) == 2` a matrix, if `length(dim) %in% c(3, 4)` a 3- or
#' 4-dimensional array.
#'
#' @references Ken Perlin, (2001) *Noise hardware*. In Real-Time Shading SIGGRAPH Course Notes, Olano M., (Ed.)
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_simplex(c(100, 100))
#'
#' image(noise, col = grey.colors(256, 0, 1))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_simplex(grid$x, grid$y)
#' plot(as.raster(grid, normalise(noise)))
#'
noise_simplex <- function(dim, frequency = 0.01, interpolator = 'quintic',
                   fractal = 'fbm', octaves = 3, lacunarity = 2, gain = 0.5,
                   pertubation = 'none', pertubation_amplitude = 1) {
  fractal <- match.arg(fractal, fractals)
  fractal <- match(fractal, fractals) - 1
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1

  if (length(dim) == 2) {
    noise <- simplex_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                          freq = frequency, fractal = fractal,
                          octaves = octaves, lacunarity = lacunarity, gain = gain,
                          pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- simplex_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                          freq = frequency, fractal = fractal,
                          octaves = octaves, lacunarity = lacunarity, gain = gain,
                          pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else if (length(dim) == 4) {
    if (fractal != 0) stop('4D Simplex noise does not support fractals', call. = FALSE)
    if (pertubation != 0) stop('4D Simplex noise does not support pertubation', call. = FALSE)

    noise <- simplex_4d_c(dim[1], dim[2], dim[3], dim[4], seed = sample(.Machine$integer.max, size = 1),
                          freq = frequency, fractal = fractal,
                          octaves = octaves, lacunarity = lacunarity, gain = gain,
                          pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('Simplex noise only supports two, three, or four dimensions', call. = FALSE)
  }
  noise
}

#' @rdname noise_simplex
#' @param x,y,z,t Coordinates to get noise value from
#' @export
gen_simplex <- function(x, y = NULL, z = NULL, t = NULL, frequency = 1, seed = NULL) {
  dims <- check_dims(x, y, z, t)
  if (is.null(seed)) seed <- random_seed()
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
