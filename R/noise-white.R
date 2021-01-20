#' White noise generator
#'
#' White noise is a random noise with equal intensities at different
#' frequencies. It is most well-known as what appeared on old televisions when
#' no signal was found.
#'
#' @inheritParams noise_simplex
#'
#' @return For `noise_white()` a matrix if `length(dim) == 2` or an array if
#' `length(dim) >= 3`. For `gen_white()` a numeric vector matching the length of
#' the input.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_white(c(100, 100))
#'
#' plot(as.raster(normalise(noise)))
#'
#' # Using the generator
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_white(grid$x, grid$y)
#' plot(grid, noise)
#'
noise_white <- function(dim, frequency = 0.01, pertubation = 'none', pertubation_amplitude = 1) {
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1L

  white_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
             freq = frequency, pertube = pertubation, pertube_amp = pertubation_amplitude)
  if (length(dim) == 2) {
    noise <- white_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- white_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else if (length(dim) == 4) {
    if (pertubation != 0) stop('4D white noise does not support pertubation', call. = FALSE)

    noise <- white_4d_c(dim[1], dim[2], dim[3], dim[4], seed = sample(.Machine$integer.max, size = 1),
                        freq = frequency, pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('White noise only supports two, three, or four dimensions', call. = FALSE)
  }
  noise
}

#' @rdname noise_white
#' @param x,y,z,t Coordinates to get noise value from
#' @export
gen_white <- function(x, y = NULL, z = NULL, t = NULL, frequency = 1, seed = NULL, ...) {
  dims <- check_dims(x, y, z, t)
  if (is.null(seed)) seed <- random_seed()
  if (is.null(t)) {
    if (is.null(z)) {
      gen_white2d_c(dims$x, dims$y, frequency, seed)
    } else {
      gen_white3d_c(dims$x, dims$y, dims$z, frequency, seed)
    }
  } else {
    gen_white4d_c(dims$x, dims$y, dims$z, dims$t, frequency, seed)
  }
}
