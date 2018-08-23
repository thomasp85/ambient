#' White noise generator
#'
#' White noise is a random noise with equal intensities at different
#' frequencies. It is most well-known as what appeared on old televisions when
#' no signal was found.
#'
#' @inheritParams noise_simplex
#'
#' @return If `length(dim) == 2` a matrix, if `length(dim) %in% c(3, 4)` a 3- or
#' 4-dimensional array.
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_white(c(100, 100))
#'
#' image(noise, col = grey.colors(256, 0, 1))
#'
noise_white <- function(dim, frequency = 0.01, pertubation = 'none', pertubation_amplitude = 1) {
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1

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
