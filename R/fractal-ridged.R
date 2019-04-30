#' Ridged-Multi fractal
#'
#' This fractal is slightly more complex than the regular [fbm()] fractal. It
#' uses the prior octave to modify the values of the current octave before
#' adding it to the cumulating values. The result of this is that the final
#' values will show steep hills and larger smooth areas, resembling mountain
#' ranges. This function is intended to be used in conjunction with
#' [fracture()]
#'
#' @details
#' The ridged fractal was designed with a slightly more complex gain sequence
#' in mind, and while any sequence or generator would work [fracture()] should
#' be called with `gain = spectral_gain()` to mimick the original intention of
#' the fractal.
#'
#' @inheritParams fbm
#' @param octave The current octave
#' @param offset The new values are first modified by `(offset - abs(new))^2`
#' @param gain A value to multiply the old octave by before using it to modify
#' the new octave
#'
#' @family Fractal functions
#' @aliases rigid rigid-multi
#' @export
#'
#' @examples
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#'
#' grid$simplex <- fracture(gen_simplex, ridged, octaves = 8,
#'                          gain = spectral_gain(), x = grid$x, y = grid$y)
#' plot(as.raster(grid, normalise(simplex)))
#'
ridged <- function(base, new, strength, octave, offset = 1, gain = 2, ...) {
  sig <- (offset - abs(new))^2
  if (octave == 1) {
    old_noise$n <- 1
  }
  sig <- sig * old_noise$n

  old_noise$n <- cap(sig * gain)

  base + sig * strength
}
attr(ridged, 'finalise') <- function(x) x * 1.25 - 1

#' @rdname ridged
#' @param h Each successive gain is raised to the power of `-h`
#' @param lacunarity A multiplier to apply to the previous value before raising
#' it to the power of `-h`
#' @export
spectral_gain <- function(h = 1, lacunarity = 2) {
  frequency <- 1
  function(x) {
    gain <- frequency^-h
    frequency <<- frequency * lacunarity
    gain
  }
}

old_noise <- new.env(parent = emptyenv())
