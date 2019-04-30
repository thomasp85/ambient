#' Clamped fractal
#'
#' This fractal is a slight variation of [fbm()] fractal. Before adding the new
#' octave to the cumulated values it will clamp it between a minimum and maximum
#' value. This function is intended to be used in conjunction with [fracture()]
#'
#' @inheritParams fbm
#' @param min,max The upper and lower bounds of the noise values
#'
#' @family Fractal functions
#'
#' @export
#'
#' @examples
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#'
#' grid$simplex <- fracture(gen_simplex, clamped, octaves = 8, x = grid$x,
#'                          y = grid$y)
#' plot(as.raster(grid, normalise(simplex)))
#'
clamped <- function(base, new, strength, min = 0, max = Inf, ...) {
  new[new < min] <- min
  new[new > max] <- max
  base + new * strength
}
