#' Generate a checkerboard pattern
#'
#' This generator supplies 0 or 1 value depending on the provided coordinates
#' position on a checkerboard. The `frequency` determines the number of squares
#' per unit.
#'
#' @param x,y,z,t The coordinates to get pattern from
#' @param frequency The frequency of the generator
#' @param ... ignored
#'
#' @return A numeric vector
#'
#' @export
#'
#' @family Pattern generators
#'
#' @examples
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$chess <- gen_checkerboard(grid$x, grid$y)
#'
#' plot(grid, chess)
#'
gen_checkerboard <- function(
  x,
  y = NULL,
  z = NULL,
  t = NULL,
  frequency = 1,
  ...
) {
  dims <- check_dims(x, y, z, t)
  all <- floor(dims$x * frequency) +
    floor((dims$y %||% 0) * frequency) +
    floor((dims$z %||% 0) * frequency) +
    floor((dims$t %||% 0) * frequency)
  as.numeric((all %% 2) == 0)
}
