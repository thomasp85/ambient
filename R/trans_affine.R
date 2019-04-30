#' Apply linear transformation to a long_grid
#'
#' This function allows you to calculate linear transformations of coordinates
#' in a long_grid object. You can either pass in a transformation matrix or a
#' trans object as produced by `ggforce::linear_trans(...)`. The latter makes it
#' easy to stack multiple transformations into one, but require the ggforce
#' package.
#'
#' @param grid A long_grid object
#' @param ... A sequence of transformations
#' @param x,y The unquoted columns holding the x and y coordinates to transform
#' @param output The column names to save the transformed coordinates in. If
#' `NULL` the names will be derived from `x` and `y`.
#'
#' @section Linear Transformations:
#' The following transformation matrix constructors are supplied, but you can
#' also provide your own 3x3 matrices to `translate()`
#'
#' - `rotate()`: Rotate coordinates by `angle` (in radians) around the center
#'   counter-clockwise.
#' - `stretch()`: Stretches the x and/or y dimension by multiplying it with
#'   `x0`/`y0`.
#' - `shear()`: Shears the x and/or y dimension by `x0`/`y0`.
#' - `translate()`: Moves coordinates by `x0`/`y0`.
#' - `reflect()`: Reflects coordinates through the line that goes through `0, 0`
#'   and `x0, y0`.
#'
#' @export
#'
#' @examples
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid <- trans_affine(grid, rotate(pi/3), shear(-2), rotate(-pi/3), x = x, y = y)
#' grid$chess <- gen_checkerboard(grid$x, grid$y)
#'
#' plot(as.raster(grid, chess))
#'
trans_affine <- function(grid, ..., x, y, output = NULL) {
  x <- enquo(x)
  y <- enquo(y)
  if (!inherits(grid, 'long_grid')) {
    stop('grid must be a long_grid object', call. = FALSE)
  }
  if (is.null(output)) {
    output <- c(quo_name(x), quo_name(y))
  }
  trans_mat <- Reduce(function(l, r) r %*% l, list(...))
  trans <- trans_mat %*% rbind(eval_tidy(x, grid), eval_tidy(y, grid), z = 1)
  grid[[output[1]]] <- trans[1, ]
  grid[[output[2]]] <- trans[2, ]
  grid
}

#' @rdname trans_affine
#' @param angle An angle in radians
#' @export
rotate <- function(angle = 0) {
  matrix(c(cos(angle), -sin(angle), 0, sin(angle), cos(angle), 0, 0, 0, 1),
         ncol = 3)
}
#' @rdname trans_affine
#' @param x0 the transformation magnitude in the x-direction
#' @param y0 the transformation magnitude in the x-direction
#' @export
stretch <- function(x0 = 0, y0 = 0) {
  matrix(c(x0, 0, 0, 0, y0, 0, 0, 0, 1), ncol = 3)
}
#' @rdname trans_affine
#' @export
shear <- function(x0 = 0, y0 = 0) {
  matrix(c(1, y0, 0, x0, 1, 0, 0, 0, 1), ncol = 3)
}
#' @rdname trans_affine
#' @export
translate <- function(x0 = 0, y0 = 0) {
  matrix(c(1, 0, 0, 0, 1, 0, x0, y0, 1), ncol = 3)
}
#' @rdname trans_affine
#' @export
reflect <- function(x0 = 0, y0 = 0) {
  l <- x0^2 + y0^2
  matrix(
    c(
      (x0^2 - y0^2) / l,
      2 * x0 * y0 / l,
      0,
      2 * x0 * y0 / l,
      (y0^2 - x0^2) / l,
      0,
      0,
      0,
      1
    ),
    ncol = 3
  )
}
