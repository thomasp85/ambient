#' Apply linear transformation to a long_grid
#'
#' This function allows you to calculate linear transformations of coordinates
#' in a long_grid object. You can either pass in a transformation matrix or a
#' trans object as produced by `ggforce::linear_trans(...)`. The latter makes it
#' easy to stack multiple transformations into one, but require the ggforce
#' package.
#'
#' @param x,y The coordinates to transform
#' @param ... A sequence of transformations
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
#' grid$trans <- trans_affine(grid$x, grid$y, rotate(pi/3), shear(-2), rotate(-pi/3))
#' grid$chess <- gen_checkerboard(grid$trans$x, grid$trans$y)
#'
#' plot(as.raster(grid, chess))
#'
trans_affine <- function(x, y, ...) {
  trans_mat <- Reduce(function(l, r) r %*% l, list(...))
  trans <- trans_mat %*% rbind(x, y, z = 1)
  data.frame(x = trans[1,], y = trans[2,])
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
