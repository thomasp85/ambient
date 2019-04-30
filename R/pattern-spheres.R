#' Generate a pattern of concentric spheres
#'
#' This generator creates a pattern of concentric circles centered at 0.
#' Depending on how many dimensions you supply it can be used to generate
#' cylinders and circles as well. The output value is the shortest distance to
#' the nearest sphere normalised to be between -1 and 1. The frequency
#' determines the radius multiplier for each unit sphere.
#'
#' @param x,y,z,t The coordinates to get pattern from
#' @param frequency The frequency of the generator
#'
#' @return A numeric vector
#'
#' @export
#'
#' @family Pattern generators
#'
#' @examples
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$circles <- gen_spheres(grid$x, grid$y)
#' grid$cylinders <- gen_spheres(grid$x)
#'
#' plot(as.raster(grid, normalise(circles)))
#' plot(as.raster(grid, normalise(cylinders)))
#'
gen_spheres <- function(x, y = NULL, z = NULL, t = NULL, frequency = 1) {
  all <- (x * frequency)^2 +
    ((y %||% 0 * frequency))^2 +
    ((z %||% 0 * frequency))^2 +
    ((t %||% 0 * frequency))^2
  dist <- sqrt(all)
  dist_small <- dist - floor(dist)
  dist_large <- 1 - dist_small
  1 - pmin(dist_small, dist_large) * 4
}
