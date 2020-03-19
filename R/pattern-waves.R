#' Generate a wave pattern
#'
#' This generator generates multidimensional waves based on [cos] to the
#' distance to the center. This means that you can create ripple waves or
#' parallel waves depending on how many dimensions you provide. The output is
#' scaled between -1 and 1 and the frequency determines the number of waves per
#' unit. The result is much like [gen_spheres()] but has smooth transitions at
#' each extreme.
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
#' grid$ripple <- gen_waves(grid$x, grid$y)
#' grid$wave <- gen_waves(grid$x)
#'
#' plot(grid, ripple)
#' plot(grid, wave)
#'
gen_waves <- function(x, y = NULL, z = NULL, t = NULL, frequency = 1) {
  all <- (x * frequency)^2 +
    ((y %||% 0 * frequency))^2 +
    ((z %||% 0 * frequency))^2 +
    ((t %||% 0 * frequency))^2
  dist <- sqrt(all)
  cos(dist * pi * 2)
}
