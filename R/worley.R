#' Worley (cell) noise generator
#'
#' Worley noise, sometimes called cell (or cellular) noise, is quite distinct
#' due to it's kinship to voronoi tesselation. It is created by sampling random
#' points in space and then for any point in space measure the distance to the
#' closest point. The noise can be modified further by changing either the
#' distance measure or by combining multiple distances. The noise algorithm was
#' developed by Steven Worley in 1996 and has been used to simulated water and
#' stone textures among other things.
#'
#' @inheritParams noise_perlin
#' @param distance The distance measure to use, either `'euclidean'` (default),
#' `'manhattan'`, or `'natural'` (a mix of the two)
#' @param value The noise value to return. Either
#' - `'value'` (default) A random value associated with the closest point
#' - `'distance'` The distance to the closest point
#' - `'distance2'` The distance to the nth closest point (n given by
#' `distance_ind[1]`)
#' - `'distance2add'` Addition of the distance to the nth and mth closest point given in `distance_ind`
#' - `'distance2sub'` Substraction of the distance to the nth and mth closest point given in `distance_ind`
#' - `'distance2mul'` Multiplication of the distance to the nth and mth closest point given in `distance_ind`
#' - `'distance2div'` Division of the distance to the nth and mth closest point given in `distance_ind`
#' @param distance_ind Reference to the nth and mth closest points that should
#' be used when calculating `value`.
#' @param jitter The maximum distance a point can move from its start position
#' during sampling of cell points.
#'
#' @return If `length(dim) == 2` a matrix, if `length(dim) == 3` a 3-dimensional
#' array.
#'
#' @references Worley, Steven (1996). *A cellular texture basis function*. Proceedings of the 23rd annual conference on computer graphics and interactive techniques. pp. 291–294. ISBN 0-89791-746-4
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_worley(c(100, 100))
#'
#' image(noise, col = grey.colors(256, 0, 1))
#'
noise_worley <- function(dim, frequency = 0.01, distance = 'euclidean',
                   value = 'cell', distance_ind = c(1, 2), jitter = 0.45,
                   pertubation = 'none', pertubation_amplitude = 1) {
  distance <- match.arg(distance, distances)
  distance <- match(distance, distances) - 1
  distance_ind <- distance_ind - 1
  value <- match.arg(value, values)
  value <- match(value, values) - 1
  pertubation <- match.arg(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1

  if (length(dim) == 2) {
    noise <- worley_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                         freq = frequency, dist = distance, value = value,
                         dist2ind = distance_ind, jitter = jitter,
                         pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- worley_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                         freq = frequency, dist = distance, value = value,
                         dist2ind = distance_ind, jitter = jitter,
                         pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    stop('Worley noise only supports two or three dimensions', call. = FALSE)
  }
  noise
}
