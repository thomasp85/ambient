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
#' @return For `noise_worley()` a matrix if `length(dim) == 2` or an array if
#' `length(dim) == 3`. For `gen_worley()` a numeric vector matching the length of
#' the input.
#'
#' @references Worley, Steven (1996). *A cellular texture basis function*. Proceedings of the 23rd annual conference on computer graphics and interactive techniques. pp. 291–294. ISBN 0-89791-746-4
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_worley(c(100, 100))
#'
#' plot(as.raster(normalise(noise)))
#'
#' # Using the generator and another value metric
#' grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
#' grid$noise <- gen_worley(grid$x, grid$y, value = 'distance')
#' plot(grid, noise)
#'
noise_worley <- function(dim, frequency = 0.01, distance = 'euclidean',
                         fractal = 'none', octaves = 3, lacunarity = 2, gain = 0.5,
                   value = 'cell', distance_ind = c(1, 2), jitter = 0.45,
                   pertubation = 'none', pertubation_amplitude = 1) {
  distance <- arg_match0(distance, distances)
  distance <- match(distance, distances) - 1L
  distance_ind <- as.integer(distance_ind) - 1L
  value <- arg_match0(value, values)
  value <- match(value, values) - 1L
  fractal <- arg_match0(fractal, fractals)
  fractal <- match(fractal, fractals) - 1L
  pertubation <- arg_match0(pertubation, pertubations)
  pertubation <- match(pertubation, pertubations) - 1L

  if (length(dim) == 2) {
    noise <- worley_2d_c(dim[1], dim[2], seed = sample(.Machine$integer.max, size = 1),
                         freq = frequency, fractal = fractal, octaves = octaves,
                         lacunarity = lacunarity, gain = gain,dist = distance,
                         value = value, dist2ind = distance_ind, jitter = jitter,
                         pertube = pertubation, pertube_amp = pertubation_amplitude)
  } else if (length(dim) == 3) {
    noise <- worley_3d_c(dim[1], dim[2], dim[3], seed = sample(.Machine$integer.max, size = 1),
                         freq = frequency, fractal = fractal, octaves = octaves,
                         lacunarity = lacunarity, gain = gain,dist = distance,
                         value = value, dist2ind = distance_ind, jitter = jitter,
                         pertube = pertubation, pertube_amp = pertubation_amplitude)
    noise <- array(noise, dim)
  } else {
    cli::cli_abort('Worley noise only supports two or three dimensions')
  }
  noise
}

#' @rdname noise_worley
#' @param x,y,z Coordinates to get noise value from
#' @export
gen_worley <- function(x, y = NULL, z = NULL, frequency = 1, seed = NULL,
                       distance = 'euclidean', value = 'cell',
                       distance_ind = c(1, 2), jitter = 0.45, ...) {
  dims <- check_dims(x, y, z)
  distance <- arg_match0(distance, distances)
  distance <- match(distance, distances) - 1L
  distance_ind <- as.integer(distance_ind) - 1L
  value <- arg_match0(value, values)
  value <- match(value, values) - 1L
  if (is.null(seed)) seed <- random_seed()
  frequency <- as.numeric(frequency)
  seed <- as.integer(seed)
  if (is.null(z)) {
    gen_worley2d_c(dims$x, dims$y, frequency, seed, distance, value,
                   distance_ind, jitter)
  } else {
    gen_worley3d_c(dims$x, dims$y, dims$z, frequency, seed, distance, value,
                   distance_ind, jitter)
  }
}
