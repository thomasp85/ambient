interpolators <- c('linear', 'hermite', 'quintic')
pertubations <- c('none', 'normal', 'fractal')
fractals <- c('none', 'fbm', 'billow', 'rigid-multi')
distances <- c('euclidean', 'manhattan', 'natural')
values <- c('cell', 'noise', 'distance', 'distance2', 'distance2add', 'distance2sub', 'distance2mul', 'distance2div')

random_seed <- function(n = 1, seed = NULL) {
  if (!is.null(seed)) {
    next_seed <- random_seed()
    set.seed(as.integer(seed))
    on.exit(set.seed(next_seed))
  }
  sample(.Machine$integer.max, size = n)
}

check_dims <- function(x, y = NULL, z = NULL, t = NULL) {
  l <- max(length(x), length(y), length(z), length(t))
  if (length(x) == 1) x <- rep(x, l)
  if (is.null(y)) y <- 0
  if (length(y) == 1) y <- rep(y, l)
  if (length(z) == 1) z <- rep(z, l)
  if (length(t) == 1) t <- rep(t, l)

  if (l != length(x)) stop('x must either be of length 1, or match the total length', call. = FALSE)
  if (!is.null(y) && l != length(y)) stop('y must either be of length 1, or match the total length', call. = FALSE)
  if (!is.null(z) && l != length(z)) stop('z must either be of length 1, or match the total length', call. = FALSE)
  if (!is.null(t) && l != length(t)) stop('t must either be of length 1, or match the total length', call. = FALSE)
  list(x = x, y = y, z = z, t = t)
}
