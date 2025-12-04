interpolators <- c('linear', 'hermite', 'quintic')
pertubations <- c('none', 'normal', 'fractal')
fractals <- c('none', 'fbm', 'billow', 'rigid-multi')
distances <- c('euclidean', 'manhattan', 'natural')
values <- c('cell', 'noise', 'distance', 'distance2', 'distance2add', 'distance2sub', 'distance2mul', 'distance2div')

random_seed <- function(n = 1, seed = NULL) {
  if (!is.null(seed)) {
    next_seed <- random_seed()
    set.seed(as.integer(seed[1]))
    on.exit(set.seed(next_seed))
  }
  if (isTRUE(getOption('ambient.old_seed'))) {
    sample(.Machine$integer.max, size = n)
  } else {
    c(seed, sample(.Machine$integer.max, size = n))[seq_len(n)]
  }
}

check_dims <- function(x, y = NULL, z = NULL, t = NULL) {
  l <- max(length(x), length(y), length(z), length(t))
  if (length(x) == 1) x <- rep(x, l)
  if (is.null(y)) y <- 0
  if (length(y) == 1) y <- rep(y, l)
  if (length(z) == 1) z <- rep(z, l)
  if (length(t) == 1) t <- rep(t, l)

  if (!is.null(x)) x <- as.numeric(x)
  if (!is.null(y)) y <- as.numeric(y)
  if (!is.null(z)) z <- as.numeric(z)
  if (!is.null(t)) t <- as.numeric(t)

  if (l != length(x)) cli::cli_abort('{.arg x} must either be of length 1, or match the total length')
  if (!is.null(y) && l != length(y)) cli::cli_abort('{.arg y} must either be of length 1, or match the total length', call. = FALSE)
  if (!is.null(z) && l != length(z)) cli::cli_abort('{.arg z} must either be of length 1, or match the total length', call. = FALSE)
  if (!is.null(t) && l != length(t)) cli::cli_abort('{.arg t} must either be of length 1, or match the total length', call. = FALSE)
  list(x = x, y = y, z = z, t = t)
}
