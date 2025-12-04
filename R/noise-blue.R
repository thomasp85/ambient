#' Blue noise generator
#'
#' Blue noise is a form of noise that has weak low-frequency. This means that
#' it is devoid of larger structures and can be blurred to an even gray. Blue
#' noise in ambient is calculated using the popular Void-and-cluster method
#' developed by Ulichney. Calculating blue noise is much more computationally
#' expensive than e.g. white noise so ambient does not provide a `gen_blue()`
#' generator, only the `noise_blue()` texture function. Computation time
#' increases linearly with the number of pixels in the texture and can get
#' prohibitly long very soon. However, blue noise is tile-able so a good
#' suggestion is to try tiling e.g. a 64x64 texture to the desired dimensions
#' and see if that suffices.
#'
#' @inheritParams noise_simplex
#' @param sd The standard deviation of the gaussian filter to apply during the
#' search for clusters and voids.
#' @param seed_frac The fraction of pixels to seed the algorithm with during
#' start
#'
#' @return For `noise_white()` a vector if `length(dim) == 1`, matrix if
#' `length(dim) == 2` or an array if `length(dim) >= 3`.
#'
#' @references R. A. Ulichney (1993). *Void-and-cluster method for dither array generation*. Proc. SPIE 1913, Human Vision, Visual Processing, and Digital Display IV
#'
#' @export
#'
#' @examples
#' # Basic use
#' noise <- noise_blue(c(64, 64))
#'
#' plot(as.raster(normalise(noise)))
#'
noise_blue <- function(dim, sd = 10, seed_frac = 0.1) {
  n_pixels <- prod(dim)
  n_seeds <- floor(max(1, min((n_pixels - 1) / 2, n_pixels * seed_frac)))
  seed_texture <- noise_white(dim)
  seed_texture[] <- ifelse(order(seed_texture) <= n_seeds, 1, 0)
  kernel <- create_kernel(dim, sd)
  while (TRUE) {
    tightest <- find_tightest_cluster(seed_texture, kernel)
    seed_texture[tightest] <- 0
    voidest <- find_voidest_cluster(seed_texture, kernel)
    if (all(tightest == voidest)) {
      seed_texture[tightest] <- 1
      break
    } else {
      seed_texture[voidest] <- 1
    }
  }
  dither <- array(0, dim = dim)
  pattern <- seed_texture
  for (i in rev(seq_len(n_seeds)) - 1) {
    tightest <- find_tightest_cluster(pattern, kernel)
    pattern[tightest] <- 0
    dither[tightest] <- i
  }
  pattern <- seed_texture
  for (i in seq(n_seeds, floor(n_pixels / 2) - 1)) {
    voidest <- find_voidest_cluster(pattern, kernel)
    pattern[voidest] <- 1
    dither[voidest] <- i
  }
  for (i in seq(floor(n_pixels / 2), n_pixels - 1)) {
    tightest <- find_tightest_cluster(pattern, kernel)
    pattern[tightest] <- 1
    dither[tightest] <- i
  }
  dither <- dither / (n_pixels - 1)
  if (length(dim) == 1) {
    as.vector(dither)
  } else if (length(dim) == 2) {
    as.matrix(dither)
  } else {
    dither
  }
}

create_kernel <- function(dim, sd) {
  i <- do.call(
    expand.grid,
    lapply(dim, function(d) c(seq(0, d / 2), seq(-floor((d - 1) / 2), -1)))
  )
  v <- exp(-rowSums(i^2) / (2 * sd^2)) / (sd * sqrt(2 * pi))^length(dim)
  array(v, dim)
}

#' @importFrom stats fft
find_voidest_cluster <- function(pattern, kernel) {
  if (sum(pattern) * 2 >= length(pattern)) {
    pattern <- abs(pattern - 1)
  }
  filtered <- Re(fft(fft(pattern) * kernel, inverse = TRUE) / length(pattern))
  which.min(ifelse(pattern, 2, filtered))
}

#' @importFrom stats fft
find_tightest_cluster <- function(pattern, kernel) {
  if (sum(pattern) * 2 >= length(pattern)) {
    pattern <- abs(pattern - 1)
  }
  filtered <- Re(fft(fft(pattern) * kernel, inverse = TRUE) / length(pattern))
  which.max(ifelse(pattern, filtered, -1))
}
