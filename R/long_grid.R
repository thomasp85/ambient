#' Create a long format grid
#'
#' This function creates a 1-4 dimensional grid in long format, with the cell
#' positions encoded in the `x`, `y`, `z`, and `t` columns. A long_cell object
#' is the base class for the tidy interface to ambient, and allows a very
#' flexible approach to pattern generation at the expense of slightly lower
#' performance than the `noise_*` functions that maps directly to the underlying
#' C++ code.
#'
#' @param x,y,z,t For `long_grid()` vectors of grid cell positions for each
#' dimension. The final dimensionality of the object is determined by how many
#' arguments are given. For `slice_at()` an integer defining the index at the
#' given dimension to extract.
#' @param grid A long_grid object
#' @param dim The dimension to get the cell index at, either as an integer or
#' string.
#' @param value The unquoted value to use for filling out the array/matrix
#' @param ... Arguments passed on to methods (ignored)
#'
#' @export
#'
#' @examples
#' grid <- long_grid(1:10, seq(0, 1, length = 6), c(3, 6))
#'
#' # Get which row each cell belongs to
#' grid_cell(grid, 2) # equivalent to grid_cell(grid, 'y')
#'
#' # Convert the long_grid to an array and fill with the x position
#' as.array(grid, x)
#'
#' # Extract the first column
#' slice_at(grid, x = 1)
#'
#' # Convert the first column to a matrix filled with y position
#' as.matrix(slice_at(grid, x = 1), y)
#'
long_grid <- function(x, y = NULL, z = NULL, t = NULL) {
  dims <- c(length(x), length(y), length(z), length(t))
  if (any(diff(dims == 0) < 0)) {
    stop('A zero-length dimension cannot be followed by a non-zero-length dimension', call. = FALSE)
  }

  len <- prod(dims[dims != 0])

  x <- rep(x, each = len / dims[1])
  if (!is.null(y)) {
    y <- rep(y, each = len / prod(dims[1:2]), length.out = len)
  }
  if (!is.null(z)) {
    z <- rep(z, each = len / prod(dims[1:3]), length.out = len)
  }
  if (!is.null(t)) {
    t <- rep_len(t, len)
  }
  grid <- list(x = x, y = y, z = z, t = t)[dims != 0]
  attributes(grid) <- list(
    class = c('long_grid', 'tbl_df', 'tbl', 'data.frame'),
    names = names(grid),
    row.names = .set_row_names(len),
    dims = dims[dims != 0]
  )
  grid
}

#' @rdname long_grid
#' @export
#'
grid_cell <- function(grid, dim, ...) {
  UseMethod('grid_cell')
}
#' @export
grid_cell.long_grid <- function(grid, dim, ...) {
  if (is.character(dim)) dim <- match(tolower(dim), c('x', 'y', 'z', 't'), nomatch = -1L)
  if (dim <= 0) {
    stop('dim must be positive or match x, y, z, or t', call. = FALSE)
  }
  dims <- attr(grid, 'dims')
  if (dim > length(dims)) {
    return(rep_len(NA, nrow(grid)))
  }
  cells <- seq_len(dims[dim])
  len <- prod(dims)
  rep(cells, each = len / prod(dims[1:dim]), length.out = len)
}

#' @rdname long_grid
#' @export
as.array.long_grid <- function(x, value, ...) {
  val <- eval_tidy(enquo(value), x)
  dims <- rev(attr(x, 'dims'))
  dimnames <- rev(list(x = NULL, y = NULL, z = NULL, t = NULL)[seq_along(dims)])
  array(val, dims, dimnames)
}

#' @rdname long_grid
#' @export
as.matrix.long_grid <- function(x, value, ...) {
  val <- eval_tidy(enquo(value), x)
  dims <- attr(x, 'dims')
  if (sum(dims > 1) > 2) {
    stop('as.matrix can only be applied to 2-dimensional grids', call. = FALSE)
  }
  use_dims <- which(dims > 1)
  if (length(use_dims) < 2) {
    extra_dims <- which(dims == 1)[seq_len(2 - length(use_dims))]
    use_dims <- sort(c(use_dims, extra_dims))
  }
  matrix(val, ncol = dims[use_dims[1]], nrow = dims[use_dims[2]], dimnames = list(x = NULL, y = NULL, z = NULL, t = NULL)[use_dims])
}

#' @rdname long_grid
#' @export
#' @importFrom grDevices as.raster
as.raster.long_grid <- function(x, value, ...) {
  value <- enquo(value)
  as.raster(as.matrix(x, !!value), ...)
}

#' @rdname long_grid
#' @export
slice_at <- function(grid, ...) {
  UseMethod('slice_at')
}
#' @export
slice_at.long_grid <- function(grid, x = NULL, y = NULL, z = NULL, t = NULL, ...) {
  keep <- rep(TRUE, nrow(grid))
  dims <- attr(grid, 'dims')
  n_dims <- length(dims)
  if (!is.null(x)) {
    keep[grid_cell(grid, 1) != x] <- FALSE
    dims[1] <- 1L
  }
  if (!is.null(y) && n_dims >= 2) {
    keep[grid_cell(grid, 2) != y] <- FALSE
    dims[2] <- 1L
  }
  if (!is.null(z) && n_dims >= 3) {
    keep[grid_cell(grid, 3) != z] <- FALSE
    dims[3] <- 1L
  }
  if (!is.null(t) && n_dims == 4) {
    keep[grid_cell(grid, 4) != t] <- FALSE
    dims[4] <- 1L
  }
  grid <- grid[keep, , drop = FALSE]
  attr(grid, 'dims') <- dims
  grid
}
