mutate.long_grid <- function(.data, ...) {
  dims <- attr(.data, 'grid_dims')
  .data <- NextMethod()
  `attr<-`(.data, 'grid_dims', dims)
}

transmute.long_grid <- function(.data, ...) {
  dims <- attr(.data, 'grid_dims')
  .data <- NextMethod()
  `attr<-`(.data, 'grid_dims', dims)
}

select.long_grid <- function(.data, ...) {
  dims <- attr(.data, 'grid_dims')
  .data <- NextMethod()
  `attr<-`(.data, 'grid_dims', dims)
}

arrange.long_grid <- function(.data, ...) {
  stop('long_grid objects cannot be rearranged', call. = FALSE)
}

filter.long_grid <- function(.data, ...) {
  stop('long_grid objects cannot be filtered', call. = FALSE)
}

summarise.long_grid <- function(.data, ...) {
  stop('long_grid objects cannot be summarised', call. = FALSE)
}
summarize.long_grid <- summarise.long_grid

slice.long_grid <- function(.data, ...) {
  stop('long_grid objects cannot be sliced. Use slice_at to slice dimensions', call. = FALSE)
}
