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
  cli::cli_abort('{.cls long_grid} objects cannot be rearranged')
}

filter.long_grid <- function(.data, ...) {
  cli::cli_abort('{.cls long_grid} objects cannot be filtered')
}

summarise.long_grid <- function(.data, ...) {
  cli::cli_abort('{.cls long_grid} objects cannot be summarised')
}
summarize.long_grid <- summarise.long_grid

slice.long_grid <- function(.data, ...) {
  cli::cli_abort('{.cls long_grid} objects cannot be sliced. Use slice_at to slice dimensions')
}
