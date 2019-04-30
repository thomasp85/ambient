.onLoad <- function(...) {
  register_s3_method("dplyr", "mutate", "long_grid")
  register_s3_method("dplyr", "transmute", "long_grid")
  register_s3_method("dplyr", "select", "long_grid")
  register_s3_method("dplyr", "arrange", "long_grid")
  register_s3_method("dplyr", "filter", "long_grid")
  register_s3_method("dplyr", "summarise", "long_grid")
  register_s3_method("dplyr", "summarize", "long_grid")
  register_s3_method("dplyr", "slice", "long_grid")

  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
