#' Configure and validate the Python environment used by GBPP
#'
#' Configures `reticulate` to use the Python interpreter specified by
#' the `GBPP_PYTHON` or `RETICULATE_PYTHON` environment variable or,
#' when neither is set, auto-discovers a project-local virtual environment (`.venv`).
#'
#' @return Invisibly returns `TRUE` if the Python environment is available.
#' @keywords internal
#' @export
configure_gbpp_python <- function() {

  # If Python is already initialized in this R session, avoid re-initialization
  if (reticulate::py_available(initialize = FALSE)) {
    return(invisible(TRUE))
  }

  python <- Sys.getenv("GBPP_PYTHON")

  if (!nzchar(python)) {
    python <- Sys.getenv("RETICULATE_PYTHON")
  }

  if (!nzchar(python)) {
    curr <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    candidate <- NULL

    for (i in seq_len(5L)) {
      win_py <- file.path(curr, ".venv", "Scripts", "python.exe")
      unix_py <- file.path(curr, ".venv", "bin", "python")

      if (file.exists(win_py)) {
        candidate <- win_py
        break
      } else if (file.exists(unix_py)) {
        candidate <- unix_py
        break
      }

      parent <- dirname(curr)
      if (identical(parent, curr)) break
      curr <- parent
    }

    if (!is.null(candidate) && file.exists(candidate)) {
      python <- candidate
    } else {
      stop(
        paste0(
          "GBPP Python environment could not be found. ",
          "Set the `GBPP_PYTHON` environment variable or ",
          "create a project-local `.venv` environment."
        ),
        call. = FALSE
      )
    }
  }

  if (!file.exists(python)) {
    stop(
      paste0(
        "The Python executable does not exist: ",
        python
      ),
      call. = FALSE
    )
  }

  reticulate::use_python(
    python,
    required = TRUE
  )

  invisible(TRUE)
}

#' Locate GBPP's bundled Python backend files
#'
#' @return Path to the directory containing GBPP Python backend modules.
#' @keywords internal
#' @export
gbpp_python_path <- function() {

  path <- system.file(
    "python",
    package = "gbpp"
  )

  if (nzchar(path) && dir.exists(path)) {
    return(normalizePath(path, winslash = "/"))
  }

  # Development fallback: search current directory and up to 5 parent levels
  curr <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(5L)) {
    candidate <- file.path(curr, "inst", "python")
    if (dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/"))
    }
    parent <- dirname(curr)
    if (identical(parent, curr)) break
    curr <- parent
  }

  stop(
    "GBPP Python backend files could not be found.",
    call. = FALSE
  )
}
