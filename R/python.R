#' Configure and validate the Python environment used by GBPP
#'
#' Configures `reticulate` to use the Python interpreter specified by
#' the `GBPP_PYTHON` environment variable or, when that variable is not
#' set, the project-local Python environment.
#'
#' @return Invisibly returns `TRUE` if the Python environment is available.
#' @keywords internal
configure_gbpp_python <- function() {

  python <- Sys.getenv("GBPP_PYTHON")

  if (!nzchar(python)) {

    project_root <- getwd()

    project_python <- file.path(
      project_root,
      ".venv",
      "Scripts",
      "python.exe"
    )

    if (file.exists(project_python)) {
      python <- project_python
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
gbpp_python_path <- function() {

  path <- system.file(
    "python",
    package = "gbpp"
  )

  if (nzchar(path) && dir.exists(path)) {
    return(path)
  }

  # Development fallback for devtools::load_all()
  path <- file.path(
    getwd(),
    "inst",
    "python"
  )

  if (dir.exists(path)) {
    return(path)
  }

  stop(
    "GBPP Python backend files could not be found.",
    call. = FALSE
  )
}
