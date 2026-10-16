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

    if (is.null(candidate) || !file.exists(candidate)) {
      if (tryCatch(reticulate::virtualenv_exists("r-gbpp"), error = function(e) FALSE)) {
        reticulate::use_virtualenv("r-gbpp", required = TRUE)
        return(invisible(TRUE))
      }
      if (tryCatch(reticulate::condaenv_exists("r-gbpp"), error = function(e) FALSE)) {
        reticulate::use_condaenv("r-gbpp", required = TRUE)
        return(invisible(TRUE))
      }
      disc <- tryCatch(reticulate::py_discover_config(), error = function(e) NULL)
      if (!is.null(disc) && nzchar(disc$python) && file.exists(disc$python)) {
        candidate <- disc$python
      }
    }

    if (!is.null(candidate) && file.exists(candidate)) {
      python <- candidate
    } else {
      stop(
        paste0(
          "GBPP Python environment could not be found.\n",
          "You can automatically set up a dedicated environment with all required packages by running:\n",
          "  gbpp::install_gbpp_python()\n",
          "Or configure an existing Python interpreter via `Sys.setenv(GBPP_PYTHON = 'path/to/python')`."
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

#' Install Python dependencies for GBPP
#'
#' Automatically creates an isolated virtual environment (`"r-gbpp"`) and
#' installs the required Python dependencies: `numpy`, `scipy`, `xgboost`,
#' `lightgbm`, and `catboost`.
#'
#' @param envname Name of the Python virtual environment. Default is `"r-gbpp"`.
#' @param method Method passed to [reticulate::py_install()], e.g. `"auto"`, `"virtualenv"`, or `"conda"`.
#' @param ... Additional arguments passed to [reticulate::py_install()].
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
install_gbpp_python <- function(
    envname = "r-gbpp",
    method = "auto",
    ...
) {
  pkgs <- c("numpy", "scipy", "xgboost", "lightgbm", "catboost")
  message("Installing GBPP Python dependencies (", paste(pkgs, collapse = ", "), ") into environment '", envname, "'...")
  reticulate::py_install(
    packages = pkgs,
    envname = envname,
    method = method,
    ...
  )
  message("GBPP Python dependencies installed successfully!")
  invisible(TRUE)
}

#' Check GBPP Python Environment and Engine Status
#'
#' Verifies that Python is properly configured and inspects the availability
#' and versions of required packages (`lightgbm`, `catboost`, `xgboost`, `numpy`, `scipy`).
#'
#' @return Invisibly returns a named logical vector of package availability.
#' @export
check_gbpp_python <- function() {
  cat("GBPP Python Environment Diagnostic\n")
  cat("------------------------------------\n")
  configure_gbpp_python()
  py_config <- reticulate::py_config()
  cat("Python executable :", py_config$python, "\n")
  cat("Python version    :", as.character(py_config$version), "\n")

  pkgs <- c("numpy", "scipy", "lightgbm", "catboost", "xgboost")
  status <- logical(length(pkgs))
  names(status) <- pkgs

  cat("\nRequired Boosting Packages:\n")
  for (pkg in pkgs) {
    avail <- reticulate::py_module_available(pkg)
    status[pkg] <- avail
    if (avail) {
      mod <- tryCatch(reticulate::import(pkg), error = function(e) NULL)
      ver <- if (!is.null(mod) && !is.null(mod$`__version__`)) mod$`__version__` else "available"
      cat(sprintf("  %-10s : [OK] version %s\n", pkg, ver))
    } else {
      cat(sprintf("  %-10s : [NOT FOUND]\n", pkg))
    }
  }
  invisible(status)
}
