# Coding Standards for `gbpp`

This document establishes the permanent coding, architecture, and safety standards for the `gbpp` (Gradient Boosting for Point Processes) package. All contributors and agentic assistants must strictly follow these conventions.

---

## 1. Naming Conventions

### Exported Functions
- Must strictly use `snake_case`.
- Must have descriptive, verb-first names (e.g., `fit_gbpp()`, `prepare_gbpp()`, `estimate_interaction_integral()`, `calculate_network_dummy_intensity()`).
- Backward-compatible shims follow the legacy pattern `fit_gbpp_<engine>_<objective>()`.

### Internal Helper Functions
- Must use the `.gbpp_*` prefix with `snake_case` (e.g., `.gbpp_fit_xgboost()`, `.gbpp_normalize_params()`, `.gbpp_catboost()`, `.gbpp_python_env_candidates()`).
- Internal functions must never pollute the package export namespace.

### File Organization
All source files in `R/` must follow semantic, domain-prefixed naming:
- `api-*.R`: Public-facing APIs, generic S3 methods, legacy shims (`api-fit.R`, `api-predict.R`, `api-legacy.R`).
- `data-*.R`: Point process data structures and prep pipelines (`data-construct.R`, `data-validate.R`, `data-prepare-planar.R`, `data-prepare-network.R`).
- `engine-*.R`: Boosting backends and hyperparameter translation (`engine-xgboost.R`, `engine-lightgbm.R`, `engine-catboost.R`, `engine-params.R`).
- `math-*.R`: Mathematical formulas, numerical integration, reference derivatives (`math-dummy-intensity.R`, `math-interaction-planar.R`, `math-interaction-network.R`, `math-iterative-weights.R`, `math-objectives-reference.R`).
- `sys-*.R`: System infrastructure, Python environment configuration, package hooks (`sys-python.R`, `sys-package.R`).

---

## 2. S3 Interface & Object Contracts

### Primary Generic
- The primary entry point for model training is:
  ```r
  fit_gbpp(
    data,
    engine = c("xgboost", "lightgbm", "catboost"),
    objective = c("poisson", "weighted_poisson", "logistic",
                  "weighted_logistic", "iterative_poisson", "iterative_logistic"),
    params = list(),
    n_trees = 100L,
    baseline = NULL,
    ...
  )
  ```

### Class Hierarchy
- Fitted model objects must inherit from an engine-specific subclass and the primary generic class:
  ```r
  class(model) <- c(paste0("gbpp_", engine), "gbpp_model")
  ```
  Examples: `c("gbpp_xgboost", "gbpp_model")`, `c("gbpp_lightgbm", "gbpp_model")`, `c("gbpp_catboost", "gbpp_model")`.

### Metadata Storage
- **Never mutate Python objects directly**: Due to `reticulate` intercepting the `$` operator on Python C++ instances, store all R-side metadata as R object attributes:
  ```r
  attr(model, "engine") <- engine
  attr(model, "objective") <- objective
  attr(model, "baseline") <- as.numeric(baseline)
  attr(model, "feature_names") <- colnames(data$X)
  ```

### Prediction Contract
- All models must implement `predict.gbpp_model(object, newdata, type = c("intensity", "link", "raw"), ...)`:
  - `"raw"`: Raw margin output from the underlying booster without baseline offset ($f(x)$).
  - `"link"`: Linear predictor including the baseline offset ($\eta(x) = \eta_0 + f(x)$).
  - `"intensity"`: Natural-scale point process intensity ($\lambda(x) = \exp(\eta(x))$).
- **Mathematical Invariant**: `predict(..., type = "intensity") == exp(predict(..., type = "link"))`.
- **Return Type**: Always returns a standard 1D double vector of length equal to `nrow(newdata)`.

---

## 3. Roxygen2 & Documentation Standards

- **Full Parity**: Every exported function must include complete Roxygen2 documentation:
  - `@title`: Short, single-sentence summary in sentence case.
  - `@description`: Clear explanation of purpose and theoretical/computational context.
  - `@param <arg>`: Must document *every* argument in the function signature. Never leave an argument undocumented.
  - `@return`: Precise description of the returned object type and structure.
  - `@export`: Present on all public functions.
- **Internal Functions**:
  - Non-exported helper functions must be tagged with `#' @keywords internal`.
  - **Never** place `#' @export` on internal helpers or objective factory functions (`make_*_objective`), as R CMD check will flag them as undocumented user objects.
- **Cross-References**:
  - Use code backticks (e.g., `` `spatstat.linnet::integral.linfun()` ``) instead of fragile cross-package S3 method Rd links (e.g., avoid `[pkg:method]{pkg::method()}`).

---

## 4. Python Bridge Safety (`reticulate`)

1. **Explicit Initialization**:
   - Always invoke `configure_gbpp_python()` before attempting to import any Python module (`xgboost`, `lightgbm`, `catboost`) or calling custom Python scripts.
   - For tests, always use `skip_if_not(reticulate::py_available(initialize = TRUE))` after `configure_gbpp_python()`.
2. **Cross-Platform Path Resolution**:
   - Detect virtual environments using `.venv/Scripts/python.exe` on Windows and `.venv/bin/python` on Linux/macOS.
   - Support upward directory traversal to locate parent virtual environments without crashing.
3. **Session Immutability**:
   - Once Python is initialized in an R process, `reticulate` cannot switch Python binaries. Never call `use_python()` or `use_virtualenv()` if `reticulate::py_available()` is already `TRUE`.
4. **Data Conversion**:
   - Ensure feature matrices passed to Python DMatrix/Dataset/Pool are strictly numeric matrices: `as.matrix(data$X)` with `is.numeric() == TRUE`.

---

## 5. Mathematical Formulations & Numerical Stability

1. **Numerical Clamping**:
   - Always clamp raw predictions before exponentiation to avoid floating-point overflow (`Inf`) or underflow (`0.0`):
     ```r
     phi <- pmin(pmax(prediction, -50), 50)
     lambda <- exp(phi)
     ```
   - Python implementations must similarly use `np.clip(preds, -50.0, 50.0)`.
2. **Baseline Offset ($\eta_0$)**:
   - For homogeneous Poisson initialization, $\eta_0 = \log(\bar{\lambda}) = \log\left(\frac{\sum y_i}{\sum w_i}\right)$.
   - Only apply $\eta_0$ as the default baseline for `"poisson"` and `"weighted_poisson"`.
   - For `"logistic"`, `"weighted_logistic"`, `"iterative_poisson"`, and `"iterative_logistic"`, default baseline is `0.0`.
3. **Iterative Weights Denominator Safety**:
   - In iterative objectives, the denominator is $1 + \lambda(u) I(u)$.
   - When the spatial interaction is inhibitory ($I(u) < 0$), large $\lambda$ values can force the denominator $\le 0$.
   - The loss layer must check `1.0 + lam * interaction_integral <= 0.0` and raise an informative error rather than producing `NaN` or `Inf`.
