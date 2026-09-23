# Testing Guidelines for `gbpp`

This document defines the testing architecture, numerical precision standards, fixture policies, and validation workflows for `gbpp`. All automated tests must adhere to these guidelines.

---

## 1. 3-Tier Test Suite Architecture

All test files in `tests/testthat/` must be categorized into one of three explicit tiers using file prefix conventions:

### Tier 1: Mathematical Verification (`test-tier1-math-*`)
- **Scope**: Rigorous validation of theoretical point process mathematics and loss functions.
- **Contents**:
  - Direct comparison of Python custom loss layer outputs (gradients, Hessians) against analytical pure R reference implementations (`.gbpp_*_loss_reference`).
  - Berman-Turner quadrature weight calculations and dummy point intensity estimation.
  - Ripley's K-function planar interaction integrals and shortest-path linear network interaction integrals.
  - Iterative Berman-Turner spatial weights.
- **Rule**: Mathematical verification tests must run deterministically without fitting large tree models.

### Tier 2: Fast Engine & Hyperparameter Smoke Tests (`test-tier2-engine-*`, `test-tier2-sys-*`)
- **Scope**: Unit validation of backends, hyperparameter normalizers, and system utilities.
- **Contents**:
  - Parameter mapping and alias resolution via `.gbpp_normalize_params()`.
  - Fast smoke fits (2 to 5 boosting trees) for XGBoost, LightGBM, and CatBoost across all 6 objectives.
  - Python virtual environment discovery and session configuration (`test-tier2-sys-python.R`).
- **Rule**: Keep boosting iterations small (`n_trees = 2L` to `5L`) to ensure the entire Tier 2 suite completes in under 15 seconds.

### Tier 3: End-to-End Integration Pipelines (`test-tier3-api-*`, `test-tier3-data-*`, `test-tier3-pipeline-*`)
- **Scope**: Full data-to-model-to-prediction workflows.
- **Contents**:
  - Unified `fit_gbpp()` generic and S3 `predict.gbpp_model()` type contracts (`intensity`, `link`, `raw`).
  - 100% backward-compatibility tests for all 18 legacy `fit_gbpp_<engine>_<objective>()` shims.
  - Data constructors and validators (`new_gbpp_data()`, `validate_gbpp_data()`).
  - Planar and linear network spatial pipelines using standard `spatstat` datasets (`spatstat.data::bei`, `spatstat.data::chicago`, `simplenet`).

---

## 2. Numerical Assertions & Tolerances

- **Never Use Exact Equality for Floats**:
  Do not use `expect_identical()` or `==` on floating-point vectors, matrix gradients, or model predictions.
- **Standard Tolerance Threshold**:
  Always use `expect_equal(..., tolerance = 1e-6)` for gradient, Hessian, and prediction assertions:
  ```r
  # Correct
  expect_equal(py_gradient, r_reference_gradient, tolerance = 1e-6)

  # Incorrect
  expect_identical(py_gradient, r_reference_gradient)
  ```
- **Mathematical Invariant Checks**:
  Verify analytical relationships explicitly:
  ```r
  # Intensity must equal exponentiated link
  expect_equal(pred_intensity, exp(pred_link), tolerance = 1e-6)

  # Link must equal raw margin + baseline offset
  expect_equal(pred_link, pred_raw + baseline, tolerance = 1e-6)
  ```

---

## 3. Test Isolation & Shared Fixtures

- **Use Lightweight Shared Fixtures**:
  Avoid re-generating heavy quadrature schemes or large spatial point patterns inside individual unit tests.
- **Fixture Helpers**:
  - `make_small_gbpp_data()`: Located in `tests/testthat/helper-fixtures.R`. Supplies a validated, lightweight 8-observation `gbpp_data` fixture with all required attributes.
  - `make_small_planar_fixture()`: Located in `tests/testthat/helper-fixtures.R`. Supplies a minimal 20-point planar pattern on an `owin` unit box with 2 smooth image covariates.
  - `make_network_integration_fixture()`: Located in `tests/testthat/helper-network.R`. Supplies a minimal point pattern on `simplenet` with linear network image covariates.
- **Artifact Cleanup**:
  Engines (such as CatBoost) that create disk artifacts (e.g., `catboost_info/`) must have their directories cleaned up or excluded via `.gitignore` and `.Rbuildignore`.

---

## 4. Verification Workflow for Commits

Before submitting any code change or micro-commit, execute this sequence:

1. **Focused Component Test**:
   ```bash
   Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/<target-test-file>.R')"
   ```
2. **Full Test Suite Execution**:
   ```bash
   Rscript -e "devtools::load_all(); devtools::test()"
   ```
   *Requirement*: 100% pass rate (0 failures).
3. **Roxygen2 Documentation Update**:
   ```bash
   Rscript -e "devtools::document()"
   ```
4. **Full R CMD Check Verification**:
   ```bash
   Rscript -e "devtools::check(document = FALSE, error_on = 'warning')"
   ```
   *Requirement*: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔ | Status: OK`.
