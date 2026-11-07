# gbpp: Gradient Boosted Point Processes on Planar and Linear Network Domains

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)]()
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20macOS%20%7C%20Linux-lightgrey.svg)]()

`gbpp` is an R package for fitting gradient boosted decision trees (GBDT) to spatial point process data. It integrates spatial point process methodology with gradient boosting backends (**LightGBM**, **CatBoost**, and **XGBoost**).

The package supports both two-dimensional planar Euclidean domains ($\mathbb{R}^2$) and one-dimensional linear networks ($\mathcal{L} \subset \mathbb{R}^2$, such as street or river networks), alongside spatial cross-validation via independent stochastic thinning and TreeSHAP-based model explanations.

---

## Table of Contents

- [Key Features](#key-features)
- [Installation & Python Setup](#installation--python-setup)
  - [Step 1: Install R Package](#step-1-install-r-package)
  - [Step 2: Automated Python Environment Configuration](#step-2-automated-python-environment-configuration)
  - [Step 3: Verification & Diagnostics](#step-3-verification--diagnostics)
- [Step-by-Step Usage Guide](#step-by-step-usage-guide)
  - [Workflow 1: 2D Planar Point Processes](#workflow-1-2d-planar-point-processes)
  - [Workflow 2: Evaluating Fit Metrics & Calibration](#workflow-2-evaluating-fit-metrics--calibration)
  - [Workflow 3: Weighted & Iterative Objectives (Handling Clustered Data)](#workflow-3-weighted--iterative-objectives-handling-clustered-data)
  - [Workflow 4: 1D Linear Road Networks](#workflow-4-1d-linear-road-networks)
  - [Workflow 5: Spatial Cross-Validation via Stochastic Thinning](#workflow-5-spatial-cross-validation-via-stochastic-thinning)
  - [Workflow 6: Model Interpretability (TreeSHAP & Partial Dependence)](#workflow-6-model-interpretability-treeshap--partial-dependence)
- [How to Update the Package](#how-to-update-the-package)
- [API Reference Summary](#api-reference-summary)
- [License](#license)

---

## Key Features

- **Gradient Boosting Backends:** Unified interface for **LightGBM**, **CatBoost**, and **XGBoost**.
- **Spatial Domains:** Support for planar point patterns (`ppp` from `spatstat.geom`) and linear network point patterns (`lpp` from `spatstat.linnet`).
- **Point Process Likelihood Objectives:**
  - **Poisson loss:** Standard Poisson composite likelihood and iteratively weighted Poisson loss (based on Lu et al.).
  - **Logistic loss:** Standard logistic composite likelihood and iteratively weighted logistic loss (based on Choiruddin et al.).
- **Validation & Diagnostics:** Spatial cross-validation via independent stochastic thinning (`cv_gbpp`) that preserves the spatial quadrature structure.
- **Model Interpretation:** TreeSHAP feature attributions and dependence plots (`explain_gbpp`) evaluated across spatial background quadrature points.
- **Spatial Predictions:** Direct generation and visualization of continuous spatial intensity surfaces as `spatstat` pixel images (`im` and `linim`).

---

## Installation & Python Setup

`gbpp` interfaces with LightGBM, CatBoost, and XGBoost via `reticulate`.

### Step 1: Install R Package

Ensure you have R (>= 4.0.0) installed. Install `gbpp` from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ragitaaaa/gbpp")
```

*(Or from a local `.tar.gz` bundle: `install.packages("gbpp_0.1.0.tar.gz", repos = NULL, type = "source")`)*.

### Step 2: Automated Python Environment Configuration

Run the automated Python configuration function in R. This creates an isolated virtual environment (`r-gbpp`) containing `numpy`, `scipy`, `xgboost`, `lightgbm`, and `catboost`:

```r
library(gbpp)

# Automated setup (creates isolated virtualenv 'r-gbpp')
install_gbpp_python()
```

#### Custom or Pre-Existing Python Environments

If you already have a Python virtual environment or Conda environment with the required boosting libraries installed, specify it before loading `gbpp`:

```r
# Option A: Set environment variable
Sys.setenv(GBPP_PYTHON = "/path/to/your/python")

# Option B: Use reticulate directly
reticulate::use_virtualenv("my-spatial-env", required = TRUE)
```

### Step 3: Verification & Diagnostics

Verify that your Python backend and boosting engines are properly configured:

```r
library(gbpp)

# Check backend status and installed engine versions
check_gbpp_python()
```

---

## Step-by-Step Usage Guide

### Workflow 1: 2D Planar Point Processes

This example models the spatial distribution of *Beilschmiedia pendula* trees from the `bei` dataset in Barro Colorado Island using elevation and slope gradient covariates.

#### 1. Load Point Pattern and Covariates

```r
library(gbpp)
library(spatstat.geom)
library(spatstat.data)

# Load observed tree coordinates and terrain pixel images
# Note: data(bei) automatically loads both 'bei' and 'bei.extra'
data(bei, package = "spatstat.data")
```

#### 2. Prepare Quadrature Scheme (`prepare_gbpp`)

`prepare_gbpp()` samples continuous pixel covariates at event locations and dummy grid points, assigns integration weights, and returns a standardized `gbpp_data` object:

```r
# Construct logistic quadrature with dummy grid
planar_data <- prepare_gbpp(
  point_pattern = bei,
  covariates = bei.extra,
  nd = 128L,                          # Grid resolution (128 x 128)
  quadrature_method = "logistic"      # "logistic" or "berman_turner"
)

print(planar_data)
```

#### 3. Fit Model (`fit_gbpp`)

Train a gradient boosted point process model using your choice of engine and objective:

```r
# Fit model using LightGBM with logistic loss
fit_planar <- fit_gbpp(
  data = planar_data,
  engine = "lightgbm",
  objective = "logistic",
  n_trees = 150L,
  learning_rate = 0.05,
  max_depth = 5L,
  subsample = 0.8,
  colsample_bytree = 0.8,
  random_seed = 42L
)

print(fit_planar)
```

#### 4. Predict and Map Continuous Intensity Surface

Evaluate the estimated spatial intensity surface $\hat{\lambda}(u)$ across the observation window as a `spatstat` pixel image (`type = "im"`):

```r
# Predict spatial intensity image
pred_im <- predict(fit_planar, newdata = bei.extra, type = "im")

# Plot estimated intensity surface with observed events superimposed
plot(pred_im, main = "Estimated Intensity: Beilschmiedia pendula (LightGBM)")
plot(bei, add = TRUE, pch = ".", col = rgb(0, 0, 0, 0.25))
```

---

### Workflow 2: Evaluating Fit Metrics & Calibration

After fitting a point process model, you can evaluate goodness-of-fit metrics (log-likelihood, deviance) and global calibration:

```r
# 1. Predict intensity at quadrature locations
pred_intensity <- predict(fit_planar, newdata = planar_data$X, type = "intensity")

# 2. Compute spatial Poisson log-likelihood
loglik_val <- metric_poisson_loglik(
  pred_intensity = pred_intensity,
  y = planar_data$y,
  w = planar_data$quadrature_weight
)

# 3. Compute spatial Poisson deviance
deviance_val <- metric_poisson_deviance(
  pred_intensity = pred_intensity,
  y = planar_data$y,
  w = planar_data$quadrature_weight
)

# 4. Global calibration check: integral of intensity vs actual event count
expected_n <- sum(planar_data$quadrature_weight * pred_intensity)
actual_n <- sum(planar_data$y == 1)

cat(sprintf("Spatial Poisson Log-Likelihood : %.2f\n", loglik_val))
cat(sprintf("Spatial Poisson Deviance       : %.2f\n", deviance_val))
cat(sprintf("Expected Abundance (Integral)  : %.1f (Actual: %d, Diff: %+.1f)\n", 
            expected_n, actual_n, expected_n - actual_n))
```

---

### Workflow 3: Weighted & Iterative Objectives (Handling Clustered Data)

Standard point process likelihood can overfit when events exhibit spatial clustering (e.g., Cox or Neyman-Scott processes) because point aggregation is conflated with covariate effects. `gbpp` provides two principled weighting frameworks:

#### Option A: Dynamic Iterative Weights (Lu et al., 2025)

Dynamically updates variance-stabilizing Godambe weights at every boosting iteration based on Ripley's $K$-function interaction integral $A = K(r_{\max}) - \pi r_{\max}^2$:

```r
# 1. Prepare data with Berman-Turner quadrature
data_iter <- prepare_gbpp(
  point_pattern = bei,
  covariates = bei.extra,
  nd = 128L,
  quadrature_method = "berman_turner"
)

# 2. Attach spatial interaction integral (rmax = 20 meters)
data_iter <- attach_interaction_integral(
  data = data_iter,
  point_pattern = bei,
  rmax = 20
)

# 3. Fit with iteratively weighted Poisson objective
fit_iter <- fit_gbpp(
  data = data_iter,
  engine = "xgboost",
  objective = "iterative_poisson",
  n_trees = 100L,
  learning_rate = 0.05,
  max_depth = 4L
)
```

#### Option B: Custom / Local LISA Weights (D'Angelo et al., 2024, 2026)

You can supply arbitrary custom weights (e.g., pilot kernel weights from Choiruddin et al. 2026, or local second-order LISA weights from D'Angelo et al. 2024, 2026) via `attach_statistical_weight()`:

```r
library(spatstat.explore)

# 1. Compute pilot intensity via kernel estimation
pilot_lam <- density.ppp(bei, sigma = 30)

# 2. Compute Local Inhomogeneous K-function (LISA statistic) for each tree
lk <- localKinhom(bei, lambda = pilot_lam, rvalue = 20, verbose = FALSE)

# 3. Derive local Godambe weights at tree locations
A_local <- pmax(0, lk - pi * 20^2)
lam_trees <- interp.im(pilot_lam, x = bei$x, y = bei$y)
w_trees <- 1 / (1 + lam_trees * A_local)

# 4. Spatially smooth local weights across the window using Nadaraya-Watson regression
w_im <- Smooth.ppp(ppp(bei$x, bei$y, window = Window(bei), marks = w_trees), sigma = 30)
w_quad <- interp.im(w_im, x = data_iter$locations$x, y = data_iter$locations$y)
w_quad <- w_quad / mean(w_quad)  # Normalize so mean weight = 1

# 5. Attach custom statistical weights and fit with weighted_poisson
data_local <- attach_statistical_weight(data_iter, w_quad)

fit_local <- fit_gbpp(
  data = data_local,
  engine = "xgboost",
  objective = "weighted_poisson",
  n_trees = 100L,
  learning_rate = 0.05,
  max_depth = 4L
)
```

---

### Workflow 4: 1D Linear Road Networks

In network-constrained settings, events occur along street or river segments rather than across open 2D space. `gbpp` supports linear networks via `spatstat.linnet`.

#### 1. Load Linear Network Point Pattern

```r
library(gbpp)
library(spatstat.geom)
library(spatstat.linnet)
library(spatstat.data)

# Load Chicago street crimes dataset (lpp object)
data(chicago, package = "spatstat.data")

# Create network covariates (e.g., spatial coordinates along network)
net <- as.linnet(chicago)
x_coords <- as.linim(linfun(function(x, y, seg, tp) { x }, net))
y_coords <- as.linim(linfun(function(x, y, seg, tp) { y }, net))
net_covs <- list(x_coord = x_coords, y_coord = y_coords)
```

#### 2. Prepare Linear Network Quadrature (`prepare_gbpp_network`)

`prepare_gbpp_network()` discretizes network segments into subintervals, places dummy quadrature points, and computes segment integration lengths:

```r
# Prepare linear network quadrature
net_data <- prepare_gbpp_network(
  point_pattern = chicago,
  covariates = net_covs,
  nd = 100L                          # Subinterval discretization frequency
)

print(net_data)
```

#### 3. Fit Model with CatBoost Backend

```r
# Fit Poisson composite likelihood model on network
fit_net <- fit_gbpp(
  data = net_data,
  engine = "catboost",
  objective = "poisson",
  n_trees = 100L,
  learning_rate = 0.04,
  max_depth = 5L
)

print(fit_net)
```

#### 4. Predict Network-Constrained Intensity Surface (`linim`)

```r
# Generate linear network pixel image (linim)
pred_linim <- predict(fit_net, newdata = net_covs, type = "linim")

# Visualize intensity along street network segments
plot(pred_linim, main = "Estimated Street Crime Intensity on Chicago Network")
plot(as.ppp(chicago), add = TRUE, pch = 16, cex = 0.4, col = "red")
```

---

### Workflow 5: Spatial Cross-Validation via Stochastic Thinning

Standard cross-validation partitions points at random, which can distort spatial integration weights. `gbpp` implements independent stochastic thinning cross-validation:

- Observed events are thinned independently into validation and training sets.
- Quadrature dummy points are shared across folds with intensity appropriately rescaled, maintaining spatial quadrature validity.

```r
# Perform 5-fold stochastic thinning cross-validation
cv_results <- cv_gbpp(
  data = planar_data,
  engine = "lightgbm",
  objective = "logistic",
  folds = 5L,
  nrounds = 100L,
  params = list(
    learning_rate = 0.05,
    max_depth = 5L
  )
)

# View cross-validated deviance and log-likelihood metrics
print(cv_results)

# Visualize fold balance across spatial observation domain
plot(cv_results)
```

---

### Workflow 6: Model Interpretability (TreeSHAP & Partial Dependence)

`explain_gbpp()` calculates TreeSHAP feature attributions referenced against the background spatial quadrature scheme.

#### 1. Compute TreeSHAP Values

```r
# Extract TreeSHAP feature attributions
shap_output <- explain_gbpp(
  object = fit_planar,
  data = planar_data,
  background = "quadrature"
)

# Summary of mean absolute SHAP attributions
print(shap_output)
```

#### 2. Plot Feature Importance

```r
# Barplot of mean absolute SHAP values across spatial predictors
plot(shap_output, type = "importance")
```

#### 3. Partial Dependence Profiles

Inspect covariate effects on the point process intensity scale:

```r
# Plot partial dependence curve for elevation
plot(shap_output, type = "dependence", feature = "elev")

# Plot partial dependence curve for slope gradient
plot(shap_output, type = "dependence", feature = "grad")
```

---

## How to Update the Package

If you or a collaborator already have an earlier version of `gbpp` installed, you can update to the latest version directly from GitHub using `remotes`:

```r
# Force re-installation of the latest version from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ragitaaaa/gbpp", force = TRUE, upgrade = "never")
```

*(Note: If updating in an active R session where `gbpp` or `reticulate` is already loaded, restart R first (`Ctrl+Shift+F10` in RStudio or `.rs.restartR()`) so dynamic libraries can be cleanly overwritten).*

---

## API Reference Summary

| Function | Primary Purpose | Key Arguments |
|---|---|---|
| `install_gbpp_python()` | Automated virtualenv setup | `envname = "r-gbpp"` |
| `check_gbpp_python()` | Inspect installed Python engine versions | *(None)* |
| `prepare_gbpp()` | Construct 2D planar quadrature scheme | `point_pattern`, `covariates`, `nd`, `quadrature_method`, `statistical_weight` |
| `prepare_gbpp_network()` | Construct 1D linear network quadrature | `point_pattern`, `covariates`, `nd`, `statistical_weight` |
| `attach_statistical_weight()` | Attach custom or local LISA weights | `data`, `weights` |
| `attach_interaction_integral()` | Attach Ripley's K interaction integral for iterative objectives | `data`, `point_pattern`, `rmax` |
| `fit_gbpp()` | Fit gradient boosted point process model | `data`, `engine`, `objective`, `n_trees`, `learning_rate`, `max_depth` |
| `predict.gbpp_model()` | Predict intensity values or spatial pixel images | `object`, `newdata`, `type = c("intensity", "link", "im", "linim")` |
| `metric_poisson_loglik()` | Calculate spatial Poisson log-likelihood | `pred_intensity`, `y`, `w`, `p_scale` |
| `metric_poisson_deviance()` | Calculate spatial Poisson deviance | `pred_intensity`, `y`, `w`, `p_scale` |
| `cv_gbpp()` | Spatial stochastic thinning cross-validation | `data`, `engine`, `params`, `nrounds`, `folds`, `objective` |
| `explain_gbpp()` | TreeSHAP feature attributions | `object`, `data`, `background` |
| `plot.gbpp_explanation()` | Visualize TreeSHAP importance or dependence curves | `x`, `type = c("importance", "dependence")`, `feature` |
| `plot.gbpp_cv()` | Visualize stochastic thinning validation folds | `x`, `separate` |
| `cv_gbpp()` | Spatial stochastic thinning cross-validation | `data`, `engine`, `params`, `nrounds`, `folds`, `objective` |
| `explain_gbpp()` | TreeSHAP feature attributions | `object`, `data`, `background` |
| `plot.gbpp_explanation()` | Visualize TreeSHAP importance or dependence curves | `x`, `type = c("importance", "dependence")`, `feature` |
| `plot.gbpp_cv()` | Visualize stochastic thinning validation folds | `x`, `separate` |

---

## License

This package is open-source software licensed under the [MIT License](LICENSE).
