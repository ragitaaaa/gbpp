# gbpp: Gradient Boosted Point Processes on Planar and Linear Network Domains

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)]()
[![CRAN status](https://img.shields.io/badge/CRAN-pending-orange.svg)]()
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20macOS%20%7C%20Linux-lightgrey.svg)]()

`gbpp` is an R package for fitting **gradient boosted decision trees (GBDT)** to spatial point process data. It bridges spatial point process theory—traditionally restricted to parametric log-linear models (`ppm`/`lppm` in `spatstat`) or low-dimensional kernel smoothers—with high-performance gradient boosting backends (**LightGBM**, **CatBoost**, and **XGBoost**).

The package supports both **two-dimensional planar Euclidean domains** ($\mathbb{R}^2$) and **one-dimensional linear network graphs** ($\mathcal{L} \subset \mathbb{R}^2$, such as road and river networks), complete with native spatial cross-validation via independent stochastic thinning and domain-aware TreeSHAP interpretability.

---

## Table of Contents

- [Key Features](#key-features)
- [Architecture & Likelihood Formulations](#architecture--likelihood-formulations)
- [Installation & Python Setup](#installation--python-setup)
  - [Step 1: Install R Package](#step-1-install-r-package)
  - [Step 2: Automated Python Environment Configuration](#step-2-automated-python-environment-configuration)
  - [Step 3: Verification & Diagnostics](#step-3-verification--diagnostics)
- [Step-by-Step Usage Guide](#step-by-step-usage-guide)
  - [Workflow 1: 2D Planar Point Processes (Forest Ecology)](#workflow-1-2d-planar-point-processes-forest-ecology)
  - [Workflow 2: 1D Linear Road Networks (Network Crimes & Traffic Safety)](#workflow-2-1d-linear-road-networks-network-crimes--traffic-safety)
  - [Workflow 3: Spatial Cross-Validation via Stochastic Thinning](#workflow-3-spatial-cross-validation-via-stochastic-thinning)
  - [Workflow 4: Model Interpretability (TreeSHAP & Partial Dependence)](#workflow-4-model-interpretability-treeshap--partial-dependence)
- [Benchmark & Empirical Guidance](#benchmark--empirical-guidance)
- [API Reference Summary](#api-reference-summary)
- [Citation](#citation)
- [License](#license)

---

## Key Features

- **Multi-Engine Boosting Backends:** Swappable interfaces for **LightGBM** (fastest Pareto-dominant engine), **CatBoost** (native ordered target statistics for high-cardinality factors), and **XGBoost**.
- **Dual Spatial Domains:** Unified modeling for:
  - **Planar Point Processes** (`spatstat.geom` / `spatstat.explore`)
  - **Linear Network Point Processes** (`spatstat.linnet`)
- **Six Point-Process Likelihood Objectives:**
  1. *Standard Poisson Composite Likelihood* (Berman–Turner quadrature)
  2. *Baddeley–Turner Logistic Composite Likelihood* (Bernoulli dummy superposition)
  3. *Nonparametrically Weighted Poisson Likelihood* (Choiruddin et al., 2026)
  4. *Weighted Logistic Likelihood*
  5. *Iteratively Reweighted Poisson Likelihood* (Ripley's $K$ / linear PCF interaction adjusted)
  6. *Iteratively Reweighted Logistic Likelihood*
- **Spatial Diagnostics & Interpretation:**
  - *Stochastic Thinning Cross-Validation* (`cv_gbpp`) preserving spatial quadrature geometry.
  - *Domain-Aware TreeSHAP Explanations* (`explain_gbpp`) referenced against spatial dummy backgrounds.
  - *Continuous Spatial Surface Mapping* directly to `spatstat` pixel images (`im` and `linim`).

---

## Architecture & Likelihood Formulations

A spatial point pattern consists solely of event coordinates without explicit negative (absence) observations. `gbpp` discretizes continuous observation windows into a valid numerical quadrature scheme $\{(u_i, y_i, v_i)\}_{i=1}^{n+m}$ composed of $n$ observed events ($y_i = 1$) and $m$ dummy quadrature points ($y_i = 0$) with integration weights $v_i > 0$.

During boosting, the ensemble of $K$ decision trees minimizes point process surrogate loss functions:

$$\log \lambda(z(u)) = \sum_{k=1}^K f_k(z(u))$$

1. **Berman–Turner Poisson Objective:** Discretizes the continuous spatial Poisson log-likelihood into a weighted Poisson counting deviance:
   $$\mathcal{L}_p(\phi) = \sum_{i=1}^{n+m} [ v_i \exp(\phi_i) - y_i \phi_i ] \quad \implies \quad g_i = v_i \exp(\phi_i) - y_i, \quad h_i = v_i \exp(\phi_i)$$
2. **Baddeley–Turner Logistic Objective:** Employs independent Poisson dummy superposition with intensity $\delta(u) > 0$, converting intensity estimation into a binary Bernoulli classification problem:
   $$\pi(u_i) = \frac{\exp(\phi_i)}{\exp(\phi_i) + \delta(u_i)} \quad \implies \quad g_i = \pi(u_i) - y_i, \quad h_i = \pi(u_i)(1 - \pi(u_i))$$
3. **Iterative Second-Order Reweighting:** For clustered point processes (e.g., Thomas cluster or Log-Gaussian Cox processes), observation weights are updated iteratively using Ripley's $K$-function or pair correlation function ($g(r)$) to eliminate variance inflation induced by latent clustering.

---

## Installation & Python Setup

`gbpp` interfaces directly with the official C++ implementations of LightGBM, CatBoost, and XGBoost via `reticulate`.

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

Run the automated one-time Python configuration inside R. This creates an isolated virtual environment (`r-gbpp`) containing `numpy`, `scipy`, `xgboost`, `lightgbm`, and `catboost`:

```r
library(gbpp)

# Automated setup (creates isolated virtualenv 'r-gbpp')
install_gbpp_python()
```

#### Custom or Pre-Existing Python Environments

If you already manage a Python virtual environment or conda environment containing the boosting engines, specify it before loading `gbpp`:

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

### Workflow 1: 2D Planar Point Processes (Forest Ecology)

This example models the spatial distribution of 3,604 *Beilschmiedia pendula* trees on Barro Colorado Island using terrain elevation and gradient covariates.

#### 1. Load Point Pattern and Covariates

```r
library(gbpp)
library(spatstat.geom)
library(spatstat.data)

# Load observed tree coordinates and terrain pixel images
data(bei, package = "spatstat.data")
data(bei.extra, package = "spatstat.data")

# bei is a spatstat 'ppp' object
# bei.extra contains two 'im' objects: elev (elevation) and grad (slope gradient)
```

#### 2. Prepare Quadrature Scheme (`prepare_gbpp`)

`prepare_gbpp()` automatically samples continuous pixel covariates at event locations and dummy grid points, assigns integration weights, and returns a standardized `gbpp_data` object:

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

#### 3. Fit Gradient Boosted Point Process Model (`fit_gbpp`)

Train a model using your choice of engine (`"lightgbm"`, `"catboost"`, or `"xgboost"`) and objective:

```r
# Fit model using the fast LightGBM engine with logistic loss
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

Evaluate the estimated first-order spatial intensity surface $\hat{\lambda}(u)$ across the entire observation window as a `spatstat` pixel image (`type = "im"`):

```r
# Predict full spatial intensity image
pred_im <- predict(fit_planar, newdata = bei.extra, type = "im")

# Plot estimated intensity surface with observed events superimposed
plot(pred_im, main = "Estimated Intensity: Beilschmiedia pendula (LightGBM)")
plot(bei, add = TRUE, pch = ".", col = rgb(0, 0, 0, 0.25))
```

---

### Workflow 2: 1D Linear Road Networks (Network Crimes & Traffic Safety)

In traffic safety and street criminology, events occur strictly along 1D street segments rather than open 2D space. `gbpp` natively supports linear networks (`spatstat.linnet`).

#### 1. Load Linear Network Point Pattern

```r
library(gbpp)
library(spatstat.geom)
library(spatstat.linnet)
library(spatstat.data)

# Load Chicago street crimes dataset (lpp object)
data(chicago, package = "spatstat.data")

# Create network covariates (e.g. Euclidean distance coordinates or network functions)
net <- as.linnet(chicago)
x_coords <- linfun(function(x, y, seg, tp) { x }, net)
y_coords <- linfun(function(x, y, seg, tp) { y }, net)
net_covs <- list(x_coord = x_coords, y_coord = y_coords)
```

#### 2. Prepare Linear Network Quadrature (`prepare_gbpp_network`)

`prepare_gbpp_network()` divides network segments into equal-length subintervals of length $\Delta$, places dummy points at subsegment midpoints, and computes geodesic subsegment integration lengths:

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

# Visualize intensity along road segments
plot(pred_linim, main = "Estimated Street Crime Intensity on Chicago Network")
plot(as.ppp(chicago), add = TRUE, pch = 16, cex = 0.4, col = "red")
```

---

### Workflow 3: Spatial Cross-Validation via Stochastic Thinning

Standard naive $K$-fold cross-validation randomly partitions points, which distorts the spatial integration geometry. `gbpp` implements **independent stochastic thinning cross-validation**:

- Observed events are thinned independently with probability $p = 1/K$ for validation and $1 - p$ for training.
- Dummy quadrature points are shared across folds with intensity appropriately rescaled, maintaining exact spatial integration validity.

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

# View cross-validated validation deviance per iteration
print(cv_results)
cat("Optimal boosting rounds:", cv_results$best_iteration, "\n")
```

---

### Workflow 4: Model Interpretability (TreeSHAP & Partial Dependence)

Tree-based point processes provide post-hoc interpretability through `explain_gbpp()`, calculating exact TreeSHAP feature attributions referenced against the background spatial quadrature scheme.

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

#### 2. Plot Global Feature Importance

```r
# Barplot of mean absolute SHAP values across spatial predictors
plot(shap_output, type = "importance")
```

#### 3. Partial Dependence Profiles

Inspect non-linear thresholds and marginal effects directly on the original covariate scales:

```r
# Plot marginal partial dependence function for elevation
plot(shap_output, type = "dependence", feature = "elev")

# Plot marginal partial dependence function for slope gradient
plot(shap_output, type = "dependence", feature = "grad")
```

---

## Benchmark & Empirical Guidance

Based on extensive Monte Carlo simulation experiments comprising **12,000 model fits** across planar continuous, categorical, high-cardinality, and empirical linear road network domains:

| Scenario / Goal | Recommended Engine | Recommended Objective | Rationale |
|---|---|---|---|
| **Fast Exploratory Analysis & Cross-Validation** | `lightgbm` | `poisson` or `logistic` | LightGBM exhibits **strict Pareto dominance in speed** (~0.036s/fit), running 45x to 76x faster than CatBoost. |
| **High-Cardinality Categorical Features** (e.g. > 8 levels: land use, soil taxonomy, administrative units) | `catboost` | `poisson` | CatBoost's **native ordered target statistics** achieve 9.2x to 16.7x lower error than integer label encoding used by LightGBM/XGBoost. |
| **1D Linear Road Networks** | `lightgbm` or `catboost` | `poisson` | Volume-weighted Poisson loss provides superior numerical stability and scaling on network graph topologies ($9.57 \times 10^{-9}$ median MISE). |
| **Clustered / Aggregated Point Patterns** (e.g., Thomas cluster or LGCP) | `lightgbm` | `iterative_poisson` | Iterative second-order reweighting via Ripley's $K$ attenuates variance inflation caused by spatial clustering. |

---

## API Reference Summary

| Function | Primary Purpose | Key Arguments |
|---|---|---|
| `install_gbpp_python()` | Automated virtualenv setup | `envname = "r-gbpp"` |
| `check_gbpp_python()` | Inspect installed Python engine versions | *(None)* |
| `prepare_gbpp()` | Construct 2D planar quadrature scheme | `point_pattern`, `covariates`, `nd`, `quadrature_method` |
| `prepare_gbpp_network()` | Construct 1D linear network quadrature | `point_pattern`, `covariates`, `nd` |
| `calculate_planar_dummy_intensity()` | Calculate logistic dummy intensity $\delta(u)$ | `point_pattern`, `nd`, `dummytype` |
| `fit_gbpp()` | Fit gradient boosted point process ensemble | `data`, `engine`, `objective`, `n_trees`, `learning_rate`, `max_depth` |
| `predict.gbpp_model()` | Predict intensity values or spatial pixel images | `object`, `newdata`, `type = c("intensity", "link", "im", "linim")` |
| `cv_gbpp()` | Spatial stochastic thinning cross-validation | `data`, `engine`, `params`, `nrounds`, `folds` |
| `explain_gbpp()` | TreeSHAP feature attributions and dependence | `object`, `data`, `background` |

---

## Citation

If you use `gbpp` in your research, please cite:

```bibtex
@article{gbpp2026,
  title = {Gbpp: Fast Gradient Boosted Point Process Modeling on Planar and Linear Network Domains in R},
  author = {Putra, Rendi Andria Gita and Choiruddin, Achmad},
  journal = {The R Journal},
  year = {2026},
  note = {Under Review}
}
```

---

## License

This package is open-source software licensed under the [MIT License](LICENSE).
