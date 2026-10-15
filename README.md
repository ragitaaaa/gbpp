# gbpp: Gradient Boosted Point Processes on Planar and Linear Network Domains

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)]()
[![CRAN status](https://img.shields.io/badge/CRAN-pending-orange.svg)]()
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20macOS%20%7C%20Linux-lightgrey.svg)]()

`gbpp` is an R package for fitting **gradient boosted decision tree models** to spatial point process data. It bridges spatial point process theory—traditionally implemented using parametric log-linear models in `spatstat`—with high-performance gradient boosting backends (**XGBoost**, **LightGBM**, and **CatBoost**).

The package supports both **two-dimensional planar Euclidean domains** ($\mathbb{R}^2$) and **one-dimensional linear road networks** ($\mathcal{L} \subset \mathbb{R}^2$), with native spatial interpretability via TreeSHAP.

---

## Key Features

- **Multi-Engine Boosting Backends:** Swappable interfaces for **XGBoost**, **LightGBM**, and **CatBoost** (with native categorical feature support).
- **Dual Spatial Domains:** Unified modeling for:
  - **Planar Point Processes** (`spatstat.geom` / `spatstat.explore`)
  - **Linear Network Point Processes** (`spatstat.linnet`)
- **Six Point-Process Likelihood Objectives:**
  1. *Standard Poisson Likelihood* (Berman-Turner quadrature)
  2. *Nonparametrically Weighted Poisson Likelihood* (Choiruddin et al., 2026)
  3. *Logistic Point Process Likelihood* (Baddeley et al., 2014)
  4. *Weighted Logistic Likelihood*
  5. *Iteratively Weighted Poisson Likelihood* (Ripley's $K$ / linear PCF interaction adjusted)
  6. *Iteratively Weighted Logistic Likelihood*
- **Spatial Validation & Interpretation:**
  - *Independent Stochastic Thinning Cross-Validation* (`cv_gbpp`) that preserves spatial quadrature geometry.
  - *Domain-Aware TreeSHAP Explanations* (`explain_gbpp`) referenced to background spatial dummy points.
  - *Continuous Spatial Surface Mapping* directly to `spatstat` pixel images (`im` and `linim`).

---

## Installation & Setup Guide (For Testers)

Follow these simple steps to install and test `gbpp` on your machine (Windows, macOS, or Linux).

### Step 1: Install R Package Prerequisites

Make sure you have R (>= 4.0.0) installed. Open R or RStudio and install `remotes`:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
```

### Step 2: Install `gbpp`

You can install `gbpp` directly from GitHub:

```r
remotes::install_github("ragitaaaa/gbpp")
```

*(Alternatively, if you received the source code as a folder or zip archive, run `remotes::install_local("path/to/gbpp")`)*.

### Step 3: Install Python Dependencies (One-Time Automatic Setup)

`gbpp` uses `reticulate` to connect to high-performance C++ gradient boosting engines. Run the automated installer inside R:

```r
library(gbpp)

# Automatically creates an isolated virtual environment ('r-gbpp') 
# and installs numpy, scipy, xgboost, lightgbm, and catboost:
install_gbpp_python()
```

> **Note:** If you already have Python configured with `xgboost`, `lightgbm`, or `catboost` in another environment, you can point to it directly:
> ```r
> Sys.setenv(GBPP_PYTHON = "/path/to/your/python")
> ```

---

## Quickstart & Testing Walkthrough

Here is a 2-minute test script you can run to verify that `gbpp` is working properly on your device:

### Test 1: Planar 2D Forestry Point Process (`bei` data)

```r
library(gbpp)
library(spatstat.data)

# 1. Load sample dataset from spatstat (tropical tree locations + elevation/slope)
data("bei", package = "spatstat.data")
data("bei.extra", package = "spatstat.data")

# 2. Prepare quadrature data (automated logistic dummy grid)
data_planar <- prepare_gbpp(
  point_pattern = bei,
  covariates = bei.extra,
  nd = 128L
)
print(data_planar)

# 3. Fit a gradient boosted point process model (try 'lightgbm', 'xgboost', or 'catboost')
model_lgb <- fit_gbpp(
  data = data_planar,
  engine = "lightgbm",
  objective = "logistic",
  n_trees = 50L,
  learning_rate = 0.05
)
print(model_lgb)

# 4. Generate and plot continuous spatial intensity surface
intensity_map <- predict(model_lgb, newdata = bei.extra, type = "im")
plot(intensity_map, main = "Predicted Spatial Intensity (Tree Density)")

# 5. Extract domain-aware TreeSHAP feature explanations
shap_res <- explain_gbpp(model_lgb, data = data_planar, background = "quadrature")
cat("Mean absolute SHAP value for elevation:", mean(abs(shap_res$shap[, "elev"])), "\n")
cat("Mean absolute SHAP value for slope:    ", mean(abs(shap_res$shap[, "grad"])), "\n")
```

---

### Test 2: Linear Network Road Traffic Point Process (`simplenet` data)

```r
library(gbpp)
library(spatstat.geom)
library(spatstat.linnet)

# 1. Create a synthetic linear network point pattern
set.seed(42)
net <- spatstat.data::simplenet
pts <- spatstat.random::runiflpp(30, net)

# 2. Add a synthetic network covariate (e.g., coordinate-based function)
dist_cov <- spatstat.linnet::linfun(function(x, y, seg, tp) { x + 2 * y }, net)
covs <- list(risk_factor = dist_cov)

# 3. Prepare network quadrature data
data_net <- prepare_gbpp_network(
  point_pattern = pts,
  covariates = covs,
  nd = 100L
)
print(data_net)

# 4. Fit network model with CatBoost
model_cat <- fit_gbpp(
  data = data_net,
  engine = "catboost",
  objective = "poisson",
  n_trees = 30L
)
print(model_cat)

# 5. Predict and map onto linear network
lin_intensity <- predict(model_cat, newdata = covs, type = "linim")
plot(lin_intensity, main = "Predicted Network Intensity on Road Network")
```

---

## Feedback Checklist for Testers

If you are testing this package on your device, please let us know:
1. **Operating System:** Windows (10/11), macOS (Intel or Apple Silicon M1/M2/M3), or Linux (Ubuntu, Debian, Fedora, etc.).
2. **Installation:** Did `remotes::install_github()` and `install_gbpp_python()` execute cleanly without errors?
3. **Engines Tested:** Did `fit_gbpp()` work with `lightgbm`, `xgboost`, and `catboost`?
4. **Any Warnings / Issues:** Please report any error messages or unexpected behavior by opening an issue on GitHub or emailing `ragita.dev@gmail.com`.

---

## Authors & Citation

- **Rendi Andria Gita Putra** (Department of Statistics, Institut Teknologi Sepuluh Nopember)
- **Achmad Choiruddin** (Department of Statistics, Institut Teknologi Sepuluh Nopember)

If you use `gbpp` in your research, please cite:

```bibtex
@article{gbpp2026,
  title = {gbpp: Gradient Boosted Point Processes on Planar and Linear Network Domains in R},
  author = {Putra, Rendi Andria Gita and Choiruddin, Achmad},
  journal = {The R Journal},
  year = {2026},
  note = {Under Review}
}
```

## License

This package is licensed under the [MIT License](LICENSE).
