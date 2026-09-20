"""CatBoost custom objectives for the GBPP package."""

import math

import numpy as np


_CLIP_MIN = -50.0
_CLIP_MAX = 50.0


def _as_float_array(values, name):
    """Convert values to a finite one-dimensional float array."""
    values = np.asarray(values, dtype=float)

    if values.ndim != 1:
        raise ValueError(f"{name} must be a one-dimensional array.")

    if not np.all(np.isfinite(values)):
        raise ValueError(f"{name} must contain only finite values.")

    return values


def _check_length(values, n, name):
    """Check that an array has the expected length."""
    if len(values) != n:
        raise ValueError(
            f"{name} must have length {n}, got {len(values)}."
        )


def _safe_exp(value):
    """Exponentiate a value after clipping to a safe numerical range."""
    value = min(max(float(value), _CLIP_MIN), _CLIP_MAX)
    return math.exp(value)


def _get_weights(weights, n):
    """
    Return CatBoost-provided weights.

    CatBoost passes None when no Pool weights are supplied.
    In that case, use unit weights.
    """
    if weights is None:
        return np.ones(n, dtype=float)

    weights = _as_float_array(weights, "weights")
    _check_length(weights, n, "weights")

    if np.any(weights < 0.0):
        raise ValueError("weights must be non-negative.")

    return weights


class PoissonObjective:
    """GBPP ordinary Poisson objective."""

    def __init__(self, quadrature_weight):
        self.quadrature_weight = _as_float_array(
            quadrature_weight,
            "quadrature_weight",
        )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        GBPP derivatives:

            g_i = v_i * exp(phi_i) - y_i
            h_i = v_i * exp(phi_i)

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.quadrature_weight,
            n,
            "quadrature_weight",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])

            gradient = (
                self.quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = (
                self.quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class WeightedPoissonObjective:
    """GBPP fixed weighted Poisson objective."""

    def __init__(
        self,
        quadrature_weight,
        statistical_weight,
    ):
        self.quadrature_weight = _as_float_array(
            quadrature_weight,
            "quadrature_weight",
        )

        self.statistical_weight = _as_float_array(
            statistical_weight,
            "statistical_weight",
        )

        _check_length(
            self.statistical_weight,
            len(self.quadrature_weight),
            "statistical_weight",
        )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        GBPP derivatives:

            g_i = w_i * (v_i * exp(phi_i) - y_i)
            h_i = w_i * v_i * exp(phi_i)

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.quadrature_weight,
            n,
            "quadrature_weight",
        )
        _check_length(
            self.statistical_weight,
            n,
            "statistical_weight",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])
            w = self.statistical_weight[i]

            gradient = w * (
                self.quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = w * (
                self.quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class LogisticObjective:
    """GBPP logistic objective with dummy-process intensity."""

    def __init__(self, dummy_intensity):
        self.dummy_intensity = _as_float_array(
            dummy_intensity,
            "dummy_intensity",
        )

        if np.any(self.dummy_intensity <= 0.0):
            raise ValueError(
                "dummy_intensity must be strictly positive."
            )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        GBPP derivatives:

            pi_i = exp(phi_i) /
                   (exp(phi_i) + delta_i)

            g_i = pi_i - y_i
            h_i = pi_i * (1 - pi_i)

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.dummy_intensity,
            n,
            "dummy_intensity",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            eta = min(
                max(approxes[i], _CLIP_MIN),
                _CLIP_MAX,
            )

            delta = self.dummy_intensity[i]

            pi = 1.0 / (
                1.0
                + math.exp(
                    -(eta - math.log(delta))
                )
            )

            gradient = pi - targets[i]

            hessian = pi * (1.0 - pi)

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class WeightedLogisticObjective:
    """GBPP fixed weighted logistic objective."""

    def __init__(
        self,
        dummy_intensity,
        statistical_weight,
    ):
        self.dummy_intensity = _as_float_array(
            dummy_intensity,
            "dummy_intensity",
        )

        self.statistical_weight = _as_float_array(
            statistical_weight,
            "statistical_weight",
        )

        if np.any(self.dummy_intensity <= 0.0):
            raise ValueError(
                "dummy_intensity must be strictly positive."
            )

        _check_length(
            self.statistical_weight,
            len(self.dummy_intensity),
            "statistical_weight",
        )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        GBPP derivatives:

            g_i = w_i * (pi_i - y_i)
            h_i = w_i * pi_i * (1 - pi_i)

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.dummy_intensity,
            n,
            "dummy_intensity",
        )
        _check_length(
            self.statistical_weight,
            n,
            "statistical_weight",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            eta = min(
                max(approxes[i], _CLIP_MIN),
                _CLIP_MAX,
            )

            delta = self.dummy_intensity[i]

            pi = 1.0 / (
                1.0
                + math.exp(
                    -(eta - math.log(delta))
                )
            )

            w = self.statistical_weight[i]

            gradient = w * (
                pi - targets[i]
            )

            hessian = w * (
                pi * (1.0 - pi)
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class IterativePoissonObjective:
    """GBPP iteratively weighted Poisson objective."""

    def __init__(
        self,
        quadrature_weight,
        interaction_integral,
    ):
        self.quadrature_weight = _as_float_array(
            quadrature_weight,
            "quadrature_weight",
        )

        self.interaction_integral = _as_float_array(
            interaction_integral,
            "interaction_integral",
        )

        _check_length(
            self.interaction_integral,
            len(self.quadrature_weight),
            "interaction_integral",
        )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        Iterative weight:

            w_i = 1 /
                  (1 + lambda_i * A_i)

        GBPP derivatives:

            g_i = w_i * (v_i * lambda_i - y_i)
            h_i = w_i * v_i * lambda_i

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.quadrature_weight,
            n,
            "quadrature_weight",
        )
        _check_length(
            self.interaction_integral,
            n,
            "interaction_integral",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])

            denominator = (
                1.0
                + lam * self.interaction_integral[i]
            )

            if denominator <= 0.0:
                raise ValueError(
                    "Iterative Poisson weight denominator "
                    "must be positive."
                )

            weight = 1.0 / denominator

            gradient = weight * (
                self.quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = weight * (
                self.quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class IterativeLogisticObjective:
    """GBPP iteratively weighted logistic objective."""

    def __init__(
        self,
        dummy_intensity,
        interaction_integral,
    ):
        self.dummy_intensity = _as_float_array(
            dummy_intensity,
            "dummy_intensity",
        )

        self.interaction_integral = _as_float_array(
            interaction_integral,
            "interaction_integral",
        )

        if np.any(self.dummy_intensity <= 0.0):
            raise ValueError(
                "dummy_intensity must be strictly positive."
            )

        _check_length(
            self.interaction_integral,
            len(self.dummy_intensity),
            "interaction_integral",
        )

    def calc_ders_range(self, approxes, targets, weights):
        """
        Calculate CatBoost derivatives.

        Iterative logistic weight:

            w_i =
                (lambda_i + delta_i)
                --------------------------------
                delta_i * (1 + lambda_i * A_i)

        where

            lambda_i = exp(phi_i)

            pi_i =
                lambda_i /
                (lambda_i + delta_i)

        GBPP derivatives:

            g_i = w_i * (pi_i - y_i)
            h_i = w_i * pi_i * (1 - pi_i)

        CatBoost expects (-g_i, -h_i).
        """
        targets = _as_float_array(targets, "targets")
        approxes = _as_float_array(approxes, "approxes")

        n = len(targets)

        _check_length(approxes, n, "approxes")
        _check_length(
            self.dummy_intensity,
            n,
            "dummy_intensity",
        )
        _check_length(
            self.interaction_integral,
            n,
            "interaction_integral",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            eta = min(
                max(approxes[i], _CLIP_MIN),
                _CLIP_MAX,
            )

            delta = self.dummy_intensity[i]

            lam = math.exp(eta)

            denominator = (
                1.0
                + lam * self.interaction_integral[i]
            )

            if denominator <= 0.0:
                raise ValueError(
                    "Iterative Logistic weight denominator "
                    "must be positive."
                )

            weight = (
                (lam + delta)
                / (
                    delta
                    * denominator
                )
            )

            pi = 1.0 / (
                1.0
                + math.exp(
                    -(eta - math.log(delta))
                )
            )

            gradient = weight * (
                pi - targets[i]
            )

            hessian = weight * (
                pi * (1.0 - pi)
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result
