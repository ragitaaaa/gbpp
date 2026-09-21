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


class _BatchObjective:
    """
    Manage sequential batches supplied by CatBoost.

    CatBoost may call calc_ders_range() with only a subset of the
    training observations. The subset is supplied in the original
    observation order. This class maps each batch back to the
    corresponding slice of the full observation-level arrays.
    """

    def __init__(self, n_obs):
        self._n_obs = int(n_obs)
        self._offset = 0

    def _next_batch(self, n):
        """
        Return the starting index of the next CatBoost batch.

        The cursor is reset after a complete pass through all
        observations.
        """
        n = int(n)

        if n < 0:
            raise ValueError("Batch size must be non-negative.")

        if self._offset == self._n_obs:
            self._offset = 0

        if self._offset + n > self._n_obs:
            raise ValueError(
                "CatBoost batch exceeds the stored observation range: "
                f"offset={self._offset}, batch_size={n}, "
                f"n_obs={self._n_obs}."
            )

        start = self._offset
        self._offset += n

        return start

    def _batch_slice(self, values, start, n, name):
        """Return the observation-level values for the current batch."""
        _check_length(values, self._n_obs, name)

        return values[start:start + n]


class PoissonObjective(_BatchObjective):
    """GBPP ordinary Poisson objective."""

    def __init__(self, quadrature_weight):
        self.quadrature_weight = _as_float_array(
            quadrature_weight,
            "quadrature_weight",
        )

        super().__init__(len(self.quadrature_weight))

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

        start = self._next_batch(n)

        quadrature_weight = self._batch_slice(
            self.quadrature_weight,
            start,
            n,
            "quadrature_weight",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])

            gradient = (
                quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = (
                quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class WeightedPoissonObjective(_BatchObjective):
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

        super().__init__(len(self.quadrature_weight))

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

        start = self._next_batch(n)

        quadrature_weight = self._batch_slice(
            self.quadrature_weight,
            start,
            n,
            "quadrature_weight",
        )

        statistical_weight = self._batch_slice(
            self.statistical_weight,
            start,
            n,
            "statistical_weight",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])
            w = statistical_weight[i]

            gradient = w * (
                quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = w * (
                quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class LogisticObjective(_BatchObjective):
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

        super().__init__(len(self.dummy_intensity))

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

        start = self._next_batch(n)

        dummy_intensity = self._batch_slice(
            self.dummy_intensity,
            start,
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

            delta = dummy_intensity[i]

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


class WeightedLogisticObjective(_BatchObjective):
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

        super().__init__(len(self.dummy_intensity))

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

        start = self._next_batch(n)

        dummy_intensity = self._batch_slice(
            self.dummy_intensity,
            start,
            n,
            "dummy_intensity",
        )

        statistical_weight = self._batch_slice(
            self.statistical_weight,
            start,
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

            delta = dummy_intensity[i]

            pi = 1.0 / (
                1.0
                + math.exp(
                    -(eta - math.log(delta))
                )
            )

            w = statistical_weight[i]

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


class IterativePoissonObjective(_BatchObjective):
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

        super().__init__(len(self.quadrature_weight))

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

        start = self._next_batch(n)

        quadrature_weight = self._batch_slice(
            self.quadrature_weight,
            start,
            n,
            "quadrature_weight",
        )

        interaction_integral = self._batch_slice(
            self.interaction_integral,
            start,
            n,
            "interaction_integral",
        )

        catboost_weights = _get_weights(weights, n)

        result = []

        for i in range(n):
            lam = _safe_exp(approxes[i])

            denominator = (
                1.0
                + lam * interaction_integral[i]
            )

            if denominator <= 0.0:
                raise ValueError(
                    "Iterative Poisson weight denominator "
                    "must be positive."
                )

            weight = 1.0 / denominator

            gradient = weight * (
                quadrature_weight[i] * lam
                - targets[i]
            )

            hessian = weight * (
                quadrature_weight[i] * lam
            )

            gradient *= catboost_weights[i]
            hessian *= catboost_weights[i]

            result.append(
                (-gradient, -hessian)
            )

        return result


class IterativeLogisticObjective(_BatchObjective):
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

        super().__init__(len(self.dummy_intensity))

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

        start = self._next_batch(n)

        dummy_intensity = self._batch_slice(
            self.dummy_intensity,
            start,
            n,
            "dummy_intensity",
        )

        interaction_integral = self._batch_slice(
            self.interaction_integral,
            start,
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

            delta = dummy_intensity[i]

            lam = math.exp(eta)

            denominator = (
                1.0
                + lam * interaction_integral[i]
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
