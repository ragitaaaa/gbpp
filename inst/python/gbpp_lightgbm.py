"""
LightGBM backend for gbpp.

This module provides custom objective factories for the six
GBPP objective functions:

1. Poisson
2. Weighted Poisson
3. Logistic
4. Weighted Logistic
5. Iterative Poisson
6. Iterative Logistic
"""

import numpy as np


# ============================================================
# Utility
# ============================================================

def _clip_prediction(preds):
    """
    Clip raw predictions before exponentiation.

    This prevents numerical overflow in exp(prediction).
    """
    return np.clip(np.asarray(preds, dtype=float), -50.0, 50.0)


def _get_labels(train_data):
    """
    Extract labels from a LightGBM Dataset.
    """
    return np.asarray(train_data.get_label(), dtype=float)


# ============================================================
# Poisson
# ============================================================

def make_poisson_objective(quadrature_weight):
    """
    Create the GBPP Poisson custom objective.

    Loss:
        L = -sum(y_i * phi_i) + sum(v_i * exp(phi_i))

    Gradient:
        g_i = v_i * exp(phi_i) - y_i

    Hessian:
        h_i = v_i * exp(phi_i)
    """

    quadrature_weight = np.asarray(
        quadrature_weight,
        dtype=float
    )

    def poisson_objective(preds, train_data):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        lam = np.exp(phi)

        gradient = (
            quadrature_weight * lam
            - labels
        )

        hessian = (
            quadrature_weight * lam
        )

        return gradient, hessian

    return poisson_objective


# ============================================================
# Weighted Poisson
# ============================================================

def make_weighted_poisson_objective(
    quadrature_weight,
    statistical_weight
):
    """
    Create the GBPP weighted Poisson custom objective.

    Gradient:
        g_i = w_i * (v_i * exp(phi_i) - y_i)

    Hessian:
        h_i = w_i * v_i * exp(phi_i)
    """

    quadrature_weight = np.asarray(
        quadrature_weight,
        dtype=float
    )

    statistical_weight = np.asarray(
        statistical_weight,
        dtype=float
    )

    def weighted_poisson_objective(preds, train_data):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        lam = np.exp(phi)

        gradient = statistical_weight * (
            quadrature_weight * lam
            - labels
        )

        hessian = (
            statistical_weight
            * quadrature_weight
            * lam
        )

        return gradient, hessian

    return weighted_poisson_objective


# ============================================================
# Logistic
# ============================================================

def make_logistic_objective(dummy_intensity):
    """
    Create the GBPP logistic custom objective.

    pi_i =
        exp(phi_i) / (exp(phi_i) + delta_i)

    Gradient:
        g_i = pi_i - y_i

    Hessian:
        h_i = pi_i * (1 - pi_i)
    """

    dummy_intensity = np.asarray(
        dummy_intensity,
        dtype=float
    )

    log_dummy_intensity = np.log(dummy_intensity)

    def logistic_objective(preds, train_data):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        # pi = sigmoid(phi - log(delta))
        pi = 1.0 / (
            1.0
            + np.exp(
                -(phi - log_dummy_intensity)
            )
        )

        gradient = pi - labels

        hessian = pi * (1.0 - pi)

        return gradient, hessian

    return logistic_objective


# ============================================================
# Weighted Logistic
# ============================================================

def make_weighted_logistic_objective(
    dummy_intensity,
    statistical_weight
):
    """
    Create the GBPP weighted logistic custom objective.

    Gradient:
        g_i = w_i * (pi_i - y_i)

    Hessian:
        h_i = w_i * pi_i * (1 - pi_i)
    """

    dummy_intensity = np.asarray(
        dummy_intensity,
        dtype=float
    )

    statistical_weight = np.asarray(
        statistical_weight,
        dtype=float
    )

    log_dummy_intensity = np.log(dummy_intensity)

    def weighted_logistic_objective(preds, train_data):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        pi = 1.0 / (
            1.0
            + np.exp(
                -(phi - log_dummy_intensity)
            )
        )

        gradient = (
            statistical_weight
            * (pi - labels)
        )

        hessian = (
            statistical_weight
            * pi
            * (1.0 - pi)
        )

        return gradient, hessian

    return weighted_logistic_objective


# ============================================================
# Iterative Poisson
# ============================================================

def make_iterative_poisson_objective(
    quadrature_weight,
    interaction_integral
):
    """
    Create the iterative GBPP Poisson custom objective.

    Weight:
        w_i = 1 / (1 + lambda_i * A_i)

    Gradient:
        g_i = w_i * (v_i * lambda_i - y_i)

    Hessian:
        h_i = w_i * v_i * lambda_i

    where:
        lambda_i = exp(phi_i)
        A_i       = interaction_integral_i
    """

    quadrature_weight = np.asarray(
        quadrature_weight,
        dtype=float
    )

    interaction_integral = np.asarray(
        interaction_integral,
        dtype=float
    )

    def iterative_poisson_objective(
        preds,
        train_data
    ):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        lam = np.exp(phi)

        denominator = (
            1.0
            + lam * interaction_integral
        )

        if np.any(denominator <= 0.0):
            raise ValueError(
                "Iterative Poisson weight denominator "
                "must be positive."
            )

        weight = 1.0 / denominator
        weight = weight / np.mean(weight)

        gradient = weight * (
            quadrature_weight * lam
            - labels
        )

        hessian = (
            weight
            * quadrature_weight
            * lam
        )

        return gradient, hessian

    return iterative_poisson_objective


# ============================================================
# Iterative Logistic
# ============================================================

def make_iterative_logistic_objective(
    dummy_intensity,
    interaction_integral
):
    """
    Create the iterative GBPP logistic custom objective.

    Weight:
        w_i =
            (lambda_i + delta_i)
            /
            [delta_i * (1 + lambda_i * A_i)]

    Gradient:
        g_i = w_i * (pi_i - y_i)

    Hessian:
        h_i = w_i * pi_i * (1 - pi_i)

    where:
        lambda_i = exp(phi_i)
        delta_i   = dummy intensity
        A_i       = interaction integral
    """

    dummy_intensity = np.asarray(
        dummy_intensity,
        dtype=float
    )

    interaction_integral = np.asarray(
        interaction_integral,
        dtype=float
    )

    log_dummy_intensity = np.log(
        dummy_intensity
    )

    def iterative_logistic_objective(
        preds,
        train_data
    ):

        phi = _clip_prediction(preds)
        labels = _get_labels(train_data)

        lam = np.exp(phi)

        denominator = (
            1.0
            + lam * interaction_integral
        )

        if np.any(denominator <= 0.0):
            raise ValueError(
                "Iterative Logistic weight denominator "
                "must be positive."
            )

        weight = (
            (lam + dummy_intensity)
            /
            (
                dummy_intensity
                * denominator
            )
        )
        weight = weight / np.mean(weight)

        pi = 1.0 / (
            1.0
            + np.exp(
                -(phi - log_dummy_intensity)
            )
        )

        gradient = weight * (
            pi - labels
        )

        hessian = (
            weight
            * pi
            * (1.0 - pi)
        )

        return gradient, hessian

    return iterative_logistic_objective
