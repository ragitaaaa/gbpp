import numpy as np

def make_poisson_objective(quadrature_weight):
    """
    Create the GBPP Poisson custom objective for XGBoost.

    Parameters
    ----------
    quadrature_weight : array-like
        Berman-Turner quadrature weights.

    Returns
    -------
    callable
        XGBoost custom objective function.
    """

    weight = np.asarray(quadrature_weight, dtype=float)

    def poisson_objective(preds, dtrain):
        phi = np.clip(preds, -50.0, 50.0)
        lam = np.exp(phi)
        labels = dtrain.get_label()

        gradient = weight * lam - labels
        hessian = weight * lam

        return gradient, hessian

    return poisson_objective

def make_weighted_poisson_objective(
    quadrature_weight,
    statistical_weight
):
    """
    Create the GBPP weighted Poisson custom objective for XGBoost.

    Parameters
    ----------
    quadrature_weight : array-like
        Berman-Turner quadrature weights.

    statistical_weight : array-like
        Fixed statistical/spatial weights.

    Returns
    -------
    callable
        XGBoost custom objective function.
    """

    quadrature_weight = np.asarray(
        quadrature_weight,
        dtype=float
    )

    statistical_weight = np.asarray(
        statistical_weight,
        dtype=float
    )

    def weighted_poisson_objective(preds, dtrain):
        phi = np.clip(preds, -50.0, 50.0)
        lam = np.exp(phi)
        labels = dtrain.get_label()

        gradient = (
            statistical_weight
            * (quadrature_weight * lam - labels)
        )

        hessian = (
            statistical_weight
            * quadrature_weight
            * lam
        )

        return gradient, hessian

    return weighted_poisson_objective

def make_logistic_objective(dummy_intensity):
    """
    Create the GBPP logistic custom objective for XGBoost.

    Parameters
    ----------
    dummy_intensity : array-like
        Dummy-process intensity at each quadrature point.

    Returns
    -------
    callable
        XGBoost custom objective function.
    """

    dummy_intensity = np.asarray(
        dummy_intensity,
        dtype=float
    )

    log_dummy_intensity = np.log(dummy_intensity)

    def logistic_objective(preds, dtrain):
        phi = np.clip(preds, -50.0, 50.0)
        labels = dtrain.get_label()

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

def make_weighted_logistic_objective(
    dummy_intensity,
    statistical_weight
):
    """
    Create the GBPP weighted logistic custom objective for XGBoost.

    Parameters
    ----------
    dummy_intensity : array-like
        Dummy-process intensity at each quadrature point.

    statistical_weight : array-like
        Fixed statistical/spatial weight at each quadrature point.

    Returns
    -------
    callable
        XGBoost custom objective function.
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

    def weighted_logistic_objective(preds, dtrain):
        phi = np.clip(preds, -50.0, 50.0)
        labels = dtrain.get_label()

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
