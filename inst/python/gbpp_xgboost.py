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
