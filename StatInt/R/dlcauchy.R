#' log-cauchy pdf
#' 
#' @param x A vector of positive quantiles.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @inheritParams plcauchy
#' @return log-cauchy pdf.
#' @examples
#' dlcauchy(1:5, locationlog=3, scalelog=2)
#' dlcauchy(1:5, locationlog=3, scalelog=1)
#' 
#' @importFrom stats dcauchy
#' @export
dlcauchy <- function(x, locationlog = 0, scalelog = 1, log = FALSE) {
    if (any(scalelog <= 0)) 
        stop("scalelog must be greater than 0")
    if (any(x <= 0)) 
        stop("x must be greater than 0")
    return((1/x) * dcauchy(x = log(x), location = locationlog, scale = scalelog, log = log))
}

