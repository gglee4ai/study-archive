#' loglogistic pdf
#' 
#' @param x A vector of positive quantiles.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @inheritParams pllogis
#' @return loglogistic pdf.
#' @examples
#' dllogis(1:5, locationlog=3, scalelog=2)
#' dllogis(1:5, locationlog=3, scalelog=1)
#' 
#' @importFrom stats dlogis
#' @export
dllogis <- function(x, locationlog = 0, scalelog = 1, log = FALSE) {
    if (any(scalelog <= 0)) 
        stop("scalelog must be greater than 0")
    if (any(x <= 0)) 
        stop("x must be greater than 0")
    return((1/x) * dlogis(x = log(x), location = locationlog, scale = scalelog, log = log))
}


