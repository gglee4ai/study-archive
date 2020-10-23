#' frechet pdf
#' 
#' @param x A vector of positive quantiles.
#' @inheritParams pfrechet 
#' @return frechet cdf.
#' @examples
#' pfrechet(1:5, shape=3, scale=2)
#' pfrechet(1:5, shape=3, scale=1)
#' 
#' @export
dfrechet <- function(x, shape, scale = 1) {
    # frechet pdf
    if (any(shape <= 0)) 
        stop("shape must be greater than 0")
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    if (any(x <= 0)) 
        stop("x must be greater than 0")
    return((shape/x) * ((scale/x)^shape) * exp(-(scale/x)^shape))
}
