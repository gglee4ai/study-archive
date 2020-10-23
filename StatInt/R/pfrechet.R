#' frechet cdf
#' 
#' @param q A vector of positive quantiles.
#' @param shape A vector of positive numbers.
#' @param scale A vector of positive numbers.
#' @return frechet cdf.
#' @examples
#' pfrechet(1:5, shape=3, scale=2)
#' pfrechet(1:5, shape=3, scale=1)
#' 
#' @export
pfrechet <- function(q, shape, scale = 1) {
    # frechet cdf
    if (any(shape <= 0)) 
        stop("shape must be greater than 0")
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    if (any(q <= 0)) 
        stop("q must be greater than 0")
    return(plev(log(q), log(scale), 1/shape))
}
