#' frechet quantile
#' 
#' @param  p A vector of probabilities between 0 and 1.
#' @inheritParams pfrechet 
#' @return frechet cdf.
#' @examples
#' qfrechet(seq(0.1,0.9,by=0.1), shape=3, scale=2)
#' qfrechet(seq(0.1,0.9,by=0.1), shape=3, scale=1)
#' 
#' @export
qfrechet <- function(p, shape, scale = 1) {
    # frechet quantile
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    if (any(shape <= 0)) 
        stop("shape must be greater than 0")
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    return(scale * exp(qlev(p, 0, 1)/shape))
}
