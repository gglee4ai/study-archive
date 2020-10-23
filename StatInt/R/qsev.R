
#' sev quantile
#' 
#' @param  p A vector of probabilities between 0 and 1.
#' @inheritParams psev
#' @return sev cdf.
#' @examples
#' qsev(seq(0.1, 0.9, by=0.1), location=3, scale=2)
#' qsev(seq(0.1, 0.9, by=0.1), location=3, scale=1)
#' 
#' @export
qsev <- function(p, location = 0, scale = 1) {
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    return(location + scale * logb(-logb(1 - p)))
}
