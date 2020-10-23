#' sev cdf
#' 
#' @param q A vector of quantiles.
#' @param location A vector of numbers.
#' @param scale A vector of positive numbers.
#' @return sev cdf.
#' @examples
#' psev(0:5, location=3, scale=2)
#' psev(0:5, location=3, scale=1)
#' 
#' @export
psev <- function(q, location = 0, scale = 1) {
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    return(1 - ssev(q, location, scale))
}
