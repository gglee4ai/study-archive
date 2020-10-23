#' sev pdf
#' 
#' @param x A vector of quantiles.
#' @inheritParams psev
#' @return sev pdf.
#' @examples
#' dsev(0:5, location=3, scale=2)
#' dsev(0:5, location=3, scale=1)
#' 
#' @export
dsev <- function(x, location = 0, scale = 1) {
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    z <- (x - location)/scale
    return((1/scale) * exp(z - exp(z)))
}
