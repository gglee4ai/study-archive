#' lev pdf
#' 
#' @param q A vector of quantiles.
#' @inheritParams plev
#' @return lev cdf.
#' @examples
#' dlev(0:5, location=3, scale=2)
#' dlev(0:5, location=3, scale=1)
#' 
#' @export
dlev <- function(q, location = 0, scale = 1) {
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    zvec <- (q - location)/scale
    return((1/scale) * exp(-zvec - exp(-zvec)))
}
