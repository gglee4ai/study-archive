#' lev quantile
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @inheritParams plev
#' @return lev cdf.
#' @examples
#' qlev(seq(0.1, 0.9, by=0.1), location=3, scale=2)
#' qlev(seq(0.1, 0.9, by=0.1), location=3, scale=1)
#' 
#' @export
qlev <- function(p, location = 0, scale = 1) {
    if (any(scale <= 0)) 
        stop("scale must be greater than 0")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    return(location - scale * logb(-logb(p)))
}
