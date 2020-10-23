#' beta-binomiam quantile
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @inheritParams pbetabinom
#' @return beta-binomiam quantile.
#' @examples
#' qbetabinom(0.10, 10, 2, 3)
#' qbetabinom(seq(0.1,0.9,by=0.1), 20, 2, 3)
#' 
#' @export
qbetabinom <- function(p, size, shape1, shape2) {
    if (any(size <= 0)) 
        stop("size must be greater than 0")
    if (any(shape1 <= 0)) 
        stop("shape1 must be greater than 0")
    if (any(shape2 <= 0)) 
        stop("shape2 must be greater than 0")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    the.cumsum <- cumsum(dbetabinom(0:size, size, shape1, shape2))
    sapply(p, function(x) sum(the.cumsum < x))
}
