#' beta-binomiam pdf
#' 
#' @param x A vector of quantiles between 0 and size.
#' @inheritParams pbetabinom
#' @return beta-binomiam cdf.
#' @examples
#' pbetabinom(0:7, 10, 2, 3)
#' pbetabinom(0:7, 20, 2, 3)
#' 
#' @export
dbetabinom <- function(x, size, shape1, shape2) {
    if (any(size <= 0)) 
        stop("size must be greater than 0")
    if (any(shape1 <= 0)) 
        stop("shape1 must be greater than 0")
    if (any(shape2 <= 0)) 
        stop("shape2 must be greater than 0")
    if (any(x < 0) || any(x > size)) 
        stop("x must be between 0 and size")
    exp(lbeta(x + shape1, size - x + shape2) - lbeta(shape1, shape2) + lchoose(size, x))
}
