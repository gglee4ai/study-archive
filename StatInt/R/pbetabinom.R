#' beta-binomiam cdf
#' 
#' @param q A vector of quantiles between 0 and size.
#' @param size A vector of positive integers.
#' @param shape1 A vector of numbers greater than 0.
#' @param shape2 A vector of numbers greater than 0.
#' @return beta-binomiam cdf.
#' @examples
#' pbetabinom(0:7, 10, 2, 3)
#' pbetabinom(0:7, 20, 2, 3)
#' 
#' @export
pbetabinom <- function(q, size, shape1, shape2) {
    if (any(size <= 0)) 
        stop("size must be greater than 0")
    if (any(shape1 <= 0)) 
        stop("shape1 must be greater than 0")
    if (any(shape2 <= 0)) 
        stop("shape2 must be greater than 0")
    if (any(q < 0) || any(q > size)) 
        stop("q must be between 0 and size")
    return(sapply(q, function(xx) sum(dbetabinom(0:xx, size, shape1, shape2))))
}
