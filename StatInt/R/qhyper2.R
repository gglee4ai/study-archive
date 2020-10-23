#' hypergeometric quantile with size, D, N parameterization
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @inheritParams phyper2
#' @return hypergeometric cdf.
#' @examples
#' qhyper2(0.1, 5, 10, 20)
#' qhyper2(0.9, 5, 10, 20)
#'
#' @importFrom stats qhyper
#' @export
qhyper2 <- function(p, size, D, N, lower.tail = TRUE, log.p = FALSE) {
    if (any(size <= 0) || any(size > N)) 
        stop("size must be greater than 0 and less than or equal to N")
    if (any(N <= 0)) 
        stop("N must be greater than 0")
    if (any(D < 0) || any(D > N)) 
        stop("D must be between 0 and N")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    return(qhyper(p, m = D, n = N - D, k = size, lower.tail, log.p))
}
