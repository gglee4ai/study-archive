#' hypergeometric pdf with size, D, N parameterization
#' 
#' @param x A vector of integers between max(0, size + D - N) and pmin(size, D).
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @inheritParams phyper2
#' @return hypergeometric cdf.
#' @examples
#' dhyper2(2, 5, 10, 20)
#' dhyper2(4, 5, 10, 20)
#' 
#' @importFrom stats dhyper
#' @export
dhyper2 <- function(x, size, D, N, log = FALSE) {
    if (any(size <= 0) || any(size > N)) 
        stop("size must be greater than 0 and less than or equal to N")
    if (any(N <= 0)) 
        stop("N must be greater than 0")
    if (any(D < 0) || any(D > N)) 
        stop("D must be between 0 and N")
    minvalues <- pmax(0, size + D - N)
    maxvalues <- pmin(size, D)
    if (any(x < minvalues) || any(x > maxvalues)) 
        stop("x outside allowable range")
    return(dhyper(x, m = D, n = N - D, k = size, log = log))
}
