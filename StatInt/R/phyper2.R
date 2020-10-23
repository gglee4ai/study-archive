#' hypergeometric cdf with size, D, N parameterization
#' 
#' @param q A vector of integers between max(0, size + D - N) and pmin(size, D).
#' @param size A vector of integers between 1 and N.
#' @param D A vector of integers between 1 and N.
#' @param N A vector of positive integers.
#' @param lower.tail logical; if TRUE (default), probabilities are Pr[X <= x], otherwise, Pr[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return hypergeometric cdf.
#' @examples
#' phyper2(2, 5, 10, 20)
#' phyper2(4, 5, 10, 20)
#' 
#'
#' @importFrom stats phyper
#' @export
phyper2 <- function(q, size, D, N, lower.tail = TRUE, log.p = FALSE) {
    if (any(size <= 0) || any(size > N)) 
        stop("size must be greater than 0 and less than or equal to N")
    if (any(N <= 0)) 
        stop("N must be greater than 0")
    if (any(D < 0) || any(D > N)) 
        stop("D must be between 0 and N")
    minvalues <- pmax(0, size + D - N)
    maxvalues <- pmin(size, D)
    if (any(q < minvalues) || any(q > maxvalues)) 
        stop("q outside allowable range")
    return(phyper(q, m = D, n = N - D, k = size, lower.tail = lower.tail, log.p = log.p))
}
