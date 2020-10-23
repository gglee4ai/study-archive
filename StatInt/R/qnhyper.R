#' negative hypergeometric quantile with k, D, N parameterization
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @inheritParams pnhyper
#' @return negative hypergeometric quantile.
#' @examples
#' qnhyper(0.1, 5, 10, 20)
#' qnhyper(seq(0.1,0.9,by=0.1), 5, 10, 40)
#' 
#' @export
qnhyper <- function(p, k, D, N) {
    if (any(D > N) || any(D <= 0)) 
        stop(paste("D must be greater than 0 and less than N.", "\n"))
    if (any(k < 1) || any(k > D)) 
        stop(paste("k must be between 1 and D.", "\n"))
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    x.values <- 0:(N - D)
    cdf <- pnhyper(x.values, k = k, D = D, N = N)
    the.quantile <- min(x.values[which(cdf >= p)])
    return(the.quantile)
}
