#' negative hypergeometric cdf with k, D, N parameterization
#' 
#' @param q A vector of quantiles.
#' @param k A vector of integers between 1 and D.
#' @param D A vector of integers between 1 and N.
#' @param N A vector of positive integers.
#' @param lower.tail logical; if TRUE (default), probabilities are Pr[X <= x], otherwise, Pr[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return negative hypergeometric cdf.
#' @examples
#' pnhyper(2, 5, 10, 20)
#' pnhyper(2:7, 5, 10, 40)
#' 
#' @export
pnhyper <- function(q, k, D, N, lower.tail = TRUE, log.p = FALSE) {
    if (any(D > N) || any(D <= 0)) 
        stop(paste("D must be greater than 0 and less than N.", "\n"))
    if (any(k < 1) || any(k > D)) 
        stop(paste("k must be between 1 and D.", "\n"))
    q <- ceiling(q)
    if (any(q < 0) || any(q > N - D)) 
        stop(paste("q must be between 0 and N-D.", "\n"))
    result <- rep(0, length(q))
    for (i in 1:length(q)) {
        if (q[i] < 0) {
            result[i] <- 0
        } else if (q[i] >= N - D) {
            result[i] <- 1
        } else result[i] <- sum(dnhyper(0:q[i], k = k, D = D, N = N))
    }
    if (lower.tail == FALSE) 
        result <- 1 - result
    if (log.p) {
        result <- log(result)
        return(result)
    }
    result <- pmin(pmax(result, 0), 1)
    return(result)
}
