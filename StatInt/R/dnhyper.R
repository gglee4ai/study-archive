#' negative hypergeometric pdf with k, D, N parameterization
#' 
#' @param x A vector of quantiles.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @inheritParams pnhyper
#' @return negative hypergeometric pdf.
#' @examples
#' dnhyper(2, 5, 10, 20)
#' dnhyper(2:7, 5, 10, 40)
#' @export
dnhyper <- function(x, k, D, N, log = FALSE) {
    if (any(D > N) || any(D <= 0)) 
        stop(paste("D must be greater than 0 and less than N.", "\n"))
    if (any(k < 1) || any(k > D)) 
        stop(paste("k must be between 1 and D.", "\n"))
    if (any(x < 0) || any(x > N - D)) 
        stop(paste("x must be between 0 and N-D.", "\n"))
    p <- lchoose(x + k - 1, x) + lchoose(N - x - k, N - D - x) - lchoose(N, D)
    if (!log) {
        p <- exp(p)
        p[is.nan(p)] <- 0
        p <- pmin(pmax(p, 0), 1)
    }
    return(p)
}
