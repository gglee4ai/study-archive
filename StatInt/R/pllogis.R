#' loglogistic cdf
#' 
#' @param q A vector of positive quantiles.
#' @param locationlog A vector of numbers.
#' @param scalelog A vector of positive numbers.
#' @param lower.tail logical; if TRUE (default), probabilities are Pr[X <= x], otherwise, Pr[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @return loglogistic cdf.
#' @examples
#' pllogis(1:5, locationlog=3, scalelog=2)
#' pllogis(1:5, locationlog=3, scalelog=1)
#' 
#' @importFrom stats plogis
#' @export
pllogis <- function(q, locationlog = 0, scalelog = 1, lower.tail = TRUE, log.p = FALSE) {
    # 
    if (any(scalelog <= 0)) 
        stop("scalelog must be greater than 0")
    if (any(q <= 0)) 
        stop("q must be greater than 0")
    return(plogis(q = log(q), location = locationlog, scale = scalelog, lower.tail = lower.tail, log.p = log.p))
}

