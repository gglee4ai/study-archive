#' llogis quantile
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @param lower.tail  logical; if TRUE (default), probabilities are Pr[X <= x], otherwise, Pr[X > x].
#' @param log.p logical; if TRUE, probabilities p are given as log(p)
#' @inheritParams pllogis
#' @return llogis quantile.
#' @examples
#' qllogis(seq(0.1, 0.9, by=0.1), locationlog=3, scalelog=2)
#' qllogis(seq(0.1, 0.9, by=0.1), locationlog=3, scalelog=1)
#'
#' @importFrom stats qlogis
#' @export
qllogis <- function(p, locationlog = 0, scalelog = 1, lower.tail = TRUE, log.p = FALSE) {
    if (any(scalelog <= 0)) 
        stop("scalelog must be greater than 0")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    return(exp(qlogis(p, location = locationlog, scale = scalelog, lower.tail = lower.tail, log.p = log.p)))
}

