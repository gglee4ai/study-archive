#' log-uniform quantile
#' 
#' @param p A vector of probabilities between 0 and 1.
#' @inheritParams plunif 
#' @return log-uniform quantile
#' @examples
#' qlunif(seq(0.1, 0.9, by=0.1), location1=.01, location2=100)
#' qlunif(seq(0.1, 0.9, by=0.1), location1=.001, location2=1000)
#'
#' @export
qlunif <- function(p, location1, location2 = 1) {
    # log-uniform quantile
    if (any(location1 <= 0)) 
        stop("location1 must be greater than 0")
    if (any(location2 <= 0)) 
        stop("location2 must be greater than 0")
    if (any(location2 <= location1)) 
        stop("location2 must be greater than location1")
    if (any(p <= 0) || any(p >= 1)) 
        stop("p must be between 0 and 1")
    return(location1 * (location2/location1)^p)
}


