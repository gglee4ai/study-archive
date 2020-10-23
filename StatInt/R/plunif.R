#' log-uniform cdf
#' 
#' @param q A vector of positive quantiles.
#' @param location1 A vector of positive numbers.
#' @param location2 A vector of positive numbers location2 > location1.
#' @return log-uniform cdf
#' @examples
#' plunif(c(.1,1,10), location1=.01, location2=100)
#' plunif(c(.1,1,10), location1=.001, location2=1000)
#' 
#' @export
plunif <- function(q, location1 = 0, location2 = 1) {
    if (any(location1 <= 0)) 
        stop("location1 must be greater than 0")
    if (any(location2 <= 0)) 
        stop("location2 must be greater than 0")
    if (any(location2 <= location1)) 
        stop("location2 must be greater than location1")
    if (any(q < location1) || any(q > location2)) 
        stop("q must be between location1 location2")
    return((log(q) - log(location1))/(log(location2) - log(location1)))
}

