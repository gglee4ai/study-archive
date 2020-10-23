#' log-uniform pdf
#' 
#' @param x A vector of positive quantiles.
#' @inheritParams plunif 
#' @return log-uniform pdf
#' @examples
#' dlunif(c(.1,1,10), location1=.01, location2=100)
#' dlunif(c(.1,1,10), location1=.001, location2=1000)
#' 
#' @export
dlunif <- function(x, location1, location2 = 1) {
    if (location1 <= 0) 
        stop("location1 must be greater than 0")
    if (location2 <= 0) 
        stop("location2 must be greater than 0")
    if (location2 <= location1) 
        stop("location2 must be greater than location1")
    if (any(x < location1) || any(x > location2)) 
        stop("q must be between location1 location2")
    return(1/(x * (log(location2) - log(location1))))
}
