#' Confidence interval for a normal distribution tail probability
#' 
#' @param alpha A vector of confidence level values (e.g., 0.90, 0.95, or 0.99) for one-sided upper confidence bounds or 1 - (confidence level)  values (e.g., 0.10, 0.05, 0.01) for one-sided lower confidence bounds on a normal distribution probability of being greater than a specified x value. To obtail a two-sided confidence interval use 1-alpha/2 and alpha/2.
#' @param xk.factor A vector of studentized values (xbar-x)/s.
#' @param sample.size A vector of sample size values (corresponding to the sample size providing the data for the construction of the confidence intervals.
#' @return Endpoint for the one-sided confidence bounds for the probability of being greater than a specified x
#' @examples
#' normTailCI(0.025, 1.6, 5)
#' normTailCI(0.975, 1.6, 5)
#' normTailCI(0.05, 1.6, 5)
#' normTailCI(0.95, -1.6, 5)
#' 
#' @useDynLib StatInt
#' @export
normTailCI <- function(alpha, xk.factor, sample.size) {
    nrows <- max(length(alpha), length(sample.size), length(xk.factor))
    alpha <- expand.vec(alpha, nrows)
    sample.size <- expand.vec(sample.size, nrows)
    if (any(sample.size < 2)) 
        stop("Sample size must be greater than 1")
    xk.factor <- expand.vec(xk.factor, nrows)
    zout <- .Fortran("sxtab7", conf.level = 1 - as.double(alpha), sample.size = as.double(sample.size), xk.factor = as.double(xk.factor), 
        answer = double(nrows), nrows = as.integer(nrows), ier = integer(1), kprint = as.integer(0), PACKAGE = "StatInt")
    # 
    return(zout$answer)
}
