#' Factors for computing a confidence interval for a normal distribution quantile
#' 
#' @param conf.level A vector of confidence levels (e.g., 0.90, 0.95, or 0.99).
#' @param beta A vector of probabilities, corresponding to the amount of the distribution to be covered.
#' @param sample.size A vector of sample size values (corresponding to the sample size providing the data for the construction of the confidence intervals.
#' @return coefficient for computing a confidence interal for a normal distribution quantile.
#' @examples
#' normCenterTI(0.95, 0.90, 100)
#' normCenterTI(0.95, 0.99, 100)
#'
#' @useDynLib StatInt
#' @export
normCenterTI <- function(conf.level, beta, sample.size) {
    # factors for MHE2017 Table J.5
    nrows <- max(length(beta), length(conf.level), length(sample.size))
    beta <- expand.vec(beta, nrows)
    conf.level <- expand.vec(conf.level, nrows)
    sample.size <- expand.vec(sample.size, nrows)
    if (any(sample.size < 2)) 
        stop("Sample size must be greater than 1")
    zout <- .Fortran("sxtab3", sample.size = as.double(sample.size), beta = as.double(beta), conf.level = as.double(conf.level), 
        xk = double(nrows), nrows = as.integer(nrows), ier = integer(1), kprint = as.integer(0), PACKAGE = "StatInt")
    # 
    return(zout$xk)
}
