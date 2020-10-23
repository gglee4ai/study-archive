#' Factors for computing a confidence interval for a normal distribution quantile
#' 
#' @param gamma A vector of confidence levels (e.g., 0.90, 0.95, or 0.99).
#' @param pvalue A vector of probabilities, corresponding to the quantile of interest.
#' @param sample.size A vector of sample size values (corresponding to the sample size providing the data for the construction of the confidence intervals).
#' @param method The method is either Odeh or nctr (which uses the R funcion for a quantile of the noncentral t distribution). The Odeh methods has been shown to be more accurate and it therefore recommended.
#' @return coefficinet for computing a confidence interal for a normal distribution quantile.
#' @examples
#' normQuantileCI(0.95, 0.90, 100)
#' normQuantileCI(0.95, 0.99, 100)
#' 
#' @importFrom stats qt qnorm
#' @useDynLib StatInt
#' @export
normQuantileCI <- function(gamma, pvalue, sample.size, method = "Odeh") {
    # factors for MHE2017 Table J.7 used the MHE2017 definition of p (pvalue) (complement of the Odeh definition) simulation
    # suggests that Odeh's method is more accurate for large n
    nrows <- max(length(pvalue), length(gamma), length(sample.size))
    pvalue <- expand.vec(pvalue, nrows)
    gamma <- expand.vec(gamma, nrows)
    sample.size <- expand.vec(sample.size, nrows)
    if (any(sample.size < 2)) 
        stop("Sample size must be greater than 1")
    switch(casefold(method), odeh = {
        gfactors <- .Fortran("sxtab1", pvalue = as.double(1 - pvalue), gamma = as.double(gamma), sample.size = as.double(sample.size), 
            xk = double(nrows), nrows = as.integer(nrows), ier = integer(1), kprint = as.integer(0), PACKAGE = "StatInt")$xk
    }, nctr = {
        gfactors <- qt(p = gamma, df = sample.size - 1, ncp = -qnorm(pvalue) * sqrt(sample.size))/sqrt(sample.size)
    })
    gfactors
}
