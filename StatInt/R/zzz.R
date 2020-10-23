
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("StatInt version ", packageVersion("StatInt"), " loaded")
    packageStartupMessage("Send bug reports to wqmeeker@iastate.edu")
}
