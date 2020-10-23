ssev <- function(x, location = 0, scale = 1) {
    # smallest extreme value distribution survival function
    return(exp(-exp((x - location)/scale)))
}
