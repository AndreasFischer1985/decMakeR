#' Function satisficingHeuristic
#' @export


satisficingHeuristic <- function (c1, c2) 
{
    satisficingFunction = function(x, kriterium) {
        return(sum(x >= kriterium) == length(x))
    }
    krit = max(c(max(c1), max(c2)))
    for (k in 0:100) {
        if (satisficingFunction(c1, krit - k/100 * krit) > 0) 
            return(1)
        if (satisficingFunction(c2, krit - k/100 * krit) > 0) 
            return(2)
    }
    return(1.5)
}
