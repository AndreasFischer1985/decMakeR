#' Function outcomeHeuristic
#' @export


outcomeHeuristic <- function (c1, c2, fun = NULL) 
{
    equalWeightFunction = function(x) {
        return(sum(x))
    }
    equiprobableFunction = function(x) {
        return(mean(x))
    }
    minimaxFunction = function(x) {
        return(min(x))
    }
    maximaxFunction = function(x) {
        return(max(x))
    }
    betterThanAverageFunction = function(x, overallAverage) {
        return(sum(x > overallAverage))
    }
    if (is.null(fun)) 
        fun = equalWeightFunction
    o1 = fun(c1)
    o2 = fun(c2)
    if (o1 != o2) 
        return(which.max(c(o1, o2)))
    return(1.5)
}
