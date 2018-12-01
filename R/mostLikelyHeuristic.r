#' Function mostLikelyHeuristic
#' @export


mostLikelyHeuristic <- function (c1, p1, c2, p2) 
{
    lexicographicFunction = function(p, x, n) {
        d = data.frame(p, x)
        d = (d[order(d[, 1], decreasing = T), ][n, 2])
        return(d)
    }
    t1 = lexicographicFunction(p1, c1, 1)
    t2 = lexicographicFunction(p2, c2, 1)
    if (t1 != t2) 
        return(which.max(c(t1, t2)))
    return(1.5)
}
