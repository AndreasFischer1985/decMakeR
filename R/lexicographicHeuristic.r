#' Function lexicographicHeuristic
#' @export


lexicographicHeuristic <- function (c1, p1, c2, p2) 
{
    lexicographicFunction = function(p, x, n) {
        d = data.frame(p, x)
        d = (d[order(d[, 1], decreasing = T), ][n, 2])
        return(d)
    }
    for (i in 1:min(length(c1), length(c2))) {
        t1 = lexicographicFunction(p1, c1, i)
        t2 = lexicographicFunction(p2, c2, i)
        if (t1 != t2) 
            return(which.max(c(t1, t2)))
    }
    return(1.5)
}
