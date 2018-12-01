#' Function eliminationHeuristic
#' @export


eliminationHeuristic <- function (c1, p1, c2, p2) 
{
    lexicographicFunction = function(p, x, n) {
        d = data.frame(p, x)
        d = (d[order(d[, 1], decreasing = T), ][n, 2])
        return(d)
    }
    krit = max(c(max(c1), max(c2)))
    aspects = min(c(length(c1), length(c2)))
    for (k in 0:100) for (a in 1:aspects) {
        e1 = sum(lexicographicFunction(p1, c1, a) >= krit - k/100 * 
            krit)
        e2 = sum(lexicographicFunction(p2, c2, a) >= krit - k/100 * 
            krit)
        if (e1 > 0 && e2 == 0) 
            return(1)
        if (e2 > 0 && e1 == 0) 
            return(2)
    }
    return(1.5)
}
