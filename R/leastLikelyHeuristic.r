#' Function leastLikelyHeuristic
#' @export


leastLikelyHeuristic <- function (c1, p1, c2, p2) 
{
    LeastLikelyFunction = function(p, x, n) {
        d = data.frame(p, x)
        d = (d[order(d[, 2], decreasing = F), ][n, 1])
        return(d)
    }
    t1 = LeastLikelyFunction(p1, c1, 1)
    t2 = LeastLikelyFunction(p2, c2, 1)
    if (t1 != t2) 
        return(which.min(c(t1, t2)))
    return(1.5)
}
