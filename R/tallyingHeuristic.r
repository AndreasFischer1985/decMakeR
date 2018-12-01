#' Function tallyingHeuristic
#' @export


tallyingHeuristic <- function (c1, p1, c2, p2) 
{
    t1 = 0
    t2 = 0
    t1 = t1 + (min(c1) > min(c2))
    t2 = t2 + (min(c1) < min(c2))
    t1 = t1 + (max(c1) > max(c2))
    t2 = t2 + (max(c1) < max(c2))
    f1 = ((min(c1) < 0) - 0.5) * -2
    f2 = ((min(c2) < 0) - 0.5) * -2
    t1 = t1 + (f1 * p1[which.min(c1)] < f2 * p2[which.min(c2)])
    t2 = t2 + (f1 * p1[which.min(c1)] > f2 * p2[which.min(c2)])
    f1 = ((max(c1) < 0) - 0.5) * -2
    f2 = ((max(c2) < 0) - 0.5) * -2
    t1 = t1 + (f1 * p1[which.max(c1)] > f2 * p2[which.max(c2)])
    t2 = t2 + (f1 * p1[which.max(c1)] < f2 * p2[which.max(c2)])
    if (t1 != t2) 
        return(which.max(c(t1, t2)))
    if (t1 == t2) 
        return(1.5)
}
