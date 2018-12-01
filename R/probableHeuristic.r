#' Function probableHeuristic
#' @export


probableHeuristic <- function (c1, p1, c2, p2) 
{
    prob = 1/length(p1)
    p1[p1 < prob] = 0
    prob = 1/length(p2)
    p2[p2 < prob] = 0
    m1 = mean(c1[p1 > 0])
    m2 = mean(c2[p2 > 0])
    if (m1 != m2) 
        return(which.max(c(m1, m2)))
    if (m1 == m2) 
        return(1.5)
}
