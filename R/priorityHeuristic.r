#' Function priorityHeuristic
#' @export


priorityHeuristic <- function (c1, p1, c2, p2) 
{
    priorityFunction1 = function(x, p, maxGain) {
        a1 = 0.1 * maxGain
        a2 = 0.1
        a3 = 0.1 * maxGain
        a4 = 0
        r1 = min(x)
        r2 = -1 * p[which.min(x)]
        r3 = max(x)
        r4 = 1 * p[which.max(x)]
        d = data.frame(c(r1, r2, r3, r4), c(a1, a2, a3, a4))
        names(d) = c("values", "aspiration levels of differences")
        return(d)
    }
    priorityFunction2 = function(x, p, maxLoss) {
        a1 = -0.1 * maxLoss
        a2 = 0.1
        a3 = -0.1 * maxLoss
        a4 = 0
        r1 = max(x)
        r2 = 1 * p[which.max(x)]
        r3 = min(x)
        r4 = -1 * p[which.min(x)]
        d = data.frame(c(r1, r2, r3, r4), c(a1, a2, a3, a4))
        names(d) = c("values", "aspiration levels of differences")
        return(d)
    }
    if (sum(c1 >= 0) == length(p1) && sum(c2 >= 0) == length(p2) && 
        (length(p1) < 3 && length(p2) < 3)) {
        p1 = priorityFunction1(c1, p1, max(c(c1, c2)))
        p1[3, 2] = 0
        p2 = priorityFunction1(c2, p2, max(c(c1, c2)))
        p2[3, 2] = 0
        for (i in 1:3) {
            comp = p1[i, 1] - p2[i, 1]
            if (abs(comp) > p1[i, 2]) 
                return(which.max(c(p1[i, 1], p2[i, 1])))
        }
    }
    if (sum(c1 <= 0) == length(p1) && sum(c2 <= 0) == length(p2) && 
        (length(p1) < 3 && length(p2) < 3)) {
        p1 = priorityFunction2(c1, p1, min(c(c1, c2)))
        p1[3, 2] = 0
        p2 = priorityFunction2(c2, p2, min(c(c1, c2)))
        p2[3, 2] = 0
        for (i in 1:3) {
            comp = p1[i, 1] - p2[i, 1]
            if (abs(comp) >= p1[i, 2]) 
                return(which.max(c(p1[i, 1], p2[i, 1])))
        }
    }
    if (sum(c1 >= 0) == length(p1) && sum(c2 >= 0) == length(p2) && 
        (length(p1) > 2 || length(p2) > 2)) {
        p1 = priorityFunction1(c1, p1, max(c(c1, c2)))
        p2 = priorityFunction1(c2, p2, max(c(c1, c2)))
        for (i in 1:4) {
            comp = p1[i, 1] - p2[i, 1]
            if (abs(comp) >= p1[i, 2]) 
                return(which.max(c(p1[i, 1], p2[i, 1])))
        }
    }
    if (sum(c1 <= 0) == length(p1) && sum(c2 <= 0) == length(p2) && 
        (length(p1) > 2 || length(p2) > 2)) {
        p1 = priorityFunction2(c1, p1, min(c(c1, c2)))
        p2 = priorityFunction2(c2, p2, min(c(c1, c2)))
        for (i in 1:4) {
            comp = p1[i, 1] - p2[i, 1]
            if (abs(comp) >= p1[i, 2]) 
                return(which.max(c(p1[i, 1], p2[i, 1])))
        }
    }
    return(1.5)
}
