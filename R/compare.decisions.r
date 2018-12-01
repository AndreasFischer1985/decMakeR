#' Function compare.decisions
#' @export


compare.decisions <- function (m = NULL, maxi = 20, goal = 1, extended = F) 
{
    if (is.null(m)) {
        m = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
            1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
            1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0), nrow = 10, ncol = 10, byrow = T)
        rownames(m) = c("Politik", "UV Sanierung", "UV Produktion", 
            "Umweltbelastung", "Bevölkerung", "Vermehrung", "UV Lebensqualität", 
            "UV Aufklärung", "Aktionen", "Geburtenwunsch")
        colnames(m) = rownames(m)
    }
    n = rownames(m)
    if (is.null(n)) 
        n = colnames(m)
    m1 = m
    for (i in 1:sqrt(length(m))) m[, i][abs(m[, i]) > 0] = i
    ret1 = logical(0)
    ret2 = logical(0)
    ret3 = logical(0)
    as1 = logical(0)
    ps1 = logical(0)
    as2 = logical(0)
    ps2 = logical(0)
    p1 = logical(0)
    q1 = logical(0)
    p2 = logical(0)
    q2 = logical(0)
    m2 = matrix(0, nrow = length(m[, 1]), ncol = length(m[1, 
        ]))
    for (i in 1:length(m[, 1])) {
        c0 = m[i, ][m[i, ] > 0]
        l0 = 1
        l1 = length(c0)
        counter = 0
        e1 = 0
        g1 = 0
        if (maxi == 0) 
            e1 = e1 + length(c0[c0 == i])
        if (maxi == 0) 
            g1 = g1 + length(c0[c0 == goal])
        if (l1 > 0 && maxi != 0) 
            while (T && l1 >= l0) {
                counter = counter + 1
                c1 = logical(0)
                for (j in l0:l1) c1 = c(c1, m[c0[j], ][m[c0[j], 
                  ] > 0])
                e1 = e1 + length(c1[c1 == i])
                g1 = g1 + length(c1[c1 == goal])
                for (j in 1:l1) {
                  c1 = c1[c1 != c0[j]]
                }
                l0 = length(c0) + 1
                c0 = c(c0, c1)
                l1 = length(c0)
                if (counter >= maxi) 
                  break
            }
        ret1 = c(ret1, e1)
        ret2 = c(ret2, length(c0))
        ret3 = c(ret3, g1)
        as1 = c(as1, sum(abs(m1[i, ])))
        ps1 = c(ps1, sum(abs(m1[, i])))
        m2[i, c0] = 1
    }
    for (i in 1:length(m[, 1])) {
        as2 = c(as2, sum(m2[i, ]))
        ps2 = c(ps2, sum(m2[, i]))
    }
    p1 = as1 * ps1
    q1 = as1/ps1
    p2 = as2 * ps2
    q2 = as2/ps2
    dat = data.frame(ret1, ret2, ret3, as1, ps1, p1, q1, as2, 
        ps2, p2, q2)
    names(dat) = c("Rückkopplungen", paste("Zentralität", maxi), 
        paste("Zielzentralität", goal), "AS1", "PS1", "P1", "Q1", 
        "AS2", "PS2", "P2", "Q2")
    if (extended == T) 
        for (i in 1:length(m[, 1])) {
            dat = data.frame(dat, m2[i, ])
        }
    rownames(dat) = n
    return(dat)
}
