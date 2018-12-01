#' Function prospectTheory
#' @export


prospectTheory <- function (c = 2^(seq(1, 100) - 1), p = 1/2^seq(1, 100), vFun = NULL, 
    wFun = NULL, plot = T) 
{
    valueFunctionPN = function(x) {
        v = x^0.88
        v[x < 0] = -2.25 * (-x[x < 0])^0.88
        return(v)
    }
    valueFunctionA = function(x) {
        k = (x < 0) + 1
        v = k * (1/(1 + exp(-x)) - 0.5)
        return(v)
    }
    valueFunctionB = function(x, a = 1.376768, b = 3.096566) {
        v = a * log(abs(x) + 1)
        v[x < 0] = -b * log(abs(x[x < 0]) + 1)
        return(v)
    }
    weightingFunction = function(x) {
        d = 1.3
        p = 0.65
        w = exp(-d * (-log(x))^p)
        return(w)
    }
    if (is.null(vFun)) 
        vFun = valueFunctionPN
    v = vFun
    if (is.null(wFun)) 
        wFun = weightingFunction
    w = wFun
    if (plot) {
        par(mfrow = c(1, 2))
        x <- seq(-1, 1, length = 100)
        plot(x, vFun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("value-funktion v(x)")
        x <- seq(0, 1, length = 100)
        plot(x, wFun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("weight-funktion w(x)")
    }
    return(sum(w(p) * v(c)))
}
