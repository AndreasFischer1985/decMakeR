#' Function cumulativeProspectTheory
#' @export


cumulativeProspectTheory <- function (c = 2^(seq(1, 100) - 1), p = 1/2^seq(1, 100), vFun = NULL, 
    w1Fun = NULL, w2Fun = NULL, plot = T) 
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
    weightingFunctionP = function(x) {
        p = 0.61
        w = (x^p)/(((x^p) + ((1 - x)^p))^(1/p))
        return(w)
    }
    weightingFunctionN = function(x) {
        p = 0.69
        w = (x^p)/(((x^p) + ((1 - x)^p))^(1/p))
        return(w)
    }
    if (is.null(vFun)) 
        vFun = valueFunctionPN
    v1 = vFun
    if (is.null(w1Fun)) 
        w1Fun = weightingFunctionP
    w1 = w1Fun
    if (is.null(w2Fun)) 
        w2Fun = weightingFunctionN
    w2 = w2Fun
    d = data.frame(p, c)
    d = d[order(d[, 2]), ]
    d2 = d
    posExist = 0
    for (i in 1:length(p)) if (d[i, 2] > 0 && posExist == 0) 
        posExist = i
    posBegin = 0
    for (i in 1:length(p)) if (d[i, 2] >= 0 && posBegin == 0) 
        posBegin = i
    negEnd = 0
    for (i in 1:length(p)) if (d[i, 2] > 0 && negEnd == 0) 
        negEnd = i - 1
    if (posExist == 0) 
        negEnd = length(p)
    if (negEnd > 0) 
        for (i in 1:negEnd) {
            psum1 = 0
            psum2 = 0
            for (n in 1:i) psum1 = psum1 + (d[n, 1])
            if (i > 1) 
                for (n in 1:(i - 1)) psum2 = psum2 + (d[n, 1])
            d2[i, 1] = w2(psum1) - w2(psum2)
            d2[i, 2] = v1(d2[i, 2])
        }
    if (posBegin > 0) 
        for (i in posBegin:length(p)) {
            psum1 = 0
            psum2 = 0
            for (n in i:length(p)) psum1 = psum1 + (d[n, 1])
            if ((i + 1) <= length(p)) 
                for (n in (i + 1):length(p)) psum2 = psum2 + 
                  (d[n, 1])
            d2[i, 1] = w1(psum1) - w1(psum2)
            d2[i, 2] = v1(d2[i, 2])
        }
    d
    d2
    if (plot) {
        par(mfrow = c(2, 2))
        x <- seq(0, 1, length = 100)
        plot(x, w1Fun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("weight-funktion w1(x)")
        x <- seq(0, 1, length = 100)
        plot(x, w2Fun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("weight-funktion w2(x)")
        x <- seq(-1, 1, length = 100)
        plot(x, vFun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("value-funktion v(x)")
    }
    return(sum(d2[, 1] * d2[, 2]))
}
