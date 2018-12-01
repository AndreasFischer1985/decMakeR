#' Function bernoulli
#' @export


bernoulli <- function (c = 2^(seq(1, 100) - 1), p = 1/2^seq(1, 100), uFun = NULL, 
    plot = T) 
{
    if (is.null(uFun)) 
        uFun = function(x) {
            u = log(x + 1)
            return(u)
        }
    u = uFun
    if (plot) {
        x <- seq(-1, 1, length = 100)
        plot(x, uFun(x), type = "l", lwd = 3)
        lines(x, x, lty = 2)
        title("utility-funktion u(x)")
    }
    return(sum(p * u(c)))
}
