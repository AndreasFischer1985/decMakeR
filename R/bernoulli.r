#' Function bernoulli
#' @export


bernoulli <- function (p, k, uFun) 
{
    if (is.null(uFun)) 
        uFun = function(x) {
            u = log(x + 1)
            return(u)
        }
    u = uFun
    return(sum(p * u(k)))
}
