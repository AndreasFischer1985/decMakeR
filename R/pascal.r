#' Function pascal
#' @export


pascal <- function (c = 2^(seq(1, 100) - 1), p = 1/2^seq(1, 100)) 
{
    return(sum(p * c))
}
