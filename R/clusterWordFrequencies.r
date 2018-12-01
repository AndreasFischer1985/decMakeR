#' Function clusterWordFrequencies
#' @export


clusterWordFrequencies <- function (corpus, cor = F, binary = T, min = 0, plot = T) 
{
    data = t(tdmCorpus(corpus, min = 0, plot = F))
    if (binary) 
        t = apply(data > 0, c(1, 2), as.numeric)
    docs = apply(data > 0, 2, sum)
    if (is.null(min)) 
        min = 1
    dat = data[, docs >= min]
    if (cor) 
        cluster = (hclust(dist(cor(dat))))
    if (!cor) 
        cluster = (hclust(dist(t(dat))))
    if (plot) 
        plot(cluster)
}
