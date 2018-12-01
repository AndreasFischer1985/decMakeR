#' Function compare.decisions
#' @export


compare.decisions <- function (c1 = 2^(seq(1, 100) - 1), p1 = 1/2^seq(1, 100), c2 = 10, 
    p2 = 1, plot = T) 
{
    competition = apply(data.frame(c(`Option 1` = pascal(c1, 
        p1), `Option 2` = pascal(c2, p2)), c(bernoulli(c = c1, 
        p = p1, plot = F), bernoulli(c = c2, p = p2, plot = F)), 
        c(prospectTheory(c1, p1, plot = F), prospectTheory(c2, 
            p2, plot = F)), c(cumulativeProspectTheory(c1, p1, 
            plot = F), cumulativeProspectTheory(c2, p2, plot = F)), 
        c(outcomeHeuristic(c1, c2, function(x) sum(x)) == 1, 
            outcomeHeuristic(c1, c2, function(x) sum(x)) == 2), 
        c(outcomeHeuristic(c1, c2, function(x) mean(x)) == 1, 
            outcomeHeuristic(c1, c2, function(x) mean(x)) == 
                2), c(outcomeHeuristic(c1, c2, function(x) min(x)) == 
            1, outcomeHeuristic(c1, c2, function(x) min(x)) == 
            2), c(outcomeHeuristic(c1, c2, function(x) max(x)) == 
            1, outcomeHeuristic(c1, c2, function(x) max(x)) == 
            2), c(satisficingHeuristic(c1, c2) == 1, satisficingHeuristic(c1, 
            c2) == 2), c(lexicographicHeuristic(c1, p1, c2, p2) == 
            1, lexicographicHeuristic(c1, p1, c2, p2) == 2), 
        c(eliminationHeuristic(c1, p1, c2, p2) == 1, eliminationHeuristic(c1, 
            p1, c2, p2) == 2), c(mostLikelyHeuristic(c1, p1, 
            c2, p2) == 1, mostLikelyHeuristic(c1, p1, c2, p2) == 
            2), c(leastLikelyHeuristic(c1, p1, c2, p2) == 1, 
            leastLikelyHeuristic(c1, p1, c2, p2) == 2), c(tallyingHeuristic(c1, 
            p1, c2, p2) == 1, tallyingHeuristic(c1, p1, c2, p2) == 
            2), c(probableHeuristic(c1, p1, c2, p2) == 1, probableHeuristic(c1, 
            p1, c2, p2) == 2), c(priorityHeuristic(c1, p1, c2, 
            p2) == 1, priorityHeuristic(c1, p1, c2, p2) == 2)), 
        1, as.numeric)
    competition[rowSums(competition) == 0, ] = c(0.5, 0.5)
    rownames(competition) = c("Pascal", "Bernoulli", "PT", "CPT", 
        "SumMax", "MeanMax", "MiniMax", "MaxiMax", "Satisficing", 
        "Elimination", "Lexicographic", "MostLikely", "LeastLikely", 
        "Tallying", "Probable", "Priority")
    competition = competition[order(competition[, 1] - competition[, 
        2], decreasing = T), ]
    if (plot) {
        c2 = apply(competition, 2, function(x) x/apply(competition, 
            1, max))
        b = barplot(t(c2), main = paste0("Option 1 vs Option 2"), 
            beside = T, col = c("black", "lightgrey"), names.arg = rep("", 
                dim(competition)[1]))
        text(colMeans(b), 0, paste0(rownames(c2), "   "), srt = 45, 
            pos = 2, xpd = T, cex = 1)
    }
    return(competition)
}
