# Utility function: compute η² (effect size measure)
# Measures the proportion of variance of a quantitative variable explained by a factor
eta2_manual <- function(fac, z) {
    fac <- as.factor(fac)
    z <- as.numeric(z)
    ss_total <- sum((z - mean(z))^2)
    if (ss_total == 0) {
        return(0)
    }
    ss_between <- sum(tapply(z, fac, function(v) length(v) * (mean(v) - mean(z))^2))
    return(ss_between / ss_total)
}
