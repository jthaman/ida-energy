library(readr)
library(here)

pvals <- read_csv(here::here("pvals.csv"))

pvals <- pvals$pvals

EXX_uniform <- function(){1/3}
EXy_uniform <- function(x) {x^2 -x + 1/2}

Qn_uniform <- function(dat){
    n <- length(dat)
    sorted <- sort(dat)
    lin <- sum((2*seq(1:n)-1-n)*sorted)
    EnergyStat <- n*(2*mean(EXy_uniform(dat)) - EXX_uniform() - (2/(n^2))*lin)
    EnergyStat
}

stat <- Qn_uniform(pvals)
stat

Q.pvalue_uniform <- function(stat, dat){
    n <- length(dat)
    Q <- rep(NA, 10000)
    for(i in 1:10000){
        dat <- runif(n)
        Q[i] <- Qn_uniform(dat)
    }
    mean(Q > stat)
}

Q.pvalue_uniform(stat, pvals)
