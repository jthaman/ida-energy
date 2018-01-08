library(energy)
library(readr)

## KS test vs Energy

ks_energy_sim_means <- function(nSims = 1000, alpha = 0.05){
    means <- seq(-1.5, 1.5, length.out = 41)
    df <- data.frame(energy = rep(0, length(means)),
                     ks = rep(0, length(means)),
                     means = means)
    for (i in 1:length(means)){
        Qp <- rep(0, nSims)
        ksp <- rep(0, nSims)
        for(j in 1:nSims){
            dat1 <- rnorm(n = 30, mean = 0, sd = 1)
            dat2 <- rnorm(n = 30, mean = means[i], sd = 1)
            fulldat <- c(dat1, dat2)
            Qp[j] <- eqdist.etest(fulldat, c(30,30), R = 1000)$p.value
            ksp[j] <- ks.test(dat1, dat2)$p.value
        }
        df[i,1] <- mean(Qp < alpha)
        df[i,2] <- mean(ksp < alpha)
    }
    df
    
}

res1 <- ks_energy_sim_means(nSims = 1000)
write_csv(res1, "./ks_energy_means.csv")


ks_energy_sim_sd <- function(nSims = 1000, alpha = 0.05){
    sds <- seq(0.3, 3, length.out = 51)
    df <- data.frame(energy = rep(0, length(sds)),
                     ks = rep(0, length(sds)),
                     sds = sds)
    for (i in 1:length(sds)){
        Qp <- rep(0, nSims)
        ksp <- rep(0, nSims)
        for(j in 1:nSims){
            dat1 <- rnorm(n = 30, mean = 0, sd = 1)
            dat2 <- rnorm(n = 30, mean = 0, sd = sds[i])
            fulldat <- c(dat1, dat2)
            Qp[j] <- eqdist.etest(fulldat, c(30,30), R = 1000)$p.value
            ksp[j] <- ks.test(dat1, dat2)$p.value
        }
        df[i,1] <- mean(Qp < alpha)
        df[i,2] <- mean(ksp < alpha)
    }
    df
    
}


res2 <- ks_energy_sim_sd(nSims = 1000)
write_csv(res2, "./ks_energy_sd.csv")
