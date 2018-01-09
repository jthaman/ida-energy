library(tidyverse)
setwd("~/Dropbox/talks/IDA talk")


theme_set(theme_bw(
    plot.background = element_rect(fill = "transparent"), 
    panel.background = element_rect(fill = "transparent"), 
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent") 
))



x <- c(1.03, 1.24, 1.47, 1.52, 1.92, 1.93, 1.94, 1.95, 1.96, 1.97, 1.98, 
       1.99, 2.72, 2.75, 2.78, 2.81, 2.84, 2.87, 2.9, 2.93, 2.96, 2.99, 3.6, 
       3.64, 3.66, 3.72, 3.77, 3.88, 3.91, 4.14, 4.54, 4.77, 4.81, 5.62)
df <- data.frame(x=x)

barfill <- "#4271AE"
barlines <- "#1F3552"

# normal plot
ggplot(df, aes(x=x)) +
    ggtitle("Histogram of simulated data (sample size = 34)") +
    geom_histogram(breaks = seq(0.3, 6.7, by = 0.8), colour = barlines, fill = barfill) +
    theme(text = element_text(size=20)) + 
    xlim(c(0,7)) +
    xlab("Data Range") +
    ggsave("hist1.png", width = 8, height = 6)

# skew plot
ggplot(df, aes(x=x)) +
    ggtitle("Histogram of the same data!") +
    theme(text = element_text(size=20)) + 
    geom_histogram(breaks = 0:7,colour = barlines, fill = barfill ) +
    xlim(c(0,7)) +
    xlab("Data Range") +
    ggsave("hist2.png", width = 8, height = 6)

# energy distance
df2 <- data.frame(x = c(1,2,1,2, 2.5,3.5,2.5,3.5),
                  y = c(1,1,2,2, 0.5,1.5,2.5,3.5),
                  colo = c(0,0,0,0,1,1,1,1))

df3 <- rbind(df2,df2)
df4 <- rbind(df3, df2)
df4[["group"]] = c(17,17,18,18,19,19,20,20, 21,22,21,22,23,24,23,24, 25,26,26,25,27,28,28,27)

## Within Group Distance
ggplot(df4, aes(x=x, y=y, group = group)) +
    geom_point(size = 10, aes(shape = factor(colo))) +
    scale_shape_manual(values = c(16, 21)) + 
    geom_line() +
    theme(text = element_text(size=20)) + 
    theme(legend.position="none") +
    ggtitle("Within Color Distance") +
    ggsave("dots1.png", width = 8, height = 6)

### No Distances
ggplot(df4, aes(x=x, y=y)) +
    geom_point(size = 10, aes(shape = factor(colo))) +
    theme(text = element_text(size=20)) + 
    scale_shape_manual(values = c(16, 21)) + 
    theme(legend.position="none") +
    ggtitle("Random Data", "Four Black and White Points") +
    ggsave("dots0.png", width = 8, height = 6)

## Between Group Distances

df3 <- rbind(df2, df2)
df5 <- rbind(df3, df3)
df5[["group"]] = c(1,5,9,13,1,2,3,4,
                   2,6,10,14,5,6,7,8,
                   3,7,11,15,9,10,11,12,
                   4,8,12,16,13,14,15,16)

ggplot(df5, aes(x=x, y=y, group = group)) +
    geom_point(size = 10, aes(shape = factor(colo))) +
    scale_shape_manual(values = c(16, 21)) +
    theme(text = element_text(size=20)) + 
    theme(legend.position="none") +
    geom_line() +
    ggtitle("Between Color Distances") +
    ggsave("dots2.png", width = 8, height = 6)

### combined plot (all distances)
df6 <- rbind(df4, df5)
df7 <- cbind(df6, dd = ifelse(df6$group <= 16, 1, 0 ))

ggplot(df7, aes(x=x, y=y, group = group)) +
    geom_point(size = 10, aes(shape = factor(colo))) +
    scale_color_manual(breaks = c("1", "0"),
                       values=c("red", "blue")) +
    theme(text = element_text(size=20)) + 
    scale_shape_manual(values = c(16, 21)) + 
    geom_line(aes(colour = factor(dd)), size = 1.2) +
    theme(legend.position="none") +
    ggtitle("All Distances") +
    ggsave("dots3.png", width = 8, height =6)

## mis-specified scale
df_w <- read_csv("~/Dropbox/talks/IDA talk/sim_data_b.csv")
bs <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2)
df_w <- reshape2::melt(df_w)
df_w$b <- rep(bs, 4)
df_w <- rename(df_w, Method = variable)

ggplot(df_w, aes(x = b, y=value, group = Method, color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(color = Method), size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1.5) + 
    geom_vline(xintercept = 1, linetype = "dashed", size = 1.5) + 
    theme(text = element_text(size=20)) + 
    ggtitle("Test of Misspecified Scale Parameter", "Sample Size 30, 1000 replicates") +
    xlab("Scale Parameter") +
    annotate("text", x = 1.75, y = 0.1, label = "P(Type I error) = 0.05") +
    ylab("Power") +
    aes(group=rev(Method)) +
    ggsave("weibull_power.png", width = 8, height = 6)

## mis-specified shape

df_w2 <- read_csv("~/Dropbox/talks/IDA talk/sim_data_a.csv")
as <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2)
df_w2 <- reshape2::melt(df_w2)
df_w2$a <- rep(as, 4)
df_w2 <- rename(df_w2, Method = variable)

ggplot(df_w2, aes(x = a, y=value, group = Method, color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(color = Method), size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1.5) + 
    geom_vline(xintercept = 1, linetype = "dashed", size = 1.5) + 
    theme(text = element_text(size=20)) + 
    ggtitle("Test of Misspecified Shape Parameter", "Sample Size 30, 1000 replicates") +
    xlab("Shape Parameter") +
    ylab("Power") +
    aes(group=rev(Method)) +
    annotate("text", x = 1.75, y = 0.1, label = "P(Type I error) = 0.05") +
    ggsave("weibull_power_shape.png", width = 8, height = 6)


## weibull P-values follow uniform
pvals <- as.data.frame(pvals)
write_csv(pvals, "./pvals.csv")

ggplot(pvals, aes(x = pvals)) +
    xlab("P-values of Energy Test") +
    theme(text = element_text(size=20)) + 
    ggtitle("P-values are uniform when data is truly Weibull", "Energy Test of uniformity : (p = 0.94)") +
    geom_histogram(breaks = c(0:10)/10, colour = barlines, fill = barfill) +
    ggsave("pvals.png", width = 8, height = 6)


### Scatter Plot
rAML <- function (n, theta=c(0,0), m=c(0,0), Sigma=diag(2)) {
    if (missing(Sigma)) Sigma <- diag(ncol(m))
    Sigma <- as.matrix(Sigma)
    d <- NROW(Sigma)
    if (n < d) stop("in rAML: n must be larger than d")
    thetas <- matrix(theta, n, d, byrow = TRUE)
    ms <-  matrix(m, n, d, byrow = TRUE)
    Z <- matrix(rexp(n, 1), n, d)
    X <- mvrnorm(n, rep(0, d), Sigma)
    Y <- thetas + ms*Z + sqrt(Z) * X
    return(Y)
}

sigma <- matrix(c(1,.5,.5,1), nrow=2)

dat_norm <- mvrnorm(n=500, mu=c(0,0), Sigma = sigma) %>%
    as_data_frame %>%
    mutate(label = "My Data") %>%
    rename(x = V1 ) %>%
    rename(y = V2)

dat_laplace <- rAML(n=500, theta = c(0,0), m=c(0,0), Sigma = sigma) %>%
    as_data_frame %>%
    mutate(label = "Someone Else's Data") %>%
    rename(x = V1 ) %>%
    rename(y = V2) %>%
    bind_rows(dat_norm)

ggplot(dat_laplace, aes(x = x, y = y)) +
    theme(text = element_text(size=20)) + 
    xlab(label = "") +
    ylab(label = "") +
    xlim(-4,4) +
    ylim(-4,4) +
    geom_point(size = 1.5)+
    facet_wrap(~ label) +
    ggsave("scatter.png", width = 8, height = 6)

## Two ECDF

df_e <- data.frame(
    x = c(rnorm(25, 0, 3), rnorm(25, 2, 10)),
    Sample = gl(2, 25)
)

ggplot(df_e, aes(x, colour = Sample)) +
    ggtitle("Two Samples of Data") +
    ylab("Probability") +
    xlab("Data Range") +
    stat_ecdf(size = 1.5) +
    theme(text = element_text(size=20)) + 
    ggsave("two_ecdf.png", width = 8, height = 6)


## ECDF vs CDF
df_e1 <- data.frame(
    x = rnorm(25, 0, 1))

ggplot(df_e1, aes(x)) +
    ggtitle("CDF vs eCDF") +
    theme(text = element_text(size=20)) + 
    xlab("Data Range") +
    ylab("Probability") +
    stat_ecdf(size = 1.5, color = "red") +
    stat_function(fun = "pnorm", size = 1.5, color = "blue") +
    ggsave("one_ecdf.png", width = 8, height = 6)


ggplot(df_e1, aes(x)) +
    ggtitle("CDF vs eCDF") +
    theme(text = element_text(size=20)) + 
    xlab("Data Range") +
    ylab("Probability") +
    stat_ecdf(size = 1.5, color = "red") +
    stat_function(fun = "pnorm", size = 1.5, color = "blue") +
    ggsave("one_ecdf.png", width = 8, height = 6)


sample1 <- rnorm(25, 10, 5)
sample2 <- rnorm(10000, 1, 5)
group <- c(rep("Sample Data", length(sample1)), rep("Hypothesized Distribution", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    theme(legend.position ="top") +
    theme(text = element_text(size=20)) + 
    xlab("Sample") +
    ylab("ECDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
    ggtitle("Kolmogorov-Smirnov (KS) Test") +
    theme(legend.title=element_blank()) +
    ggsave("ks_one_sample.png", width = 8, height = 6)


ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    theme(legend.position ="top") +
    theme(text = element_text(size=20)) + 
    xlab("Sample") +
    ylab("ECDF") +
    #geom_line(size=1) +
    ggtitle("Sample Data and Hypothesized Distribution") +
    theme(legend.title=element_blank()) +
    ggsave("ecdf.png", width = 8, height = 6)



e1 <- ecdf(dat[group == "Sample Data",]$KSD)
e2 <- ecdf(dat[group == "Hypothesized Distribution",]$KSD)

sort(e1(dat[group == "Sample Data",]$KSD))
sort(e2(dat[group == "Hypothesized Distribution",]$KSD))

dat <- data.frame(KSD = c(sample1,sample2), group = group)

dat2 <- data.frame(
    sample1 = sort(sample1),
    sample2 = sort(sample2)
)

ecdf1 = rep(sort(e1(dat[group == "Sample Data",]$KSD)), 400)
ecdf2 = sort(e2(dat[group == "Hypothesized Distribution",]$KSD))
ecdf <- c(ecdf1, ecdf2)

e1(grid)
e2(grid)
grid <- seq(min(sample2), max(sample2), length.out = 10000)

dat2 <- data.frame(
    grid = grid, 
    data = e1(grid),
    group = "Sample"
)

dat3 <- data.frame(
    grid = grid,
    data = e2(grid),
    group = "Hypothesized Distribution"
)

dat4 <- rbind(dat2, dat3)


ggplot(dat4, aes(x = grid, y = data, group = group, color = group))+
    geom_line(size=1) +
    theme(legend.position ="top") +
    theme(text = element_text(size=20)) + 
    xlab("Sample") +
    ylab("ECDF") +
    geom_ribbon(aes(ymin = rep(subset(dat4, group == "Sample")$data, 2)
                  , ymax = rep(subset(dat4, group == "Hypothesized Distribution")$data, 2)), alpha = 0.3) +
    ggtitle("L2 Tests: Anderson-Darling, Cramer von-Mises") +
    theme(legend.title=element_blank()) +
    geom_line(size=1) +
    ggsave("ad_one_sample.png", width = 8, height = 6)



rep(subset(dat4, group == "Sample")$data, 2)
rep(subset(dat4, group == "Hypothesized Distribution")$data, 2)

## KS test TWO SAMPLE
sample1 <- rnorm(25, 10, 5)
sample2 <- rnorm(25, 1, 5)
group <- c(rep("Sample 1", length(sample1)), rep("Sample 2", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    theme(legend.position ="top") +
    theme(text = element_text(size=20)) + 
    xlab("Sample") +
    ylab("ECDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
    ggtitle("Kolmogorov-Smirnov (KS) Test -- Two Samples") +
    theme(legend.title=element_blank()) +
    ggsave("ks_two_sample.png", width = 8, height = 6)

## Weibull pics

dat0 <- seq(0, 5, length.out = 1000)
dat0 <- data.frame(x = dat0)

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 2
cols = gg_color_hue(2)
cols

ggplot(dat0, aes(x)) +
    ggtitle("Exponential curves (Shape = 1)", "Scale = 2 (black) Scale = 2 (red), Scale = 0.5 (blue)") +
    theme(text = element_text(size=20)) + 
    xlab("Data") +
    ylab("Density") +
    stat_function(fun = "dweibull", args = list(shape = 1, scale = 1), size = 1.2, color = "black") +
    stat_function(fun = "dweibull", args = list(shape = 1, scale = 2), size = 1.2, color = cols[1]) +
    stat_function(fun = "dweibull", args = list(shape = 1, scale = 0.5), size = 1.2, color = cols[2]) +
    ggsave("weibulls1.png", width = 8, height = 6)


ggplot(dat0, aes(x)) +
    ggtitle("Weibull Curves (Shape = 0.5)", "Scale = 1 (black), 2 (red)") +
    theme(text = element_text(size=20)) + 
    xlab("Data") +
    ylab("Density") +
    stat_function(fun = "dweibull", args = list(shape = 0.5, scale = 1), size = 1.2, color = "black") +
    stat_function(fun = "dweibull", args = list(shape = 0.5, scale = 2), size = 1.2, color = cols[1]) +
    ggsave("weibulls2.png", width = 8, height = 6)


## shape = 0.5 power graph
df_t <- read_csv("~/Dropbox/talks/IDA talk/sim_data_b_shape0_5.csv")
bs <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2)
df_t <- reshape2::melt(df_t)
df_t$b <- rep(bs, 4)
df_t <- rename(df_t, Method = variable)

ggplot(df_t, aes(x = b, y=value, group = Method, color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(color = Method), size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1.5) + 
    geom_vline(xintercept = 1, linetype = "dashed", size = 1.5) + 
    theme(text = element_text(size=20)) + 
    ggtitle("Test of Misspecified Scale Parameter", "Sample Size 30, 1000 replicates") +
    xlab("Scale Parameter") +
    annotate("text", x = 1.75, y = 0.1, label = "P(Type I error) = 0.05") +
    ylab("Power") +
    aes(group=rev(Method)) +
    ggsave("weibull_power_shape0_5.png", width = 8, height = 6)


## Energy vs KS

res1 <- read_csv("ks_energy_means.csv")
means <- res1$means
res1 <- subset(res1, select = -c(means))
res1 <- reshape2::melt(res1)
res1$means <- rep(means, 2)
res1 <- rename(res1, Method = variable)

res2 <- read_csv("ks_energy_sd.csv")
sds <- res2$sds
res2 <- subset(res2, select = -c(sds))
res2 <- reshape2::melt(res2)
res2$sds <- rep(sds, 2)
res2 <- rename(res2, Method = variable)
head(res2)

ggplot(res1, aes(x = means, y=value, group = Method, color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(color = Method), size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1.5) + 
    geom_vline(xintercept = 0, linetype = "dashed", size = 1.5) + 
    theme(text = element_text(size=20)) + 
    ggtitle("Samples with different means", "Sample Size 30, 1000 replicates") +
    xlab("mean difference") +
    annotate("text", x = 1, y = 0.1, label = "P(Type I error) = 0.05") +
    ylab("Power") +
    aes(group=rev(Method)) +
    ggsave("ks_energy_means.png", width = 8, height = 6)


ggplot(res2, aes(x = sds, y=value, group = Method, color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(color = Method), size = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1.5) + 
    geom_vline(xintercept = 1, linetype = "dashed", size = 1.5) + 
    theme(text = element_text(size=20)) + 
    ggtitle("Samples with different spreads", "Sample Size 30, 1000 replicates") +
    xlab("std. dev. ratio") +
    annotate("text", x = 2.5, y = 0.1, label = "P(Type I error) = 0.05") +
    ylab("Power") +
    aes(group=rev(Method)) +
    ggsave("ks_energy_sds.png", width = 8, height = 6)
