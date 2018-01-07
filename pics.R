library(energy)

?curve

curve(pnorm(x), -2, 2)
data <- rnorm(10)
lines(ecdf(data))
fn <- ecdf(data)
lines((pnorm(x) - fn(x))^2, col="red")

x <- seq(0, 3, length.out = 50)
y1 <- x^2 + runif(50, min=-1, max=1)
plot(x,y1)
lines(x,y=x^2)

?mvnorm.etest
