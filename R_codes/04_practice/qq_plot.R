normal_dist <- rnorm(2000)
lognormal_dist <- rlnorm(2000)

par(mfrow=c(1,2))
quant <- quantile(normal_dist,probs = seq(0.01,0.99,0.01),names=FALSE)
var <- quantile(lognormal_dist,probs = seq(0.01,0.99,0.01),names=FALSE)
plot(quant, var)

plot(density(lognormal_dist)$y, type="l")
lines(density(normal_dist)$y)

