par(mfrow=c(1,3))
sample_norm = rnorm(100000)
hist(sample_norm, breaks=10, main="")
hist(sample_norm, breaks=50, main="")
mtext("From discrete to continuous",side=3,col="black", line=1, cex=1.5)
hist(sample_norm, breaks=500, main="")

