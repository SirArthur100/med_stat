pdf_cdf = function(x, y){
par(mar = c(5, 4, 4, 4) + 0.3)
plot(x, y, type="h", lwd=2, ylab="PDF/PMF (Probability Density/Mass Function)")
points(x, y, type="p", add=TRUE, pch=19)
par(new = TRUE)
plot(x, cumsum(y), axes = FALSE, type="s", col="blue", lwd=4, ylab="")
axis(side=4, at = pretty(cumsum(y)), col="blue",col.axis="blue")
mtext("CDF (Cumulative Density Function)",side=4,col="blue",line=2)
}

success <- 0:20
pdf_cdf(success, dbinom(success, size=20, prob=.3))

x = seq(-100,100,length.out = 100)
pdf_cdf(x, dnorm(x, mean=0, sd=20))

