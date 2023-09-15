hist(mtcars$wt, col=rgb(red=1, green=0, blue=0, alpha=0.1), breaks=20, cax=0.1, xlab="Weight", main="Histogram with Strip")
stripchart(mtcars$wt, add = TRUE, pch=19, col="blue", ylim=0)
