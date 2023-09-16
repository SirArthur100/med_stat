stripchart(mtcars$wt, method = "jitter", vertical = TRUE, xlim=c(0,4))
boxplot(mtcars$wt, add=TRUE, at = 2, whisklty = 0, staplelty = 0, outline=FALSE)
boxplot(mtcars$wt, add=TRUE, at = 3)
axis(1, at=c(1,2,3), labels=c("jittered data", "box", "box + whiskers"))
