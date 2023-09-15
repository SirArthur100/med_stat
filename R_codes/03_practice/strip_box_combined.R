stripchart(mtcars$wt, method = "jitter", vertical = TRUE, xlim=c(0,3))
boxplot(mtcars$wt, add=TRUE, at = 2)
