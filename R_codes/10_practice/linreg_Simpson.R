# Simpson's paradox (base R, one plot)
set.seed(42)

# Two groups with different intercepts, same positive slope
n  <- 80
xA <- rnorm(n, mean = 1.5, sd = 0.5)
xB <- rnorm(n, mean = 2.0, sd = 0.5)

yA <- 4  + 0.6*xA + rnorm(n, sd = 0.4)   # higher intercept
yB <- 2.5 + 0.6*xB + rnorm(n, sd = 0.4)   # lower intercept

x <- c(xA, xB)
y <- c(yA, yB)
g <- factor(rep(c("A","B"), each = n))

# Fits
m_all <- lm(y ~ x)
m_A   <- lm(y ~ x, subset = g == "A")
m_B   <- lm(y ~ x, subset = g == "B")

# Plot
par(mar = c(4,4,3,1))
cols <- c("steelblue","tomato")
plot(x, y, pch = 19, cex = 0.7, col = cols[as.integer(g)],
     xlab = "x", ylab = "y",
     main = "Simpson's paradox: pooled vs within-group trends")

abline(m_all, lwd = 3, col = "gray25")      # pooled (flat or reversed)
abline(m_A,   lwd = 2, col = cols[1])       # group A (positive)
abline(m_B,   lwd = 2, col = cols[2])       # group B (positive)

legend("topright", bty = "n",
       legend = c(
         paste0("Pooled: slope=", round(coef(m_all)[2], 2),
                "  (R^2=", round(summary(m_all)$r.squared, 2), ")"),
         paste0("Group A: slope=", round(coef(m_A)[2], 2),
                "  (R^2=", round(summary(m_A)$r.squared, 2), ")"),
         paste0("Group B: slope=", round(coef(m_B)[2], 2),
                "  (R^2=", round(summary(m_B)$r.squared, 2), ")")
       ),
       lwd = c(3,2,2), col = c("gray25", cols))


