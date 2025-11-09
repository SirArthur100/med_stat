# Confounding demo: base R only
set.seed(1)
n <- 200
z <- rnorm(n)                     # true driver (confounder)
x <- z + rnorm(n, sd = 1.2)       # correlated with z
y <- 2*z + rnorm(n, sd = 0.8)     # y depends on z, not really on x

m1 <- lm(y ~ x)
m2 <- lm(y ~ x + z)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

## ---- 1) Simple regression: y ~ x (appears predictive) -------------------
plot(x, y, pch = 19, cex = 0.7, xlab = "x", ylab = "y",
     main = "Simple regression: y ~ x")
abline(m1, lwd = 2, col = "gray30")
legend("topleft", bty = "n",
       legend = paste("R^2 =", round(summary(m1)$r.squared, 2)))

## ---- 2) Multiple regression with confounder: y ~ x + z -----------------
# Fit plane on a grid
b <- coef(m2)
xs <- seq(min(x), max(x), length.out = 30)
zs <- seq(min(z), max(z), length.out = 30)
plane <- outer(xs, zs, function(a, g) b[1] + b[2]*a + b[3]*g)

# Draw plane and project points into the same perspective
pr <- persp(xs, zs, plane, theta = 40, phi = 25, expand = 0.7,
            col = "gray90", shade = 0.5, border = NA,
            xlab = "x", ylab = "z", zlab = "y",
            main = "Multiple regression: y ~ x + z (fitted plane)")

pt <- trans3d(x, z, y, pr)        # base R projection helper
points(pt$x, pt$y, pch = 19, cex = 0.6)

mtext(paste0("R^2 = ", round(summary(m2)$r.squared, 2),
             "   β_x = ", round(b[2], 2),
             "   β_z = ", round(b[3], 2)),
      side = 3, line = -2, adj = 0.5, cex = 0.8)

## (Optional) Console readout:
cat("\nSimple model  R^2:", round(summary(m1)$r.squared, 3),
    "\nMultiple model R^2:", round(summary(m2)$r.squared, 3),
    "\nCoefficients (y ~ x + z):\n", round(b, 3), "\n")
