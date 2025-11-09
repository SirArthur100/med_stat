# Bootstrap "spaghetti" around OLS (base R; uses data=subset, not x[idx])
set.seed(123)

# Demo data
n <- 20
x <- runif(n, 0, 10)
y <- 1.5 + 0.7*x + rnorm(n, sd = 2)
df <- data.frame(x = x, y = y)

# Clean & fit
df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
stopifnot(nrow(df) > 1)
m  <- lm(y ~ x, data = df)

# Grid and bootstrap container
xg <- seq(min(df$x), max(df$x), length.out = 80)
B  <- 400
P  <- matrix(NA_real_, nrow = length(xg), ncol = B)

# Bootstrap by resampling rows of df (duplicates allowed)
for (b in seq_len(B)) {
  idx <- sample.int(nrow(df), nrow(df), replace = TRUE)
  fit <- lm(y ~ x, data = df[idx, , drop = FALSE])
  P[, b] <- as.numeric(predict(fit, newdata = data.frame(x = xg)))
}

# Plot
plot(df$x, df$y, pch = 19, cex = 0.6, col = "gray40",
     xlab = "x", ylab = "y",
     main = "Bootstrap 'spaghetti' around linear regression")

matlines(xg, P, lty = 1, lwd = 1, col = rgb(0, 0, 0, 0.08))   # ~200 faint lines
lines(xg, predict(m, newdata = data.frame(x = xg)), lwd = 5, col = "red")

# Optional: 95% bootstrap band
q <- t(apply(P, 1, quantile, probs = c(.025, .975), na.rm = TRUE))
lines(xg, q[, 1], lty = 2, lwd = 3, col = "gray30")
lines(xg, q[, 2], lty = 2, lwd = 3, col = "gray30")

# Quick sanity checks if needed:
# dim(P); length(xg)  # should be 80 x 200 and 80


