# ----- Parameters you can tweak -----
n <- 30                 # number of trials
ps <- seq(0.1, 0.9, by = 0.1)  # p values from 0 to 1 in steps of 0.1
res <- 1200             # resolution for the smooth x grid

# ----- Continuous (Gamma-extended) binomial mass -----
# f(x; n, p) = C(n, x) * p^x * (1-p)^(n-x) with C(n, x) via Gamma
cbinom <- function(x, n, p) {
  # handle open-interval case for numerical stability
  logf <- lgamma(n + 1) - lgamma(x + 1) - lgamma(n - x + 1) +
          x * log(p) + (n - x) * log1p(-p)
  exp(logf)
}
cols <- grDevices::rainbow(length(ps))

# ----- Prepare x grid and y-limits -----
x <- seq(0, n, length.out = res)
# Compute a safe y-maximum across p in (0,1); include 1 for the degenerate edges p=0,1
mid_ps <- ps[ps > 0 & ps < 1]
ymax <- if (length(mid_ps)) {
  max(sapply(mid_ps, function(p) max(cbinom(x, n, p))), na.rm = TRUE)
} else 0
ymax <- max(0, ymax) * 1.05  # a bit of headroom


# ----- Base plot scaffold -----
op <- par(mar = c(5, 5, 3.5, 10), xaxs = "i", yaxs = "i")  # extra right margin for legend
on.exit(par(op), add = TRUE)

plot(NA, xlim = c(0, n), ylim = c(0, ymax),
     xlab = "x (treated as continuous)",
     ylab = "Generalized binomial mass",
     main = sprintf("Binomial(n = %d): Continuous curves across p", n),
     cex.main = 1.15, cex.lab = 1.1, cex.axis = 0.95, bty = "n")

# ----- Draw curves for each p -----
for (i in seq_along(ps)) {
  p <- ps[i]
  col_i <- cols[i]
  if (p == 0) {
    # Degenerate at x = 0 with mass 1
    points(0, 1, pch = 19, col = col_i, cex = 1.3)
  } else if (p == 1) {
    # Degenerate at x = n with mass 1
    points(n, 1, pch = 19, col = col_i, cex = 1.3)
  } else {
    lines(x, cbinom(x, n, p), col = col_i, lwd = 2.4)
  }
}

# ----- Legend (placed in right margin) -----
par(xpd = NA)  # allow drawing in the margin
legend(x = par("usr")[2], y = ymax,
       legend = sprintf("p = %.1f", ps),
       col = cols, lwd = 2.0, pch = c(rep(NA, length(ps) - 2), 19, 19),
       pt.cex = 1.1, bty = "n", xjust = 3, yjust = 0.9,
       title = "Probability p", cex = 0.9)

