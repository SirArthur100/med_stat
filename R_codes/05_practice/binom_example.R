## Data and model
x <- c(0,1,1,1,2,2,2,3,3,4)   # counts per cage
n <- 10                       # rats per cage
p <- 0.19
k <- 0:n
pmf <- dbinom(k, size = n, prob = p)  # exact Binomial probabilities

## Discrete bins centered on integers
brks <- seq(-0.5, n + 0.5, by = 1)

## Precompute for common y-limit on probability scale
h <- hist(x, breaks = brks, right = FALSE, plot = FALSE)
ylim <- c(0, max(h$density, pmf) * 1.1)

## Empirical probabilities (blue)
hist(
  x, breaks = brks, right = FALSE, prob = TRUE,
  col = rgb(0, 0, 1, 0.35), border = "white",
  xlab = "Side-effects per cage (out of 10)",
  ylab = "Probability",
  main = "Empirical vs Exact Binomial Probabilities (n = 10, p = 0.19)",
  ylim = ylim
)
axis(1, at = 0:n)

## Exact probabilities as an overlaid “histogram” (red, robust/exact via dbinom)
rect(
  xleft = k - 0.5, ybottom = 0, xright = k + 0.5, ytop = pmf,
  col = rgb(1, 0, 0, 0.25), border = "red", lwd = 2
)

## Optional: mark the PMF points and vertical sticks for clarity
points(k, pmf, pch = 16, col = "red")
segments(k, 0, k, pmf, col = "red", lwd = 2)

legend(
  "topright",
  legend = c("Empirical (10 cages)", "Exact Binomial PMF"),
  fill = c(rgb(0, 0, 1, 0.35), rgb(1, 0, 0, 0.25)),
  border = c("white", "red"),
  bty = "n"
)

