# ---- Multinomial in a 3D count cube (static) ----
# Minimal deps: scatterplot3d

# If needed, install:
if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
  install.packages("scatterplot3d")
}
library(scatterplot3d)

# Parameters
n <- 10
p <- c(A = 0.2, B = 0.5, C = 0.3)  # must sum to 1

# All integer triples (A,B,C) with A+B+C = n
grid <- do.call(rbind, lapply(0:n, function(kA) {
  kB <- 0:(n - kA)
  data.frame(A = kA, B = kB, C = n - kA - kB)
}))

# Theoretical multinomial probabilities
grid$prob <- apply(grid[, c("A","B","C")], 1, function(k) {
  dmultinom(k, size = n, prob = p)
})
cat("Check: sum of probabilities =", sum(grid$prob), "\n")  # ~1

# Map probabilities to colors (simple heat scale)
pal <- colorRampPalette(c("lightgray", "skyblue", "royalblue", "red"))
breaks <- seq(min(grid$prob), max(grid$prob), length.out = 6)  # equal-width bins
bin_id <- cut(grid$prob, breaks = breaks, include.lowest = TRUE)
cols <- pal(length(breaks) - 1)[as.integer(bin_id)]

# 3D scatter in a cube (axes are counts A,B,C)
s3d <- scatterplot3d(
  x = grid$A, y = grid$B, z = grid$C,
  color = cols, pch = 16,
  xlab = "Count A", ylab = "Count B", zlab = "Count C",
  xlim = c(0, n), ylim = c(0, n), zlim = c(0, n),
  angle = 200, box = TRUE,
  main = sprintf("Multinomial Counts in 3D (n=%d, p=[%.2f, %.2f, %.2f])", n, p[1], p[2], p[3]),
  sub  = "Color = P(A=kA, B=kB, C=kC); points lie on A+B+C=n"
)

# Mark the mean (np) for reference
s3d$points3d(n * p["A"], n * p["B"], n * p["C"],
             pch = 8, cex = 1.5, col = "black")

# Legend for probability color bins
leg_cols <- pal(length(breaks) - 1)
leg_labs <- paste0(
  format(breaks[-length(breaks)], digits = 2, trim = TRUE),
  "–",
  format(breaks[-1], digits = 2, trim = TRUE)
)
legend("topleft", inset = 0.02, title = "Probability",
       fill = leg_cols, legend = leg_labs, cex = 0.8, bg = "white")

# Optional: draw the triangle edges (A+B+C=n) for visual clarity
# Pick three corners on the plane: (n,0,0), (0,n,0), (0,0,n)
tri <- rbind(c(n,0,0), c(0,n,0), c(0,0,n), c(n,0,0))
s3d$points3d(tri[,1], tri[,2], tri[,3], type = "l", lwd = 2, col = "gray40")


