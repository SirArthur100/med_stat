## Bimodal sample + stacked base R plots
set.seed(123)

# 1) Generate a bimodal distribution from two Normals
n1 <- 600; n2 <- 400
x <- c(rnorm(n1, mean = -2,  sd = 0.8),
       rnorm(n2, mean =  2.5, sd = 1.1))

# Shared x-limits for all three panels
rng  <- range(x)
pad  <- diff(rng) * 0.05
xlim <- c(rng[1] - pad, rng[2] + pad)

# 2) Stack the three panels with a common x-axis
op <- par(no.readonly = TRUE)
on.exit(par(op))
par(oma = c(2, 2, 2, 2))

layout(matrix(c(1,2,3), nrow = 3, byrow = TRUE),
       heights = c(1.2, 3, 1.2))   # boxplot / hist / stripchart

## --- TOP: horizontal boxplot (no frame) ---
par(mar = c(0, 4, 2, 1) + 0.1, bty = "n")
boxplot(x,
        horizontal = TRUE, xlim = xlim, xaxt = "n", xlab = "", ylab = "",
        boxwex = 6,            # (from earlier) thicker box
        frame.plot = FALSE)    # remove surrounding plot frame
mtext("Horizontal boxplot", side = 3, line = 0.2, cex = 0.9)

## --- MIDDLE: histogram (keep defaults/frame) ---
par(mar = c(0, 4, 0.5, 1) + 0.1, bty = "o")
hist(x, breaks = 30, xlim = xlim, xaxt = "n", main = "", xlab = "", ylab = "Count")
mtext("Histogram", side = 3, line = 0.2, cex = 0.9)

## --- BOTTOM: stripchart (no frame) ---
par(mar = c(4, 4, 1, 1) + 0.1, bty = "n")
stripchart(x, method = "jitter", xlim = xlim, pch = 19, cex = 0.5,
           xaxt = "s", yaxt = "n", xlab = "Value", ylab = "", col = adjustcolor("black", alpha.f = 0.4))
abline(h = 1, lty = 3)
mtext("Stripchart", side = 3, line = 0.2, cex = 0.9)


