# Animated bootstrap "spaghetti" (base R) → frames + optional ffmpeg GIF
set.seed(123)

# Parameters
Nmax <- 80          # last frame will have n = Nmax
B    <- 200         # bootstrap lines per frame (bump if you like)
fps  <- 12
frame_dir <- "frames_spaghetti"
gif_out   <- "bootstrap_spaghetti.gif"
dir.create(frame_dir, showWarnings = FALSE)

# Pre-generate full dataset; each frame uses first n rows
x_full <- runif(Nmax, 0, 10)
y_full <- 1.5 + 0.7*x_full + rnorm(Nmax, sd = 2)
df_full <- data.frame(x = x_full, y = y_full)

# Fixed grid and axes (avoid flicker)
xg <- seq(min(df_full$x), max(df_full$x), length.out = 80)
m_full <- lm(y ~ x, data = df_full)
pad <- 1.5 * sd(resid(m_full))
yhat_full <- predict(m_full, newdata = data.frame(x = xg))
ylim_fixed <- range(c(df_full$y, yhat_full)) + c(-pad, pad)
xlim_fixed <- range(xg)

png_path <- function(n) sprintf("%s/frame_%03d.png", frame_dir, n)

# Make frames (n grows by 1 each time)
for (n in 5:Nmax) {
  df <- df_full[1:n, , drop = FALSE]
  m  <- lm(y ~ x, data = df)

  # Bootstrap predictions on fixed grid
  P <- matrix(NA_real_, nrow = length(xg), ncol = B)
  for (b in seq_len(B)) {
    idx <- sample.int(nrow(df), nrow(df), replace = TRUE)
    fit <- lm(y ~ x, data = df[idx, , drop = FALSE])
    P[, b] <- as.numeric(predict(fit, newdata = data.frame(x = xg)))
  }
  q <- t(apply(P, 1, quantile, probs = c(.025, .975)))

  # Draw frame
  png(png_path(n), width = 900, height = 600, res = 96)
  par(mar = c(4,4,3,1))
  plot(df$x, df$y, pch = 19, cex = 0.7, col = "gray40",
       xlab = "x", ylab = "y",
       xlim = xlim_fixed, ylim = ylim_fixed,
       main = sprintf("Bootstrap 'spaghetti' around OLS (n = %d)", n))
  matlines(xg, P, lty = 1, lwd = 1, col = rgb(0,0,0,0.08))
  lines(xg, predict(m, newdata = data.frame(x = xg)), lwd = 4, col = "red")
  lines(xg, q[, 1], lty = 2, lwd = 2, col = "gray30")
  lines(xg, q[, 2], lty = 2, lwd = 2, col = "gray30")
  dev.off()
}


# ---- 3) Stitch frames into a GIF (robust ffmpeg version) -----------------
  
fps  <- 6

ff <- Sys.which("ffmpeg")
if (nzchar(ff)) {
  frames <- list.files(frame_dir, pattern = "^frame_\\d{3}\\.png$", full.names = TRUE)
  if (!length(frames)) stop("No frames found in ", frame_dir)
  frames <- frames[order(frames)]
  start_n <- as.integer(sub(".*_(\\d{3})\\.png$", "\\1", basename(frames[1])))

  # Build palette then use it, honoring the nonzero start frame
  cmd1 <- sprintf('%s -y -framerate %d -start_number %d -i "%s/frame_%%03d.png" -vf "palettegen" "%s/palette.png"',
                  ff, fps, start_n, frame_dir, frame_dir)
  cmd2 <- sprintf('%s -y -framerate %d -start_number %d -i "%s/frame_%%03d.png" -i "%s/palette.png" -lavfi "paletteuse" "%s"',
                  ff, fps, start_n, frame_dir, frame_dir, gif_out)
  system(cmd1); system(cmd2)

  if (file.exists(gif_out)) {
    message("Wrote GIF: ", normalizePath(gif_out))
  } else {
    warning("ffmpeg ran but did not produce ", gif_out, ". Check console output above.")
  }
} else {
  message("ffmpeg not found. Use the Python fallback with imageio.")
}

