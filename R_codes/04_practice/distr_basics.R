add_lines_to_plot <- function(mean, median, mode) {
  abline(v=mean, col="blue", lwd=3)
  abline(v=median, col="red", lwd=3)
  abline(v=mode, col="green", lwd=3)
}

create_distribution_plot <- function(sample_vector){
  hist_res = hist(sample_vector, freq=FALSE, plot=FALSE)
  max_height = hist_res$density[which.max(hist_res$counts)]
  hist(sample_vector, freq=FALSE, ylim = c(0, 1.5 * max_height))
  dens = density(sample_vector)
  lines(dens, lwd=3, lty="dashed")
  # calculate box width parameter
  bx_width = 0.2 * max_height
  boxplot(sample_vector, horizontal=TRUE, boxwex=bx_width, add=TRUE, at = 1.3 * max_height, ylim = c(0, 1.3 * max_height), pars=list(outcol="red", cex=0.6))
  add_lines_to_plot(mean(sample_vector), median(sample_vector), dens$x[which.max(dens$y)])
  legend("topright", legend=c("Mean", "Median", "Mode", "Density"), col=c("blue", "red", "green", "black"), lty=c(1,1,1,2), cex=0.8, box.lty=0)
}

sample_real = c(rnorm(100, sd=10), runif(30, min = -20, max = 20))
sample_normal = rnorm(10000, sd=10)
sample_lognormal = rlnorm(10000, sd=0.5)

par(mfrow = c(1, 2))
create_distribution_plot(sample_normal)
create_distribution_plot(sample_lognormal)
dev.new()
create_distribution_plot(sample_real)

