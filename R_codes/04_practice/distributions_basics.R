add_lines_to_plot <- function(mean, median, mode) {
  # this function adds the mean, medianm and mode to the plot
  abline(v=mean, col="blue", lwd=3)
  abline(v=median, col="red", lwd=3)
  abline(v=mode, col="green", lwd=3)
}

create_distribution_plot <- function(sample_vector){
  hist_res = hist(sample_vector, freq=FALSE, plot=FALSE) # create histogram without plotting
  max_height = hist_res$density[which.max(hist_res$counts)] # get maximum height of histogram
  hist(sample_vector, freq=FALSE, ylim = c(0, 1.5 * max_height)) # plot histogram
  dens = density(sample_vector) # create KDE
  lines(dens, lwd=3, lty="dashed") # add KDE to plot
  bx_width = 0.2 * max_height  # calculate box width parameter for aesthetic appearance
  boxplot(sample_vector, horizontal=TRUE, boxwex=bx_width, add=TRUE, at = 1.3 * max_height, ylim = c(0, 1.3 * max_height), pars=list(outcol="red", cex=0.6)) # make boxplot
  add_lines_to_plot(mean(sample_vector), median(sample_vector), dens$x[which.max(dens$y)]) # call function
  legend("topright", legend=c("Mean", "Median", "Mode", "Density"), col=c("blue", "red", "green", "black"), lty=c(1,1,1,2), cex=0.8, box.lty=0) # add legend
}

sample_real = c(rnorm(100, sd=10), runif(30, min = -20, max = 20)) # create a somewhat realistic distribution
sample_normal = rnorm(10000, sd=10) # create normal distribution
sample_lognormal = rlnorm(10000, sd=0.5) # create lognormal distribution

par(mfrow = c(1, 2)) # create grid for plotting
create_distribution_plot(sample_normal)
create_distribution_plot(sample_lognormal)
dev.new() # open new window
create_distribution_plot(sample_real)

