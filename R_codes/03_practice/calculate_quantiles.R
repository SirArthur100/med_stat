sample <- c(3, 6, 7, 8, 8, 10, 13, 15, 16, 20)

length(sample)

sample <- sort(sample)

sample

quantile(sample, type=2)

# 10 * (0/4) = 0.0 => 0
# 10 * (1/4) = 2.5 => 3
# 10 * (2/4) = 5.0 => 5
# 10 * (3/4) = 7.5 => 8
# 10 * (4/4) = 10.0 => 10

