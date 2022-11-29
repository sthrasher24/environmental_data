#x_bin = 0:4
y_bin_2 = pnorm(1, 0, 1, lower.tail = TRUE)

barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 6, p = 0")

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

par(mfrow = c(1, 2))
y_cdf_1 = pnorm(x, mean = 0, sd = 1)
y_cdf_2 = pnorm(x, mean = 0, sd = 2)
y_cdf_3 = pnorm(x, mean = -2, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density", main = "CDF Plot")
lines(y_cdf_2 ~ x, type = "l", ylab = "cumulative density", lty = 'dotted')
lines(y_cdf_3 ~ x, type = "l", ylab = "cumulative density", lty = 'dashed')

n = 1000
x = seq(from = -6, to = 6, length.out = n)
y = dnorm(x, mean = 0, sd = 1)
y2 = dnorm(x, mean = 0, sd = 2)
y3 = dnorm(x, mean = -2, sd = 1)
plot(y ~ x, type = "l", ylab = "Probability Density", main = "PDF Plot")
lines(y2 ~ x, type = "l", ylab = "Probability Density", lty = 'dotted')
lines(y3 ~ x, type = "l", ylab = "Probability Density", lty = 'dashed')

