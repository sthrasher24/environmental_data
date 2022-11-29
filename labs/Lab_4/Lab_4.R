# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = FALSE)
sd(penguins$body_mass_g, na.rm = FALSE)
nrow(penguins)

n_pts        = 344
penguin_mean = 4202
penguin_sd   = 802

dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

#create random dataframe
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


#Lab 4 
norm_17 = rnorm(n = 17, mean = 10.4, sd = 2.4)
norm_30 = rnorm(n = 30, mean = 10.4, sd = 2.4)
norm_300 = rnorm(n = 300, mean = 10.4, sd = 2.4)
norm_3000 = rnorm(n = 3000, mean = 10.4, sd = 2.4)

#histograms of norms
require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180)
par(mfrow = c(2, 2))

hist(norm_17,main="17 Points")
hist(norm_30,main="30 Points")
hist(norm_300,main="300 Points")
hist(norm_3000,main="3000 Points")
dev.off()

# Generate a vector of x-values
svg("norm_1.svg")

x = seq(0, 30, length.out = 1000)
y = dnorm(x, mean= 10.4, sd = 2.4)

plot(x, y, main = "Standard Normal PDF with 10.4 mean nd 2.4 SD", type = "l", xlim = c(1, 18))
abline(h = 0)

#create random dataframe
png(
  filename = here("lab_04_random_plots.png"),
  width = 1500, height = 1600, 
  res = 180)
par(mfrow=c(2,2))

n_pts = 10
x_min = 1
x_max = 10
x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)
dat_random1 = data.frame(x = x_random, y = y_random)
plot(y~x, data = dat_random, pch = 8, main="Scatterplot 10 points")

n_pts = 30
x_min = 1
x_max = 30
x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)
dat_random2 = data.frame(x = x_random, y = y_random)
hist(x_random, col = "red",main="Histogram of 30 points")

n_pts = 20
x_min = 1
x_max = 20
x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)
dat_random3 = data.frame(x = x_random, y = y_random)
plot(y~x, data = dat_random3, pch = 5, col="blue", main="Scatterplot 20 points")

n_pts = 50
x_min = 1
x_max = 50
x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)
dat_random4 = data.frame(x = x_random, y = y_random)
plot(y~x, data = dat_random4, pch = 10, col="chartreuse", main="Scatterplot 50 points")

dev.off()


#Q 10 
png(filename = here("lab_04_linear_fit_2.png"),
    width = 1500, height = 1600, 
    res = 180)
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

plot(y~x, data = dat_random3, pch = 9, main="Scatterplot 20 points with linear fit")
curve(line_point_slope(x, 7, 0, .05), add = T)

#predicted y values
y_predicted = line_point_slope(dat_random3$x, 7,0,0.05)
y_predicted
#add y_predicted as column 
dat_random3$y_predicted = y_predicted 
dat_random3
#create residuals (predicted - observed)
resids = y_predicted - y_random
resids
sum(resids)
#add residual column 
dat_random3$resids = resids 
dat_random3

#Q 14 graphs
par(mfrow=c(1,2))
hist(dat_random3$resids, xlab="Residuals", main = "Histogram of Model Residuals")
plot(x = dat_random3$y_predicted, y = dat_random3$resids, xlab="Predicted Values", ylab="Residuals", main="Residual vs Predicted Scatterplot")
legend("bottomright", title="Legend",legend="Predicted Values",pch= 1, cex=0.7)
