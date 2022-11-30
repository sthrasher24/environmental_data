ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
                   
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

#create expoential curves 
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)", col = 'black', lty = 'solid'); box()
curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)", col = 'black', lty = 'dotted'); box()
curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)", col = 'red', lty = 'solid'); box()
curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)", col = 'red', lty = 'dotted'); box()

#plotting Ricker curves 
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 25, 0.2), add = FALSE, ylim = c(0,94), from = 0, to = 70, col = 'black', lty = 'solid')
curve(
  ricker_fun(x, 20, 0.2), add = TRUE, ylim = c(0,94),from = 0, to = 70, col = 'black', lty = 'dotted')
curve(
  ricker_fun(x, 10, 0.2), add = TRUE, ylim = c(0,94),from = 0, to = 70, col = 'black', lty = 'dotted')
curve(
  ricker_fun(x, 75, 0.3), add = TRUE, ylim = c(0,94),from = 0, to = 70, col = 'red', lty = 'solid')
curve(
  ricker_fun(x, 50, 0.3), add = TRUE, ylim = c(0,94),from = 0, to = 70, col = 'red', lty = 'dotted')
curve(
  ricker_fun(x, 40, 0.3), add = TRUE, ylim = c(0,94),from = 0, to = 70, col = 'red', lty = 'dotted')

#reading in salamnder data 
require(here)
dat_dispersal = data.frame(read.csv(here("data","dispersal.csv")))
#plotting linear salamder data 
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

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n(Bad) linear model")
curve(line_point_slope(x, 1100, 0.15, -0.0004), add = TRUE)

#plot the exponential model 
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n(Bad) linear model")
curve(
  exp_fun(x, 2, 0.0034), add = TRUE, from = 0, to = 1500,
  ann = FALSE, ylab = "f(x)", col = 'black', lty = 'solid'); box()

#Ricker function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n(Bad) linear model")
curve(
  ricker_fun(x, 0.004, 1/330), add = TRUE, ylim = c(0,94), from = 0, to = 1500, col = 'black', lty = 'solid')

#calculate residuals (predicted - observed)
dat_dispersal$predicted_linear = line_point_slope(dat_dispersal$dist.class, 1100, 0.15, -.0004)
dat_dispersal$resids_linear = dat_dispersal$disp.rate.ftb - dat_dispersal$predicted_linear
dat_dispersal$predicted_exp = exp_fun(dat_dispersal$dist.class, 2, 0.0034)
dat_dispersal$resids_exp = dat_dispersal$disp.rate.ftb - dat_dispersal$predicted_exp
dat_dispersal$predicted_ricker = ricker_fun(dat_dispersal$dist.class, 0.004, 0.003)
dat_dispersal$resids_ricker = dat_dispersal$disp.rate.ftb - dat_dispersal$predicted_ricker

#histograms 
par(mfrow = c(1,3))
hist(dat_dispersal$resids_linear, ylab = "Frequency", xlab = "Linear")
hist(dat_dispersal$resids_exp, ylab = "Frequency", xlab = "Exponential")
hist(dat_dispersal$resids_ricker, ylab = "frequency", xlab = "Ricker") 

