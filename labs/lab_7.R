require(palmerpenguins)
rm(list = ls())
dat_pen = subset(penguins, species != "Adelie")
dat_pen2 = subset(dat_pen, species != "Chinstrap")
# Choose significance level
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(dat_pen2$bill_length_mm))
print(n)

sd_calc = sd(!is.na(dat_pen2$bill_length_mm))
print(sd_calc)

sse = sd(dat_pen2$bill_length_mm, na.rm = TRUE) / sqrt(n)
print(sse)

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))
print(t_crit)

# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
bill_length_mm_ci = c(
  lower = mean(!is.na(dat_pen2$bill_length_mm)) - ci_radius,
  upper = mean(!is.na(dat_pen2$bill_length_mm)) + ci_radius)

print(round(bill_length_mm_ci, 4))

#Bootstrap CI 
# numeric() creates an vector of length m with all values initialized to zero
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
myboot = 
  boot(
    data = dat_pen2$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

quantile(
  myboot$t,
  c(0.025, 0.975))

str(myboot)
mean(!is.na(dat_pen2$bill_length_mm))

rm(list = ls())

#graph
# Re-read my data:
#peng = read.csv(here("data", "moths.csv"))
#moth_dat = moths[,-1]

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(moth_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  n = nrow(moth_dat) #number of rows or sample observations
  m = 10000 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main="Rarefaction curve")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

#SSE = 3.14 and n = 50 and mean is 10
alpha = 0.05
n = 50 
t_crit = abs(qt(alpha / 2, df = n - 1))
print(t_crit)


set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)
sum(log(dnorm(vec_rnorm, mean = 0, sd = 1)))
vec_rnorm = rnorm(n = 10, mean = 1, sd = .07)

like = dnorm(vec_rnorm, 0, 2)
log_vec = sum(log(like))
log_vec


