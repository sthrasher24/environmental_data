require(palmerpenguins) 
install.packages("simpleboot")
require(simpleboot)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

A = droplevels(subset(dat_pen, species != "Chinstrap"))
C = droplevels(subset(dat_pen, species != "Adelie"))
pen_boot = two.boot(na.omit(C$flipper_length_mm), na.omit(A$flipper_length_mm), 
                    'mean', 
                     R = 10000,
)
pen_median = two.boot(na.omit(C$flipper_length_mm), na.omit(A$flipper_length_mm), 
                      'median', 
                      R = 10000,
)
str(pen_median)
str(pen_boot)
hist(pen_boot$t, xlab = 't', main = 'Mean of Flipper Length')
hist(pen_median, xlab = 't', main = 'Median of Flipper Length')
median(pen_median$t)
mean(pen_boot$t)
#standard dev of diff in means 
sd(pen_boot$t)

#95% CI
pen_ci = quantile(pen_boot$t, probs = c(0.035, 0.975))

#ecdf
pen_ecdf = ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)
pen_ecdf(-8)

#tree data 
require(here)
veg = data.frame(read.csv(here('data', 'vegdata.csv')))
veg_treat = droplevels(subset(veg, treatment %in% c('clipped','control')))
#wilcox 
wilcox.test(pine ~ treatment, data = veg_treat, alternative = "two.sided")

x = droplevels(subset(veg_treat, treatment == 'clipped'))
y = droplevels(subset(veg_treat, treatment == 'control'))
tree_boot = two.boot(x$pine, y$pine, 'mean', R = 1000)
str(tree_boot)
tree_ci = quantile(tree_boot$t, probs = c(0.025, 0.975)) 

#import bird data 
require(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

#slope coefficent 
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#Monte Carlo Simulation 
#MC randomization 
m = 10000 
result_mc = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_mc[i] = coef(fit_resampled_i)[2]
} 
print(slope_resampled_i)

hist(result_mc)
#critical slope value 
cv = quantile(result_mc, c(.05))

#create histogram 
hist(
  result_mc,
  main = "Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = cv , lty = 2, col = 'red', lwd = 2)
     
#observed slope 
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
slope_observed

#bootstrap loop 
m = 10000 
result_boot = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  coef(fit_bs1)
  result_boot[i] = coef(fit_bs1)[2]
} 

#plot density 
plot(
  density(result_mc),
  type='l', col= 3, main = "Null and Alternative Distributions",
  xlab='Slope Coefficient', ylab='Density', xlim = c(-.05, 0.03), ylim = c(0,70))
#plot(density(result_mc), main = "Null Distribution Density Plot",xlab = 'Slope Coefficient', ylab = 'Density', col = 3, type = '1')
lines(density(result_boot), col = 2)
legend (0.02, 60, legend = c('Null', 'Alt'), fill = c(3, 2))
