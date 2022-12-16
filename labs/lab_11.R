require(here)
dat_bird = data.frame(read.csv(here('data', 'bird.sub.csv')))
dat_hab = data.frame(read.csv(here('data', 'hab.sub.csv')))
bird_hab = merge(dat_bird, dat_hab)

#scatterplot 
plot(bird_hab$ls, bird_hab$BRCR)
#linear model 
fit_1 = lm(formula = bird_hab$BRCR ~ bird_hab$ls, data = bird_hab)
summary(fit_1)
abline(a = 0.03, b = 0.007, col = 'red')

#make linear function 
linear = function(x, y_int, slope)
{
  return(y_int + slope * x)
}
linear(y_int = 1, slope = 5, x = 3)

#stochastic model ???
rnorm(fit_1)

#simulation function 
linear_simulator = function(x, y_int, slope, st_dev)
{
  y_vals = linear(x, y_int, slope) 
  random_n = rnorm(length(y_vals), mean = 0, sd = st_dev)
  return(y_vals + random_n)
}
n = 200

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

#model coefficients 
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)
fit_1_coefs

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

#get model parameters 
int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2]
sd_obs = fit_1_summary$sigma

#simulate BRCR data 
plot(
  x = bird_hab$ls, 
  y = linear_simulator(
    x = bird_hab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

plot(
  bird_hab$ls, bird_hab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = bird_hab$ls, 
  y = linear_simulator(
    x = bird_hab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))

#single simulation 
y_sim = linear_simulator(
  x = bird_hab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ bird_hab$ls)
summary(fit_sim)

#matrix of coeffs 
sum_1 = summary(fit_sim)
sum_1$coefficients

sum_1$coefficients[2, 4]

#repeated simulations #statistical power is number of times we rejected 
  #null hypothesis divided by the number of observations 
n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = bird_hab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ bird_hab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

#simulating effects size 
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = bird_hab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    effect_size = effect_sizes_1,
    power       = effect_size_powers)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')

#simulating sample size 
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(bird_hab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(bird_hab), lty = 2, col = 'red')

#bivariate 
alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(bird_hab$ls)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )
#plotting matrix
image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

#contour plotting 
contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

#3D plot 
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages('rgl')
require(rgl)
persp3d(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#saving the plot
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "labs",
    "n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

#########################
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(length(n_sds))

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = bird_hab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sds[j]
      )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_powers)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(pop_sds, pop_sd_powers, xlab = 'Dispersion', ylab = 'Statistical Power', type = "line", main = 'Statistical Power vs Dispersion')

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = sd_obs, col = 'red')

#######################
alpha = 0.05

# Start with a small number
n_sims = 30
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 30
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)
pop_sds
# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(bird_hab$ls)

pop_sd_powers = numeric(n_sds)

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_in = int_obs, 
        slope = slope_obs, 
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims 
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3, main = 'p')

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)


# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))

#contour plot 
contour(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "popoulation dispersion",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

#interactive plot 
require(rgl)
persp3d(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#saving the plot
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "labs",
    "n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

#LAB 12 !!!!!!!!!!!!!!!!!!!!!!!!!!
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(2, 20)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(bird_hab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)


fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, span = 0.3)
newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 
plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size", main = 'Loess plot of the statistical power simulation')
points(power ~ sample_size, data = sim_sample_size, col = 'red', pch = 16)
legend(13, 0.7, legend = c('Loess plot', 'Power'), col = c('black', 'red'), lty = 1:0, pch = c(NA, 16))

dat_dispersal = data.frame(read.csv(here('data', 'dispersal.csv')))

#Ricker model 
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)

#exponential model 
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
fit_exp_nls = nls(
  disp.rate.ftb ~ exp_fun(dist.class, a, b), 
  data = dat_dispersal, 
  start = list(b = 0, a = 1)) 
summary(fit_exp_nls)

dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

plot(disp.rate.ftb ~ dist.class, data = dat_dispersal, main = 'Salamander Exponential NLS Curve')
lines(predict(fit_exp_nls, newdata = dist_newdata))
curve(
  exp_fun(x, 2, 0.0034), add = TRUE, from = 0, to = 1500,
  ann = FALSE, ylab = "f(x)", col = 'red', lty = 'solid'); box()
legend("topright", legend = c("nls fit", "salamander dispersal", 'guess'), lty = c(1, 0, 1), pch = c(NA, 1), col = c(1, 1, 2))

#### BIRDS 
dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

dat_all$GCKI_pres = dat_all$GCKI > 0

#model fits 
# Create model fits
fit_gcki_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_gcki_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_gcki_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_gcki_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

summary(fit_gcki_both_additive)
AIC(fit_gcki_slope)
AIC(fit_gcki_ba_tot)
AIC(fit_gcki_both_additive)
AIC(fit_gcki_both_interactive)

summary(fit_gcki_both_interactive)

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n))
  
slope_newdata$gcki_predicted = 
  predict(
    fit_gcki_slope,
    newdata = slope_newdata,
    type = "response"
  )


ba_newdata$gcki_predicted = 
  predict(
    fit_gcki_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ ba.tot, data = ba_newdata)


AIC(
  fit_gcki_ba_tot,
  fit_gcki_slope,
  fit_gcki_both_additive,
  fit_gcki_both_interactive)

n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_gcki_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_gcki_both_interactive, 
  newdata = new_dat_all, 
  type = 'response')
)
z_gcki_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)

z_gcki_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)
require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_gcki_add,
  col = "magenta",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_gcki_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")

contour(
  x = ba.tot,
  y = slope,
  z = z_gcki_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")

