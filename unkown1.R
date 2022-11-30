require(palmerpenguins)
rm(list = ls())
sse_mean = function(x)
{
  values = !is.na(x) 
  y = x[values]
  sd(y)/sqrt(length(y))
} 

#sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

two_group_resample_diff = function(x, n_1, n_2) 
{
  values = !is.na(x)
  z = x[values]
  g = sample(z, n_1, replace = TRUE) 
  h = sample(z, n_2, replace = TRUE)
  difference_in_means = mean(g) - mean(h) 
  return(difference_in_means)
}

two_group_resample_diff(penguins$body_mass_g, 1000, 3000)

#create loop for 2000 samples 
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

#flipper length greater than 5.8 
length_flip <- sum(abs(mean_differences) >= 5.8) 

#using boxplot
boxplot(
  body_mass_g ~ species, data = dat_pen,
  ylab = "Body Mass (g)")

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE,
  )
diff_crit = diff(agg_means[, 2])

agg_means 
diff_crit 

t.test(dat_pen$body_mass_g ~ dat_pen$species)

n = 1000
mean_differences = c()
for (i in 1:n)
  
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$body_mass_g, 68, 152)
  )
}
hist(mean_differences)

body_mass <- sum(abs(mean_differences) >= diff_crit) 





