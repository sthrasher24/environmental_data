#load in penguins 
require(palmerpenguins)

#create conditional boxplot 
boxplot(body_mass_g ~ species, data = penguins)

boxplot(body_mass_g ~ sex:species, data = penguins, 
        ylab = 'Body Mass (g)',
        main = 'Doubly-Conditional Boxplot Species', 
        names = c('Adelie F', 'Adelie M', 'Chinstrap \n Female', 'Chinstrap \n Male', 'Gentoo F', 'Gentoo M'), 
        las = 3, xlab = '')

#fit a linear model 
fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

#observed mean Chinstrap female penguins 
pen_chin = droplevels(subset(penguins, species == 'Chinstrap'))
pen_fem = droplevels(subset(pen_chin, sex == 'female'))
pen_mean = mean(pen_fem$body_mass_g, na.rm = TRUE)

pen_agg = aggregate(cbind(body_mass_g) ~ species + sex, penguins, mean)
pen_agg
