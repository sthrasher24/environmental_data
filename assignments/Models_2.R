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

#observed mean female penguins 
pen_fem = aggregate(fit_both$speciesChinstrap, by = )