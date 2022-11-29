require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

hist(catrate$cat.rate, xlab = 'Reproduction Cat Rates', 
     main = 'Salamander Reproduction Catastrophic Rates')

cat_rate = catrate$cat.rate
shapiro.test(cat_rate)
head(catrate)
#t.test(catrate$pond, catrate$cat.rate, mu = 0.54)

t.test(catrate$cat.rate, mu = 2/7)

wilcox.test(catrate$cat.rate, mu = 0.28)

install.packages("nortest")
require(nortest)
require(palmerpenguins)
head(penguins)

dat_pen = na.omit(droplevels(subset(penguins, species != 'Gentoo')))
dat_pen_s = dat_pen$flipper_length_mm
shapiro.test(dat_pen_s)

dat_adeile = na.omit(droplevels(subset(dat_pen, species != 'Chinstrap')))
dat_chinstrap = na.omit(droplevels(subset(dat_pen, species != 'Adiele')))
par(mfrow = c(1, 2))
hist(dat_adeile$flipper_length_mm, main = 'Adeile Flipper Lengths', xlab = 'Flipper Lengths')
hist(dat_chinstrap$flipper_length_mm, main = 'Chinstrap Flipper Lengths', xlab = 'Flipper Lengths')

t.test(dat_pen$flipper_length_mm ~ dat_pen$species, alternative = c('greater'))
