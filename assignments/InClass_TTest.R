require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
female_ade = droplevels(subset(dat_ade, sex == "female"))
male_ade = droplevels(subset(dat_ade, sex == "male"))

hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

boxplot(body_mass_g ~ sex, data = penguins, main = 'Body mass of Adelie Penguins')

t.test(female_ade$body_mass_g, y = NULL, alternative = c('greater'), mu = 0)

t.test(male_ade$body_mass_g, y = NULL, alternative = c("greater"), mu = 4000)

t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative = c("two.sided"), mu = 0)

t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative = c('greater'), mu = 0)

t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative = c('less'), mu = 0)
