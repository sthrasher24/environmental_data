#call data
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
veg = read.csv(here("data", "vegdata.csv"))
head(veg)
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

#chi-squared test
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]
br_creeper_table

chisq_brcr = chisq.test(br_creeper_table)
chisq_brcr

#ANOVA tests 
require(palmerpenguins)
#species
fit_species = 
  lm(formula = body_mass_g ~ species,
    data = penguins)
fit_species
#sex
fit_sex = lm(formula = body_mass_g ~ species, 
             data = penguins)
fit_sex
#sex & species 
fit_both = lm(formula = body_mass_g ~ sex:species, 
              data = penguins)
fit_both 

#conditional boxplots 
boxplot(body_mass_g ~ species, data = penguins, main = 'Doubly-Conditional Boxplot Species')
boxplot(body_mass_g ~ sex, data = penguins, main = 'Doubly-Conditional Boxplot Sex')
boxplot(body_mass_g ~ sex:species, data = penguins, 
        main = 'Doubly-Conditional Boxplot Species', 
        names = c('Adelie F', 'Adelie M', 'Chinstrap \n Female', 'Chinstrap \n Male', 'Gentoo F', 'Gentoo M'), 
        las = 3, xlab = '')

#bartlett test
bartlett.test(body_mass_g ~ species, data = penguins)
bartlett.test(body_mass_g ~ sex, data = penguins)

dat_groups = aggregate(
  body_mass_g ~ species*sex,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)

#read in tree data 
dat_FL = data.frame(read.csv(here('data', 'trees_FL.csv')))
head(dat_FL)
par(mfrow = c(2, 2))
tab_prob = table(dat_FL$ProbabilityofFailure)
tab_fail = table(dat_FL$Failure_Standardized)
tab_DBH = table(dat_FL$DBH_in)
tab_height = table(dat_FL$HeighttoTop_ft)
barplot(tab_prob, xlab = 'Probability of Failure', main = 'Probability of Failure of Trees')
barplot(tab_fail, xlab = 'Failure Standardized', main = 'Standardized Failure of Trees')
hist(tab_DBH, xlab = 'DBH (in)', main = 'Diameter of Breast Height', breaks = 5)
plot(HeighttoTop_ft ~ DBH_in, data = dat_FL, xlab = 'DBH (in)', ylab = 'Height of Tree (ft)', main = 'Tree Height vs Diameter Breast Height')

dat_whole = droplevels(subset(dat_FL, Failure_Standardized == "whole"))
dat_none = droplevels(subset(dat_FL, Failure_Standardized == "none"))
ks.test(dat_whole$DBH_in, dat_none$DBH_in)

cor.test(
  dat_FL$HeighttoTop_ft,
  dat_FL$DBH_in,
  use='complete.obs',
  method='spearman')


dat_FL$fail = factor(dat_FL$Failure_Standardized != "none")

levels(dat_FL$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_FL$ProbabilityofFailure,
  dat_FL$fail)
fl_table_2

chisq_fail = chisq.test(fl_table_2)
chisq_fail
str(chisq_fail)

chisq_fail$residuals

