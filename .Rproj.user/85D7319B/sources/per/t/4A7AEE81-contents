require(here)
#question 1
rm(list = ls())

rope = data.frame(read.csv(here('data', 'rope.csv')))
rope$rope.type = factor(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

#residuals = observed - predicted 
#sum of squares total 
gmean = mean(rope$p.cut)
resids = rope$p.cut - gmean
ss_tot = sum(resids^2)
df_tot = n_obs - 1
#sum squares within 
test_agg = aggregate(x = rope$p.cut, by = list(rope$rope.type), FUN = function(x) mean(x))
test_agg
agg_resids = aggregate(x = rope$p.cut, by = list(rope$rope.type), FUN = function(x) x - mean(x))
str(agg_resids)
agg_sum_sq_resids = aggregate(x = rope$p.cut, by = list(rope$rope.type), FUN = function(x) sum((x - mean(x))^2))
str(agg_sum_sq_resids)
ss_within = sum(agg_sum_sq_resids$x)
ss_within
df_within = n_obs - 6
#sum squares among 
ss_among = ss_tot - ss_within
ss_among
df_among = 6 - 1 

ms_within = ss_within / (n_obs - n_groups)
ms_within
ms_among  = ss_among / (n_groups - 1)
ms_among

f_ratio = ms_among / ms_within
f_ratio
f_pval = 1 - pf(f_ratio, df_among, df_within)
f_pval

#boxplot 
boxplot(
  p.cut ~ rope.type,
  data = rope,
  las = 2,
  xlab = "Rope Type",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)


#bartlett test
bart_test = bartlett.test(p.cut ~ rope.type, rope)
bart_test

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

resids_norm = shapiro.test(residuals(fit_rope_1))
resids_norm

resids_groups = sapply(agg_resids$x, shapiro.test)

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(body_mass_g ~ species, pen_fem, xlab = 'Species', main = 'Condtional Boxplot of Penguins')

bartlett.test(body_mass_g ~ species, pen_fem)

fit_pen = lm(body_mass_g ~ species, data = pen_fem)
shapiro.test(residuals(fit_pen))

pen_hsd = TukeyHSD(aov(fit_pen))
pen_hsd

round(pen_hsd$species, digits = 4)
