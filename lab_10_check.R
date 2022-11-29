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

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)