require(here)
dat_catrate = data.frame(read.csv(here("data","catrate.csv")))
head(dat_catrate)
dat_delomys = data.frame(read.csv(here("data","delomys.csv")))
head(dat_delomys)
dat_rope = data.frame(read.csv(here("data","rope.csv")))
head(dat_rope)

hist(dat_catrate$cat.rate, xlab= "Cat Rate", main= "Cat Rate by Stephanie Thrasher", col="red")


