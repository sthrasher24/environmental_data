x_observed = c(2, 6)
print(x_observed)

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

wiwa_counts = c(2, 6)
sum(dpois(x = wiwa_counts, lambda = 2.6))

prod(dpois(x = wiwa_counts, lambda = 4.5))

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)

hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7)
0:7 - 0.5
hist(dat_all$WIWA, breaks = 0:7 - .5)

par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

summary(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 0:7 - .5)

par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

wiwr_counts = c(1, 6)
dbinom(2, size = 6, prob = 0.368)


