require(here)
dat_bird = data.frame(read.csv(here("data","bird.sta.csv")))
head(dat_bird)

dat_habitat = data.frame(read.csv(here("data","hab.sta.csv")))
head(dat_habitat)

pairs(dat_habitat[,c("slope","aspect","elev")])

hist(dat_bird$CBCH,xlab="Numer of birds counted", breaks=0:7-0.5,main= "CBCH Histogram")
