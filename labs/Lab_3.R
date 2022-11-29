install.packages("psych")
require(psych)
pairs.panels(iris)
names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])
#make tables 
require(here)
dat_bird = data.frame(read.csv(here("data","bird.sta.csv")))
head(dat_bird)

dat_habitat = data.frame(read.csv(here("data","hab.sta.csv")))
head(dat_habitat)

#merge them 
dat_all = data.frame(merge(dat_bird, dat_habitat))
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

my_vec = dat_all$CEWA
my_vec > 0

cewa_present_absent = as.numeric(my_vec > 1)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

pairs.panels(dat_habitat)
name(dat_haitat)
pairs.panels(dat_habitat[,c("elev","slope","aspect","ba.tot")])
 

RECR= dat_all$RECR
RECR > 0 
GRJA= dat_all$GRJA
GRJA > 0 

recr_p_a =  as.numeric(RECR > 1)
plot(x = dat_all$ba.tot, y = recr_p_a)

plot(x = dat_all$ba.tot, y =recr_p_a, xlab= "Basal Total", ylab = "Red Crossbill", main="Red Crossbill Plot")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.3), add = TRUE)

grja_p_a = as.numeric(GRJA > 1)
plot(x =dat_all$ba.hard, y = grja_p_a)

plot(x = dat_all$ba.con, y =amcr_p_a, xlab= "Basal Total", ylab = "Gray Jay Population", main="Gray Jay Plot")
curve(logistic_midpoint_slope(x, midpoint = 70, slope = -0.7), add = TRUE)

sample(dat_all$GRJA)
greyb_vec = dat_all$GRJA 
sum(greyb_vec)

my_vec = dat_all$GRJA 
sum(my_vec > 0)




