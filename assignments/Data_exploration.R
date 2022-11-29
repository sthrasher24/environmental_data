install.packages("here")
require(here)
dat_habitat = data.frame(read.csv(here("data","hab.sta.csv")))

par(mfrow = c(3, 1))
hist(dat_habitat$slope, xlab = "Slope", main= "Sampling Site Slope")
hist(dat_habitat$elev, xlab = "Elevation", main= "Sampling Site Elevation")
hist(dat_habitat$aspect, xlab = "Aspect", main= "Sampling Site Aspect", breaks=50)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
par(mfrow= c(3,1))
plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, xlab = "Slope", ylab = "Basal Total", main = "Slope vs Basal Total", cex= 0.75)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.05), add = TRUE,col="chartreuse")

plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, xlab = "Elevation", ylab = "Basal Total", main = "Elevation vs Basal Total", cex= 0.75)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.01), add = TRUE,col="chartreuse")

plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, xlab = "Aspect", ylab = "Basal Total", main = "Aspect vs Basal Total", cex= 0.75)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.1), add = TRUE,col="chartreuse")

