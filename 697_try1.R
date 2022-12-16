require(here) #helps get package 
install.packages('ncdf4') #install needed packages to load and read the file 
install.packages('raster')
library(ncdf4)
library(raster)

#load the file 
sea_level_nc = nc_open(here('data', 'total_ssp585_low_confidence_rates.nc'))
#save to text file 
{
    sink('sea_level_rise_total_values.txt')
  print(sea_level_nc)
    sink()
}

#file the dimenstion variables into their own variables 
lon = ncvar_get(sea_level_nc, "lon")
lon1 = ncatt_get(sea_level_nc, "lon")
lat = ncvar_get(sea_level_nc, "lat")
change = ncvar_get(sea_level_nc, "sea_level_change_rate")

head(lon$group_id)

nc_rates_matrix = matrix(data = sea_level_nc, ncol = lon)
