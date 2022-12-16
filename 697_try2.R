library(raster)

grd <- raster('data', 'total_ssp585_low_confidence_values.nc', stopIfNotEqualSpaced = FALSE)
coords <- brick(('data' 'total_ssp585_low_confidence_values.nc', varname = "lon")('data' 'total_ssp585_low_confidence_values.nc', varname = "lat"))

