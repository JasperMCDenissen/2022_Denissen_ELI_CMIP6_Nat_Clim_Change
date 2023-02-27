# Function to aggregate raster
agg_raster <- function(x,target) {

# this statement checks whether the target resolution is larger than the original resolution, so we know whether we need to 
# TRUE: aggregate 
# or 
# FALSE: just overwrite the original value for a smaller grid
if (min(floor(res(target) / res(x))) > 1) {
    # aggregate creates a new rasterlayer/brick with a lower resolution
    # fact gives the x,y factor by which should be aggregated
    # expand = F gives that the size of input and output raster should be equal
    # na.rm = T aggregates even if NaN
    x_agg <- raster::aggregate(x,fact = floor(res(target) / 
    res(x)),expand = F,na.rm = T)
}else{
    x_agg <- x
}

# Resample transfers values between non matching Raster* objects (in terms of origin and resolution).
# so resamples the value using bilinear interpolation if the resolution and/or origin are different
x_agg_resample <- raster::resample(x_agg, target, method = 'bilinear')

# "aggregate NAs"
# make a raster object with 1 if NA and 0 if not NA
x_NA <- is.na(x)
# resample this NA-mask to the target resolution and/or origin using nearest neighbor
x_NA <- raster::resample(x_NA, target, method = "ngb")

# and use as mask
x_agg_resample <- raster::mask(x_agg_resample, x_NA, maskvalue = 1)

x_agg_resample

}
