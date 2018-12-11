library(maptools)
data(wrld_simpl)

# Generates coordinates in the contiguous United States, if you separated
# the northern and southern halves. 
# For use in testing differences in temperature change in the North and South.
# Assumes the geographic center line to be 39.833 degrees North

# Argument "isNorth" should be true if you want a point in the northern
# half of the contiguous United States, and false if you want a point
# in the southern half.
generateUSCoord = function(isNorth) {
  isUS = FALSE # Boolean flag, used for "do-while" logic
  while (!isUS) {
    lat = 0
    if (isNorth) {
      lat = runif(1, min=39.833, max=50)
    } else {
      lat = runif(1, min=25, max=39.833)
    }
    lon = runif(1, min=-130, max=-60)
    point = c(lon, lat)
    pt <- SpatialPoints(matrix(point,ncol=2,nrow=1), proj4string=CRS(proj4string(wrld_simpl))) 
    temp = over(pt, wrld_simpl)$FIPS
    isUS = (!is.na(temp) && temp == "US")
  }
  return(point)
}

generateUSCoords = function(n, isNorth) {
  pts = matrix(NA, nrow=n, ncol=2)
  for (i in 1:n) {
    point = generateUSCoord(isNorth)
    pts[i,1] = point[2]
    pts[i,2] = point[1]
  }  
  return(pts)
}

n = 30
northPts = generateUSCoords(n,TRUE)
southPts = generateUSCoords(n,FALSE)
npts = data.frame("Lat" = northPts[,1], "Lon" = northPts[,2])
spts = data.frame("Lat" = southPts[,1], "Lon" = southPts[,2])
write.csv(npts, "northUSTemp.csv")
write.csv(spts, "southUSTemp.csv")