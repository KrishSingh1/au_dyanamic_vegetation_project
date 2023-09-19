print(unique(dea.data.satflb0004$time))

###### Validate Visually ######

abs(max(dea.data.satflb0004$x))-abs(min(dea.data.satflb0004$x))
abs(max(dea.data.satflb0004$y))-abs(min(dea.data.satflb0004$y))

MASS::eqscplot(dea.data.satflb0004$x, dea.data.satflb0004$y,tol = .5, main = file.names[RI],
               xlab = "easting", ylab = "northing")

points(x = site.info.data['pit_marker_easting'],y = site.info.data['pit_marker_northing'], pch = 2, col = 'red')
points(x = site.info.data['pit_marker_easting']+100,y = site.info.data['pit_marker_northing'], pch = 2, col= 'red')
points(x = site.info.data['pit_marker_easting'],y = site.info.data['pit_marker_northing']+100, pch = 2, col= 'red')
points(x = site.info.data['pit_marker_easting']+100,y = site.info.data['pit_marker_northing']+100, pch = 2, col= 'red')

###### Evalidate Numerically ######

# check if the satellite imaging captured the right coordinates
# algorithm:
#   1. get coords (x,y of both site and sat)
#   2. get mid.coords for site 
#   2. get largest values of sat x and y
#   3. condition checks:
#       a) site.mid.x > sat.x_min & site.mid.x < sat.X
#       b) site.mid.y > sat.y_min & site.mid.y < sat.y
#   4. If both are true, then the mid-point of the site lies within the site image
#   5. Calculate the offsets of the SW and NE borders 
#   6. Return (True/false for midpoint, offset.x, offset.y)

check.sat.position <- function(site.x.coord, site.y.coord, sat.x.coord, sat.y.coord){
  
  site.mid.x <- sat.x.coord+50
  site.mid.y <- sat.y.coord+50
  
  sat.min.max.x.y.coords <- c("x.min" = min(sat.x.coord), "x.max" = max(sat.x.coord),
                              "y.min" = min(sat.y.coord), "y.max" = max(sat.y.coord))
  
  is.witin.x <- any(site.mid.x > sat.min.max.x.y.coords["x.min"] & 
                      site.mid.x < sat.min.max.x.y.coords["x.max"])
  
  is.witin.y <- any(site.mid.y > sat.min.max.x.y.coords["y.min"] &
                      site.mid.y < sat.min.max.x.y.coords["y.max"])
  
  is.within.coords <- F
  if(is.witin.x & is.witin.y){
    is.within.coords <- T
  }
  
  ## Get offsets for the corner points (SW, NE)
  offsets.SW.components <- c(site.x.coord - sat.min.max.x.y.coords[["x.min"]],
                             site.y.coord - sat.min.max.x.y.coords[["y.min"]])
  # +100 to to SW to get NE
  offset.NE.components <- c(site.x.coord + 100 - sat.min.max.x.y.coords[["x.max"]],
                            site.y.coord + 100 - sat.min.max.x.y.coords[["y.max"]])
  
  
  offset.SW <- sqrt(sum(offsets.SW.components^2))
  offset.NE <- sqrt(sum(offset.NE.components^2))
  
  return(c("is.within.coords" = is.within.coords, 
           "offset.SW" = offset.SW,
           "offset.NE" = offset.NE))
  
}