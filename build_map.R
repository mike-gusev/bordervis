#building the map
library(raster)
library(rmapshaper)
library(rgeos)

us <- getData(name="GADM", country="USA", level=2, download=TRUE, path="C:/Users/mike/Documents/R/bordervis/data/spatial")
mx <- getData(name="GADM", country="MEX", level=2, download=TRUE, path="C:/Users/mike/Documents/R/bordervis/data/spatial")
usmx <- rbind(us, mx) %>%
  ms_simplify(keep=0.01, keep_shapes=TRUE, drop_null_geometries=FALSE)
save(usmx, file="data/spatial/usmx_county_map_simplified.RData")

border_states <- c("Baja California", "Sonora", "Chihuahua", "Coahuila", "Nuevo León", "Tamaulipas", "California", "Arizona", "New Mexico", "Texas")
ca_border_ctys <- c("San Luis Obispo", "Kern", "San Bernardino", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "Riverside", "San Diego", "Imperial")

border_map <- usmx[usmx$NAME_1 %in% border_states,]
border_map <- border_map[border_map$NAME_1 != "California" | border_map$NAME_2 %in% ca_border_ctys,]
plot(border_map)
save(border_map, file="data/spatial/border_map.RData")


#building the border
us0 <- getData("GADM", country="USA", level=0, path="data/spatial/")
mx0 <- getData("GADM", country="MEX", level=0, path="data/spatial/") 
border <- rgeos::gIntersection(us0, mx0)
border_line <- border@lineobj
save(border_line, file="data/spatial/border_line.RData")
