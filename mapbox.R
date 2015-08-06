# Install packages if necessary
# install.packages(c("sp", "rgeos", "FNN", "GISTools", "RColorBrewer"))

# Load packages
library(sp) # SpatialPoints and SpatialPolygons objects
library(rgeos) # gContains function
library(FNN) # nearest neighbors function
library(GISTools) # choropleth maps
library(RColorBrewer) # good color palettes for mapping
load("./mappingData.RData")

# Create SpatialPoints object from the coordinates in map.df.
sp.pts <- SpatialPoints(cbind(map.df$lon, map.df$lat))
# Convert points into a grid.
# Note that we have to set the tolerance because the
# grid points are not perfectly equidistant.
sp.grd <- points2grid(sp.pts, tolerance=0.000763359)
grd.layer <- as.SpatialPolygons.GridTopology(sp.grd)
# Eliminate unnecessary grid cells.
grd.cont <- ifelse(colSums(gContains(grd.layer, sp.pts, byid=T)) > 0, TRUE, FALSE)
grd.layer <- grd.layer[grd.cont]

# Assign row names to grd.layer so we can match them properly with 
# the information in map.df
row.names(grd.layer) <- as.character(1:nrow(map.df))
# use get.knnx to match up probabilities with the 
# appropriate grid cell
k.nn <- get.knnx(coordinates(grd.layer), cbind(map.df$lon, map.df$lat), 1)
# Assign the rows of map.df 
row.names(map.df) <- k.nn$nn.index
grd.layer.df <- SpatialPolygonsDataFrame(grd.layer, map.df, match.ID = T)

# Create color categories and map grid with 911 data
shades <- auto.shading(grd.layer.df$prob.911, n = 9, cols=brewer.pal(9, "Greens"))
choropleth(grd.layer, grd.layer.df$prob.911, shades)

# Write grd.layer.df to a shapefile for use with mapbox
writePolyShape(grd.layer.df, "911probabilities")
