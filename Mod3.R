library(lidR)
library(lidRviewer)
library(terra)
library(sf)

# or if you downloaded the reduced dataset instead
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud-reduced.laz')

# Classify all isolated returns. Takes a few seconds 
las = classify_noise(las, ivf(res = 3, n = 10))

# Filter out the noise
las = filter_poi(las, Classification != LASNOISE)

# The highest point value has a Z of 972.81. Keep only points below it.
max(las$Z) 
las = filter_poi(las, Z < 970)

las = classify_ground(las, algorithm = csf()) #see `?csf` for more options
# This may take a little while. We'll learn how to 'thin' point clouds later
# You may get a warning that the points were already classified. That's because NEON data comes pre-classified. But that's not always the case!

# make a ditigal elevation model
dem = rasterize_terrain(las, res=1, algorithm=tin())
# OR you can also load the dem you created previously
# rast(dem, 'dem.tiff')

# Make a canopy surface model
csm = rasterize_canopy(las, algorithm = pitfree())

# Make a canopy height model by subtracting the csm from the dem
chm = csm - dem

treetops = locate_trees(chm, lmf(ws = 6, hmin=10))

plot(chm)
plot(treetops$geometry, add = TRUE, pch = 16, cex = 0.2)

#first define the algorithm..
# Here I multiply chm by 1 (this just makes sure chm is loaded into meory)
# it may not be required if you followed the steps straight through
# Also, you may get an error here about the 'future' package. You may need to first install it if you don't have it already. We will use the future package later to help with parallel processing.
crown_delineation_algorithm = dalponte2016(chm*1, treetops, th_tree = 2)

# Next we will execute it.. 
crown_raster = crown_delineation_algorithm()

# And convert it to polygons
crowns = as.polygons(crown_raster)

# plot and make sure there are plenty of colors to tell the crowns apart
par(mfrow = c(1,2))
plot(crowns, col=pastel.colors(8000))
plot(chm); plot(crowns, border=grey(0.5), add = TRUE)

# convert treetops to vect object and write to disk
writeVector(vect(treetops), 'treetops.shp')
writeVector(crowns, 'crowns.shp')

# Find the number of crowns delineated
n_trees = nrow(crowns)

# Calculate the area of the scene. [Product of the resoultion (1x1)] * [product of the dimensions (1000 x 1000)]  
scene_area_m2 = prod(res(chm)) * prod(dim(chm)) 
scene_area_ha = scene_area_ha = scene_area_m2 / 10000

# Calculate tree density
print(n_trees/scene_area_ha) # There are ~68 trees per ha in the scene

# get area and radius of delineated crowns
crowns_sf = st_as_sf(crowns) # convert to sf object
crowns_sf$crown_area = st_area(crowns_sf)
crowns_sf$crown_radius = sqrt(crowns_sf$crown_area/pi)

# Make histograms of tree heights and crown area
par(mfrow = c(1,3), mar=c(4,4,2,2)) # make a 1x3 panel plot
hist(crowns_sf$crown_radius, main='Histogram of crown radius', xlab='Crown radius (m)')
hist(treetops$Z, main = 'Histogram of tree height', xlab='Tree height (m)')
plot(crowns_sf$crown_radius ~ treetops$Z, xlab='Tree height (m)',
     ylab='Crown radius (m)', pch=16, col='#00000033')

# the color setting #00000033 creates black plot point (000000) with a -33 level transparency
# Read more about hex color codes to learn more. 
# https://www.pluralsight.com/resources/blog/upskilling/understanding-hexadecimal-colors-simple

#set parameters from Gonzalez-Benecke
a1 = 3.773937
a2 = -7.844121
a3 = -0.710479
# estimate dbh from tree heights stored in `treetops$Z`
dbh_cm = ((log(treetops$Z - 1.37) - a1) / a2 ) ^ (1/a3)

par(mfrow = c(1,2))
hist(dbh_cm, xlab = 'DBH (cm)', main='Histogram of tree diameters')
plot(treetops$Z ~ dbh_cm, xlab='Estimated dbh (cm)', ylab='Estimated height (m)')
