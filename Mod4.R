# libraries
library(lidR)
library(terra)

# Load and denoise LiDAR data.
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud-reduced.laz')
las = classify_noise(las, ivf(res = 3, n = 10))
las = filter_poi(las, Classification != LASNOISE)
las = filter_poi(las, Z < 970)

# Classify ground points, create a dem and chm
las = classify_ground(las, algorithm = csf()) #see `?csf` for more options
dem = rasterize_terrain(las, res=1, algorithm=tin())
csm = rasterize_canopy(las, algorithm = pitfree())
chm = csm - dem

# Generate a stem map and crown delineation feature
treetops = locate_trees(chm, lmf(ws = 6, hmin=10))
crown_delineation_algorithm = dalponte2016(chm, treetops, th_tree = 2)
crown_raster = crown_delineation_algorithm()
crowns = as.polygons(crown_raster)

# Check out the data...
plot(chm)
plot(crowns, add=TRUE)

# Use the extract function, (Note i added the `terra::extract(...)` prefix 
# to avoid conflict with another package, you may just be able to 
# use `extract(...))
#terra::extract(dem, treetops) # they are stored in a column titled Z

treetops$elevation = extract(dem, treetops)$Z

par(mfrow = c(1,2)) # make a 1 x 2 panel window
plot(dem)
plot(treetops$geometry, add = TRUE, pch = 16, cex = 0.1)
hist(treetops$elevation)

# extract tree heights from the chm
treetops$ht_m = extract(chm, treetops)$Z

#extract area of crowns in units of the coordinate system
treetops$area_m2 = expanse(crowns) 

par(mfrow = c(1,2)) 
hist(treetops$ht_m)
hist(treetops$area_m2)$Z

# Make a regression of height versus elevation
height_model = lm(ht_m ~ elevation, data = treetops)

# Check the model (we are certainly violating assumptions here...)
summary(height_model)

# View the scatter plot
plot(treetops$ht_m ~ treetops$elevation, pch=16, col='#00000033',
     xlab = 'Elevation (m)', ylab= 'Tree height (m)')
# plot character = 16 (solid circle)
# col = black (hex code #000000) with some transparency (33), FF for no transparency

# Add best fit line
abline(height_model, col='blue', lwd=2) #line width = 2

# Make a regression of height versus elevation
crown_model = lm(area_m2 ~ elevation, data = treetops)

# Check the model (we are certainly violating assumptions here...)
summary(crown_model)

# View the scatter plot
plot(area_m2 ~ elevation, data=treetops, pch=16, col='#00000033',
     xlab = 'Elevation (m)', ylab= 'Crown area (m^2)')
# plot character = 16 (solid circle)
# col = black (hex code #000000) with some transparency (33), FF for no transparency

# Add best fit line
abline(crown_model, col='blue', lwd=2) #line width = 2
