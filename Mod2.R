library(lidR)
library(lidRviewer)
library(terra)

las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud-reduced.laz')

# Classify all isolated returns. Takes a few seconds
las = classify_noise(las, ivf(res = 3, n = 10))

# Filter out the noise
las = filter_poi(las, Classification != LASNOISE)

# The highest point value has a Z of 972.81. Keep only points below it.
max(las$Z)
las = filter_poi(las, Z < 970)
view(las)
# Press the z-key to view it colored by height

las = classify_ground(las, algorithm = csf()) #see `?csf for more algorithm options
# This may take a little while. We'll learn how to 'thin' point clouds later
# You may get a warning that the points were already classified.
# That's because NEON data comes pre-classified. But that's not always the case!

view(las)
#Press the c-key to view the classified points, that include ground points

dem = rasterize_terrain(las, res=1, algorithm=tin())

plot(dem)

# You can also make a slope or aspect map,
# Note these contain less noise when using coarser resolution dem. Try making one!
slope = terrain(dem, v='slope', unit='degrees')
asp = terrain(dem, v='aspect', unit='degrees')

par(mfrow=c(1,2)) # make a 1x2 panel plot window
plot(slope)
plot(asp)

# First make a canopy surface model
csm = rasterize_canopy(las, algorithm = pitfree())

# Make a canopy height model by subtracting the csm from the dem
chm = csm - dem

par(mfrow = c(1,3), mar=c(1,1,3,2))
plot(dem, main='digital elevation model')
plot(csm, main='canopy surface model')
plot(chm, main='canopy height model')

# Normalize the `LAS` object, by subtracting out height
las.norm = normalize_height(las, algorithm=tin())

# Keep only points that are not ground and that are <2 m above ground
understory_points = filter_poi(las.norm, 
                               Classification != LASGROUND & Z < 2)

# Count the point density at a 10 m resolution. 
#This could be an index of understory vegetation density
total_density = rasterize_density(las, res=10)

# Count the point density for ALL points
understory_only = rasterize_density(understory_points, res=10)

# Calculate the fraction of understory points relative to the total.
percent_understory_pts = understory_only / total_density

par(mfrow = c(1,3), mar=c(1,1,3,2))
plot(understory_only)
plot(total_density)
plot(percent_understory_pts)

# first define a function that uses the an attribute of the lidar data 
# (e.g., Z value) to summarize some useful metric 
# (in this case, we calculate a coefficiant of variation of heights (sd/mean))
my_complexity_function = function(Z) sd(Z)/mean(Z)

# and summarize it at some scale
complexity = pixel_metrics(las, my_complexity_function(Z), res = 10)

par(mfrow = c(1,1), mar=c(1,1,3,2)) #reset back to single panel
plot(complexity, main='complexity')
