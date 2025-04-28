# load the lidR library
library(lidR)
library(lidRviewer)

las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud-reduced.laz')

# plot using lidRviewer
view(las)

# with the viewer OPEN, press z on your keyboard
# This will color the points based on height (z-value)
# close the window when you are finished

# Classify all isolated returns. Takes a few minutes
las = classify_noise(las, ivf(res = 3, n = 10))

# The classify_noise function sets
# Classification = 18 (LASNOISE = 18)
las = filter_poi(las, Classification != LASNOISE)
view(las)

# You can use trial and error to adjust parameters or
# base the parameters on knowledge of the scan
# This scene still has some pesky noise that must be in a cluster. Let's delete it manually.
# Take a look at the highest point, and filter out data below it
max(las$Z) #which Z value has the maximum value.

# The highest point value has a Z of 972.812. Keep only points below it.
las = filter_poi(las, Z < 970)

# View it now
view(las)
# Press the z-key to view it colored by height

colnames(las@data)

# press r, g, or b, to view color with RGB

# press z to color with z value (elevation above see level)

# press i to color with lidar return intensity
# Ground and trunks have a higher intensity than vegetation

# press c to color with point classification (if available)
# The example point cloud is already classified. We'll learn to classify points later
# Green = tall vegetation, yellow = short vegetation, blue = ground