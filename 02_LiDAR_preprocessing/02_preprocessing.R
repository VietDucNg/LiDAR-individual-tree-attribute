# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The Script was compiled with reference to materials from the course
# LiDAR data collection and analysis taught by Dr. Nikolai Knapp at
# Thuenen Institute of Forest Ecosystems


# import LiDAR data
las = readLAS("data/pc_Mollergrab.laz")

# load aoi shapefile
aoi.sf = st_read("data/aoi_Mollergrab.shp")


##############################
#### clip las file to aoi ####
##############################

# add buffer of 5m to aoi.sf to make sure entire trees included
aoi_buff = st_buffer(aoi.sf, dist = 5)
plot(aoi.sf[0])
plot(aoi_buff[0], add=T)

# transform aoi CRS to las CRS
st_crs(aoi_buff)
lidR::projection(las)
lidR::projection(las) = 25833

# clip point cloud by polygon
las = clip_roi(las,aoi_buff)


##################
#### thinning ####
##################

# check point density
density = grid_density(las, res = 1) #density in 1 square meter
plot(density)
density

# thin point cloud to 100points/m2
las = decimate_points(las, homogenize(100,res = 1))

# check point density after thinning
density = grid_density(las, res = 1) #density in 1 square meter
plot(density)
density


###################
#### filtering ####
###################

# Check how many points in the point cloud
nrow(las@data)

# Check the height distribution
hist(las$Z)
max(las$Z)

# Simulate some noise points (e.g., birds)
birds.dt <- data.table(X=runif(n=20, min=min(las$X), max=max(las$X)),
                       Y=runif(n=20, min=min(las$Y), max=max(las$Y)),
                       Z=runif(n=20, min=min(las$Z)+100, max=max(las$Z)+100))

# Add the noise to the point cloud
noisy.las <- las
noisy.las@data <- rbind(noisy.las@data, birds.dt, fill=T)
tail(noisy.las@data)
plot(noisy.las)
hist(noisy.las$Z)
nrow(noisy.las@data) - nrow(las@data)

# classify points as noise (outliers) with isolated voxels filter (ivf)
noise.las = classify_noise(noisy.las, ivf(res=10, n=5)) #voxel size 10*3 = 30m

# Check the frequencies of each classes
table(noise.las$Classification)

# filter outliers
clean.las = filter_poi(noise.las, Classification != LASNOISE)

# Check how many points were filtered
nrow(noise.las@data) - nrow(clean.las@data)


###############################
#### ground classification ####
###############################

# Classify ground points with the progressive morphological filter (PMF)
las_check(las)
las = classify_ground(las, algorithm = pmf(ws = 5, th = 1))


# Classify ground points with the cloth simulation filter (CSF) algorithm
# Drop a simulated cloth on the inverted point cloud,
# Ground points classified by analysing
# the interactions between cloth nodes and the inverted surface.
las = classify_ground(las, algorithm=csf())
table(las$Classification)


#######################
#### normalization ####
#######################

# point cloud-based approach
# Inverse Distance Weighting (IDW) interpolation
norm.las = normalize_height(las, knnidw())
plot(norm.las)

# DTM-based approach
# first, create a DTM using triangular irregular network (TIN) interpolation
dtm.ras = grid_terrain(las, res = 0.5, algorithm = tin())
plot(dtm.ras)

# subtract from all point height values for the DTM value underneath
norm.las2 = normalize_height(las, dtm.ras)

# compare z values of ground points from 2 approachs
# point cloud-based approach
hist(filter_ground(norm.las)$Z, breaks = seq(-10, 10, 0.05), 
     main = "point cloud-based approach", xlab = "Z", xlim=c(-1, 1))
# DTM-based approach
hist(filter_ground(norm.las2)$Z, breaks = seq(-10, 10, 0.05), 
     main = "DTM-based approach", xlab = "Z", xlim=c(-1, 1))

# save preprocessed point cloud if necessary
# the preprocessed point cloud was also provided in 01_result folder
# saveRDS(norm.las,file = "result/pc_Mollergrab_prepro.RDS")
