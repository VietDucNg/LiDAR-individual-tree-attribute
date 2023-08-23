# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The Script was compiled with reference to materials from the course
# LiDAR data collection and analysis taught by Dr. Nikolai Knapp at
# Thuenen Institute of Forest Ecosystems


# import pre-processed LiDAR data
las = readRDS("result/pc_Mollergrab_prepro.RDS")

# load inventory data
inv.df = readRDS(file = "data/inv_Mollergrab.RDS")


#############
#### CHM ####
#############

# Point-to-raster algorithm
# Establish a grid at 1m resolution and
# assign the height value of he highest point to each pixel value
chm.ras = grid_canopy(las,res=1,p2r())
# check visually
plot(chm.ras, col =height.colors(35))


###################
#### Tree tops ####
###################

# Find tree tops by Local Maximum Filter (LMF) algorithm
# The algorithm investigates every point in the point cloud, 
# examines neighboring points, and checks if the point being processed 
# is the highest point within the window size
treeTop.spdf = find_trees(las, lmf(ws=5))
max(treeTop.spdf@data$treeID)

# check visually
head(treeTop.spdf@data)
plot(chm.ras, col = height.colors(35))
plot(treeTop.spdf, add=T)

plot = plot(las, size =3)
add_treetops3d(plot, treeTop.spdf)


######################
#### segmentation ####
######################

# raster-based algorithm

###########################################
#### waterhed algorithm (raster-based) ####

las.watershed = segment_trees(las, algorithm=watershed(chm.ras))
# count tree detected
length(unique(las.watershed@data[["treeID"]]))
# visualization
plot_segment = plot(las.watershed, size=2, color="treeID")
add_treetops3d(plot_segment,treeTop.spdf)


###################################################################
#### Silva (2016) Voronoi tesselation algorithm (raster-based) ####

las.silva16 = segment_trees(las, algorithm=silva2016(chm.ras, treeTop.spdf))
# count tree detected
length(unique(las.silva16@data[["treeID"]]))
# visualization
plot_segment = plot(las.silva16, size=3, color="treeID")
add_treetops3d(plot_segment,treeTop.spdf)


###############################################
#### dalponte2016 algorithm (raster-based) ####

las.dalponte16 = segment_trees(las, algorithm=
                                 dalponte2016(chm.ras,treeTop.spdf))
# count tree detected
length(unique(las.dalponte16@data[["treeID"]]))
# visualization
plot_segment = plot(las.dalponte16, size=3, color="treeID")
add_treetops3d(plot_segment,treeTop.spdf)


# point cloud-based algorithm

# point cloud-based algorithms take much longer time than raster-based ones
# because these algorithms go through every point for interpolation
# as ground points are not needed for tree crown segmentation
# remove ground points may help to reduce significantly processing time

las = filter_poi(las, Classification != 2)
plot(las)


################################################################
#### Li (2012) region growing algorithm (point cloud-based) ####

las.li12 = segment_trees(las, algorithm=li2012(hmin = 6))
# count tree detected
length(unique(las.li12@data[["treeID"]]))
# visualization
plot(las.li12, size=2, color="treeID")


#################################################################################
#### The adaptive mean shift algorithm (AMS3D) algorithm (point cloud-based) ####

# The algorthm was presented by Ferraz et al. (2012)
# Leon Steinmeier provides it in the crownsegmentr package

las.ams3d = readRDS(file = "result/pc_Mollergrab_prepro.rds")

# count tree detected
length(unique(las.ams3d@data[["treeID"]]))

# Remove points which are not classified as tree
las.ams3d = filter_poi(las.ams3d, !is.na(treeID))

# visualization
plot(las.ams3d, size=2, color="treeID")


###########################
#### crown delineation ####
###########################

crowns.spdf <- delineate_crowns(las.ams3d, func=.stdtreemetrics)
plot(crowns.spdf)
length(crowns.spdf)

# remove understory crowns or false detection
crowns.spdf = crowns.spdf[crowns.spdf$ZTOP > 6,]
crowns.spdf = crowns.spdf[crowns.spdf$convhull_area > 1,]
crowns.spdf = crowns.spdf[crowns.spdf$npoints > 50,]

length(crowns.spdf)


# visualization
head(crowns.spdf)
par(mar=c(3,3,1,1))
plot(chm.ras, col=height.colors(50))
plot(crowns.spdf, add=T)
plot(treeTop.spdf, add=T, pch=21, bg="white", cex=0.5)

# Save the crown polygons as shapefile
# shapefile(crowns.spdf, "result/Crowns.shp", overwrite=T)

# Save the tree top points as shapefile
# shapefile(treeTop.spdf, "result/treeTop.shp", overwrite=T)

# Save the CHM as raster file
# writeRaster(chm.ras,"result/chm.tif", overwrite=T)
