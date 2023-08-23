# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The matching function was created by Viet Nguyen with reference to 
# method from Pirotti, 2017
# (https://doi.org/10.5194/isprs-archives-XLII-2-W7-285-2017)

# The script spatially matches reference trees with detected tree
# Candidate detected trees were selected within 
# an radius of 4m around the reference trees
# The candidate tree has lowest H error which is 
# within 20% of reference H is matched


# load inv.df
inv.df = readRDS(file = "data/inv_Mollergrab.RDS")

# load crowns.spdf
crowns.spdf = shapefile("result/Crowns.shp")

# Get data component from crowns.spdf
pred.df = crowns.spdf@data

# Prepare data for matching
colnames(pred.df)
pred.df = pred.df[,c(1:4,7)]
colnames(pred.df) = c("treeID_pred","x_pred","y_pred","H_pred","crownArea_pred")


# convert detected tree to spatial object for identifying candidate neighbors
target_trees = SpatialPointsDataFrame(coords = cbind(pred.df$x_pred, pred.df$y_pred),
                                      data = pred.df)
plot(target_trees)
# Define the maximum distance in meters, 
# to determine neighboring candidate trees
max_dist = 4


# Create empty dataframe to store matched outputs in each loop
data.df = data.frame(treeID = numeric(),
                     species = character(),
                     x = numeric(),
                     y = numeric(),
                     DBH = numeric(),
                     H = numeric(),
                     treeID_pred = numeric(),
                     H_pred = numeric(),
                     crownArea_pred = numeric())

# For loop matching reference trees with detected trees
# Loop through each reference tree to find corresponding detected tree,
# Once a reference tree is matched, 
# the corresponding detected tree is removed from the pred.df
# so that it cannot be selected again.

for (i in inv.df$TreeID) {
  # loop through every reference tree and search for candidate detected trees
  tmp_tree = subset(inv.df, TreeID == i)
  tmp_tree = SpatialPointsDataFrame(
    coords = cbind(tmp_tree$X, tmp_tree$Y), data = tmp_tree)
  tmp_tree_buff = gBuffer(tmp_tree, width = max_dist)
  tmp_can = target_trees[tmp_tree_buff,]
  
  if(dim(tmp_can)[1] == 0){
    pred.df = as.data.frame(target_trees)
    pred.df = pred.df[,c(1:5)]
    target_trees = SpatialPointsDataFrame(
      coords = cbind(pred.df$x_pred, pred.df$y_pred),
      data = pred.df)
    
    print(paste0('tree ',i,' unmatched.', ' there is no candidate tree'))
    
  } else {
    # determine distance between reference tree and candidate detected trees
    tmp_can$x = tmp_tree$X
    tmp_can$y = tmp_tree$Y
    tmp_can$diffX = tmp_can@data$x - tmp_can@data$x_pred
    tmp_can$diffY = tmp_can@data$y - tmp_can@data$y_pred
    tmp_can$dist = with(tmp_can@data, sqrt(diffX^2 + diffY^2))
    
    # convert spatial tmp_can to dataframe and
    # order decreasingly candidate based on H error
    tmp_can = as.data.frame(tmp_can@data)
    tmp_can$H.error = abs(tmp_tree@data$H - tmp_can$H_pred)
    tmp_can = tmp_can[order(tmp_can$H.error),]
    
    # Keep candidate detected tree with
    # smallest H error within 20% of reference H
    if (min(tmp_can$H.error) < (0.20*tmp_tree$H)){
      tmp_dat = data.frame(TreeID = i,
                           Species = tmp_tree@data$Species,
                           x = tmp_tree@data$X,
                           y = tmp_tree@data$Y,
                           DBH = tmp_tree@data$DBH,
                           H = tmp_tree@data$H,
                           tmp_can[1,c(1,5,4)])
      
      # If a detected tree is matched, call it out from the Target_tree
      # and use this to remove that tree from the pred.df
      # and then reconvert this to a spatial object.
      can_target = as.integer(tmp_dat$treeID_pred)
      pred.df = as.data.frame(target_trees)
      pred.df = pred.df[,c(1:5)]
      
      data.df = rbind(data.df, tmp_dat)
      pred.df = subset(pred.df,
                       !(pred.df$treeID_pred == can_target[1]))
      
      target_trees = SpatialPointsDataFrame(
        coords = cbind(pred.df$x_pred, pred.df$y_pred),
        data = pred.df)
      
    } else {
      # if non of candidate detected tree is match,
      # in other words, all of them has H error excess 20% of reference H
      # recreate the pred.df and reconvert to a spatial object.
      pred.df = as.data.frame(target_trees)
      pred.df = pred.df[,c(1:5)]
      target_trees = SpatialPointsDataFrame(
        coords = cbind(pred.df$x_pred, pred.df$y_pred),
        data = pred.df)
      
      print(paste0('tree ',i,' unmatched.',' DBH error excess 20%'))
    }
  }
}

# Save the result
# saveRDS(data.df, file = "01_result/data.RDS")
