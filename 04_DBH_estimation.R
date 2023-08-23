# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The Script was compiled with reference to materials from the course
# LiDAR data collection and analysis taught by Dr. Nikolai Knapp at
# Thuenen Institute of Forest Ecosystems


# load matched data
data.df = readRDS("01_result/data.RDS")

# Plot histogram
par(mar=c(4,4,4,1))
hist(data.df$crownArea)
hist(data.df$H_pred)

# calculate crown diameter with an assumption that tree crown is circle
data.df$crownDiameter_pred = 2 * (data.df$crownArea_pred / pi)^0.5

# calculate 2D bounding box (tree height x crown diameter)
data.df$treeBB = data.df$H_pred * data.df$crownDiameter_pred

# plot relationship
plot(data.df$H_pred, data.df$DBH,
     xlab="Predicted height (m)", ylab="Measured DBH (cm)")
abline(lm(DBH~H_pred, data = data.df), col='blue')

plot(data.df$crownDiameter_pred, data.df$DBH,
     xlab="Predicted crown diameter (m)", ylab="Measured DBH (cm)")
abline(lm(DBH~crownDiameter_pred, data = data.df), col='blue')

plot(data.df$treeBB, data.df$DBH,
     xlab="Predicted bounding box (m2)", ylab="Measured DBH (cm)")
abline(lm(DBH~treeBB, data = data.df), col='blue')


#######################
#### calculate DBH ####
#######################

# apply the allometric equation suggested by Jucker et al. (2017)
# for temperate mixed forest in Palearctic realm.
# the equation uses tree height and crown diameter (bounding box) as predictors
# to predict DBH. 
# https://doi.org/10.1111/gcb.13388

data.df$DBH_pred = 0.708*data.df$treeBB^0.753

# Plot the estimated DBH distribution
hist(data.df$DBH_pred, breaks=seq(0, 70, 5), xlab="predicted DBH [cm]", ylim=c(0, 100))

# Compare the real DBH distribution from the inventory
hist(data.df$DBH, breaks=seq(0, 70, 5), add=T, col=rgb(0,0,1,0.4))

# prepare dataframe
DBH_pred = data.frame(group = "Predicted DBH" , value = data.df$DBH_pred)
DBH = data.frame(group = "Measured DBH" , value = data.df$DBH)
DBH.df = rbind(DBH_pred,DBH)

#boxplot
boxplot(data.df$DBH, data.df$DBH_pred,
        main = "DBH", ylab = "Distribution", 
        names = c("measured","Predicted"))

# Make a 1:1 plot between predicted and measured DBH
plot(data.df$DBH, data.df$DBH_pred,
     main="1-1 plot", xlab="Measured DBH (cm)", ylab="Predicted DBH (cm)",
     xlim=c(0,70), ylim=c(0,70))
abline(0,1,col='blue')


######################################
#### Evaluate the prediction model####
######################################

pred <- data.df$DBH_pred
obs <- data.df$DBH

# Calculate goodness-of-fit statistics
# R2
(R2 <- summary(lm(pred ~ obs))$r.squared)
# root mean squared error (RMSE)
(RMSE <- (mean((pred-obs)^2))^0.5)
# bias (mean error)
(Bias <- mean(pred-obs))

# add to the plot
# Write them into the graphic
text(x=0, y=65, paste0("R² = ", round(R2, 2)), adj=c(0, 0))
text(x=0, y=60, paste0("RMSE = ", round(RMSE, 1)), adj=c(0, 0))
text(x=0, y=55, paste0("Bias = ", round(Bias, 1)), adj=c(0, 0))

