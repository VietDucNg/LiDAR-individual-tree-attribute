# Author: Viet Nguyen, faculty of Forest and Environment, HNEE
# Email: Duc.Nguyen@hnee.de
# The Script was compiled with reference to materials from the course
# LiDAR data collection and analysis taught by Dr. Nikolai Knapp at
# Thuenen Institute of Forest Ecosystems


# load matched data
data.df = readRDS("result/data.RDS")

# Plot the estimated H distribution
hist(data.df$H_pred, breaks=seq(5, 40, 1), xlab="H [m]", ylim=c(0, 55))

# Compare the real H distribution from the inventory
hist(data.df$H, breaks=seq(5, 40, 1), add=T, col=rgb(0,0,1,0.4))

# prepare dataframe
h_pred = data.frame(group = "Predicted height" , value = data.df$H_pred)
h = data.frame(group = "Measured height" , value = data.df$H)
height.df = rbind(h_pred,h)

#boxplot
boxplot(data.df$H, data.df$H_pred,
        main = "Height", ylab = "Distribution", 
        names = c("measured","Predicted"))

# Make a 1:1 plot between predicted and measured H
plot(data.df$H, data.df$H_pred,
     main="1-1 plot", xlab="Measured H (m)", ylab="Predicted H (m)",
     xlim=c(0,40), ylim=c(0,40))
abline(0,1,col='blue')

######################################
#### Evaluate the prediction model####
######################################

pred <- data.df$H_pred
obs <- data.df$H
# Calculate goodness-of-fit statistics
# R2
(R2 <- summary(lm(pred ~ obs))$r.squared)
# root mean squared error (RMSE)
(RMSE <- (mean((pred-obs)^2))^0.5)
# bias (mean error)
(Bias <- mean(pred-obs))

# add to the plot
# Write them into the graphic
text(x=0, y=35, paste0("R² = ", round(R2, 2)), adj=c(0, 0))
text(x=0, y=32, paste0("RMSE = ", round(RMSE, 1)), adj=c(0, 0))
text(x=0, y=29, paste0("Bias = ", round(Bias, 1)), adj=c(0, 0))
